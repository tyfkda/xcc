#include "../config.h"
#include "wcc.h"

#include <assert.h>
#include <libgen.h>  // dirname
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>  // exit, free, strtoul
#include <string.h>  // strdup
#include <strings.h>  // strcasecmp
#include <sys/stat.h>
#include <unistd.h>  // getcwd

#include "fe_misc.h"
#include "lexer.h"
#include "parser.h"
#include "preprocessor.h"
#include "table.h"
#include "util.h"
#include "var.h"
#include "wasm_linker.h"

// #define USE_EMCC_AS_LINKER  1

static const char DEFAULT_IMPORT_MODULE_NAME[] = "env";

enum SourceType {
  UnknownSource,
  // Assembly,
  Clanguage,
  ObjectFile,
  ArchiveFile,
};

typedef struct {
  Vector *exports;
  Vector *lib_paths;
  Vector *defines;
  Vector *sources;
  const char *root;
  const char *ofn;
  const char *import_module_name;
  const char *entry_point;
  enum OutType out_type;
  enum SourceType src_type;
  uint32_t stack_size;
  bool nodefaultlibs, nostdlib, nostdinc;

  WasmLinkerOptions linker_opts;
} Options;

static Vector remove_on_exit;

static void usage(FILE *fp) {
  fprintf(
      fp,
      "Usage: wcc [options] file...\n"
      "Options:\n"
      "  -I <path>             Add include path\n"
      "  -D <label[=value]>    Define label\n"
      "  -o <filename>         Set output filename (Default: a.wasm)\n"
      "  -c                    Output object file\n"
      "  --entry-point=<name>  Specify entry point (Default: _start)\n"
      "  --stack-size=<size>   Output object file (Default: 8192)\n"
  );
}

static void remove_tmp_files(void) {
  for (int i = 0; i < remove_on_exit.len; ++i) {
    const char *fn = remove_on_exit.data[i];
    remove(fn);
  }
}

static bool add_lib(Vector *lib_paths, const char *fn, Vector *sources) {
  for (int i = 0; i < lib_paths->len; ++i) {
    char *path = JOIN_PATHS(lib_paths->data[i], fn);
    if (is_file(path)) {
      vec_push(sources, path);
      return true;
    }
    free(path);
  }
  return false;
}

// search 'wlibXXX.a' from library paths.
static const char *search_library(Vector *lib_paths, const char *libname) {
  char libfn[128];  // TODO: Avoid overflow.
  snprintf(libfn, sizeof(libfn), "wlib%s.a", libname);

  for (int i = 0; i < lib_paths->len; ++i) {
    const char *dir = lib_paths->data[i];
    const char *path = JOIN_PATHS(dir, libfn);
    if (is_file(path)) {
      return path;
    }
  }
  return NULL;
}

static void init_compiler(void) {
  table_init(&func_info_table);
  table_init(&gvar_info_table);
  functypes = new_vector();
  tags = new_vector();
  tables = new_vector();
  table_init(&indirect_function_table);
  init_global();

  set_fixnum_size(FX_CHAR,  1, 1);
  set_fixnum_size(FX_SHORT, 2, 2);
  set_fixnum_size(FX_INT,   4, 4);
  set_fixnum_size(FX_LONG,  4, 4);
  set_fixnum_size(FX_LLONG, 8, 8);
  set_fixnum_size(FX_ENUM,  4, 4);

  install_builtins();
}

static void do_preprocess(const char *filename) {
  // Preprocess.
  FILE *ifp;
  if (strcmp(filename, "-") != 0) {
    if (!is_file(filename) || (ifp = fopen(filename, "r")) == NULL)
      error("Cannot open file: %s\n", filename);
  } else {
    ifp = stdin;
    filename = "*stdin*";
  }
  preprocess(ifp, filename);
  if (ifp != stdin)
    fclose(ifp);
}

static void compilec(FILE *ppin, const char *filename, Vector *toplevel) {
  // Set lexer for compiler.
  init_lexer();

  // Compile.
  set_source_file(ppin, filename);
  parse(toplevel);
  if (compile_error_count != 0)
    exit(1);
}

int compile_csource(const char *src, const char *ofn, Vector *obj_files, Options *opts) {
  FILE *ppout;
  enum OutType out_type = opts->out_type;
  if (out_type == OutPreprocess)
    ppout = ofn != NULL ? fopen(ofn, "w") : stdout;
  else
    ppout = tmpfile();
  if (ppout == NULL)
    error("cannot open temporary file");

  init_preprocessor(ppout);

  for (int i = 0; i < opts->defines->len; ++i) {
    const char *def = opts->defines->data[i];
    define_macro(def);
  }

  do_preprocess(src);
  if (out_type == OutPreprocess) {
    if (ppout != stdout)
      fclose(ppout);
    return 0;
  }

  if (fseek(ppout, 0, SEEK_SET) != 0) {
    error("fseek failed");
  }

  init_compiler();
  Vector *toplevel = new_vector();
  compilec(ppout, src, toplevel);

  fclose(ppout);

  traverse_ast(toplevel);
  if (compile_error_count != 0)
    return 1;

  gen(toplevel);

  if (compile_error_count != 0)
    return 1;
  if (cc_flags.warn_as_error && compile_warning_count != 0)
    return 2;

  Vector *exports = opts->out_type < OutExecutable ? opts->exports : NULL;
  if (exports != NULL) {
    int undef = 0;
    for (int i = 0; i < exports->len; ++i) {
      const Name *name = exports->data[i];
      if (!table_try_get(&func_info_table, name, NULL)) {
        fprintf(stderr, "Export: `%.*s' not defined\n", NAMES(name));
        ++undef;
      }
    }
    if (undef > 0)
      return 3;
  }

  FILE *ofp;
  const char *outfn = NULL;
  if (out_type >= OutExecutable) {
    char template[] = "/tmp/xcc-XXXXXX.o";
    int obj_fd = mkstemps(template, 2);
    if (obj_fd == -1) {
      perror("Failed to open output file");
      exit(1);
    }
    char *tmpfn = strdup(template);
    ofp = fdopen(obj_fd, "wb");
    outfn = tmpfn;
    vec_push(&remove_on_exit, tmpfn);
  } else {
    outfn = ofn;
    if (outfn == NULL)
      outfn = src != NULL ? change_ext(basename((char*)src), "o") : "a.o";
    ofp = fopen(outfn, "wb");
  }
  if (ofp == NULL) {
    error("Cannot open output file");
  } else {
    emit_wasm(ofp, opts->import_module_name, exports);
    assert(compile_error_count == 0);
    fclose(ofp);
  }

  vec_push(obj_files, outfn);
  return 0;
}

static void parse_linker_options(const char *arg, WasmLinkerOptions *lopts) {
  const char *arg_bak = arg;
  if (*arg != '-')
    return;
  ++arg;

  enum {
    OPT_ALLOW_UNDEFINED,
  };
  static const struct option kOptions[] = {
    {"-allow-undefined", no_argument, OPT_ALLOW_UNDEFINED},

    {NULL},
  };

  for (int i = 0; ; ++i) {
    if (kOptions[i].name == NULL)
      break;
    if (strcmp(arg, kOptions[i].name) == 0) {
      switch (kOptions[i].val) {
      default: assert(false); break;
      case OPT_ALLOW_UNDEFINED:
        lopts->allow_undefined = true;
        break;
      }
      return;
    }
  }
  fprintf(stderr, "Warning: unknown option: %s\n", arg_bak);
}


static void parse_options(int argc, char *argv[], Options *opts) {
  enum {
    OPT_HELP = 128,
    OPT_VERSION,
    OPT_DUMP_VERSION,
    OPT_VERBOSE,
    OPT_ENTRY_POINT,
    OPT_STACK_SIZE,
    OPT_IMPORT_MODULE_NAME,
    OPT_NODEFAULTLIBS,
    OPT_NOSTDLIB,
    OPT_NOSTDINC,
    OPT_ISYSTEM,
    OPT_IDIRAFTER,
    OPT_FNO,
    OPT_WNO,

    OPT_OPTIMIZE,
    OPT_DEBUGINFO,
    OPT_ANSI,
    OPT_STD,
    OPT_PEDANTIC,
    OPT_MMD,
  };
  static const struct option kOptions[] = {
    {"c", no_argument},  // Output .o
    {"E", no_argument},  // Output preprocess result
    {"I", required_argument},  // Add include path
    {"isystem", required_argument, OPT_ISYSTEM},  // Add system include path
    {"idirafter", required_argument, OPT_IDIRAFTER},  // Add include path (after)
    {"l", required_argument},  // Library
    {"L", required_argument},  // Add library path
    {"D", required_argument},  // Define macro
    {"U", required_argument},  // Undefine macro
    {"C", no_argument},  // Do not discard comments
    {"o", required_argument},  // Specify output filename
    {"x", required_argument},  // Specify code type
    {"e", required_argument},  // Export names
    {"nodefaultlibs", no_argument, OPT_NODEFAULTLIBS},
    {"nostdlib", no_argument, OPT_NOSTDLIB},
    {"nostdinc", no_argument, OPT_NOSTDINC},
    {"-import-module-name", required_argument, OPT_IMPORT_MODULE_NAME},
    {"-verbose", no_argument, OPT_VERBOSE},
    {"-entry-point", required_argument, OPT_ENTRY_POINT},
    {"-stack-size", required_argument, OPT_STACK_SIZE},
    {"-help", no_argument, OPT_HELP},
    {"v", no_argument, OPT_VERSION},
    {"-version", no_argument, OPT_VERSION},
    {"dumpversion", no_argument, OPT_DUMP_VERSION},

    // Sub command
    {"fno-", optional_argument, OPT_FNO},
    {"f", optional_argument},
    {"Wno-", optional_argument, OPT_WNO},
    {"W", optional_argument},

    // Suppress warnings
    {"O", required_argument, OPT_OPTIMIZE},
    {"g", required_argument, OPT_DEBUGINFO},
    {"ansi", no_argument, OPT_ANSI},
    {"std", required_argument, OPT_STD},
    {"pedantic", no_argument, OPT_PEDANTIC},
    {"MMD", no_argument, OPT_MMD},

    {NULL},
  };
  for (;;) {
    int opt = optparse(argc, argv, kOptions);
    if (opt == -1) {
      if (optind >= argc)
        break;

      vec_push(opts->sources, argv[optind++]);
      continue;
    }

    switch (opt) {
    default: assert(false); break;
    case OPT_HELP:
      usage(stdout);
      exit(0);
    case OPT_VERSION:
      show_version("wcc", XCC_ARCH_WASM);
      exit(0);
    case OPT_DUMP_VERSION:
      show_version(NULL, -1);
      exit(0);
    case 'o':
      opts->ofn = optarg;
      break;
    case 'c':
      opts->out_type = OutObject;
      break;
    case 'E':
      opts->out_type = OutPreprocess;
      if (opts->src_type == UnknownSource)
        opts->src_type = Clanguage;
      break;
    case 'e':
      if (*optarg != '\0') {
        const char *s = optarg;
        for (;;) {
          const char *p = strchr(s, ',');
          const Name *name = alloc_name(s, p, false);
          vec_push(opts->exports, name);
          if (p == NULL)
            break;
          s = p + 1;
        }
      }
      break;
    case 'I':
      add_inc_path(INC_NORMAL, optarg);
      break;
    case OPT_ISYSTEM:
      add_inc_path(INC_SYSTEM, optarg);
      break;
    case OPT_IDIRAFTER:
      add_inc_path(INC_AFTER, optarg);
      break;
    case 'D':
      vec_push(opts->defines, optarg);
      break;
    case 'U':
      {
        Vector *defines = opts->defines;
        for (int i = 0; i < defines->len; ++i) {
          const char *str = defines->data[i];
          char *end = strchr(str, '=');
          size_t len = end != NULL ? (size_t)(end - str) : strlen(str);
          if (strncmp(str, optarg, len) == 0 && optarg[len] == '\0') {
            vec_remove_at(defines, i);
            break;
          }
        }
      }
      break;
    case 'C':
      set_preserve_comment(true);
      break;
    case 'l':
      {
        char *p;
        if (strncmp(argv[optind - 1], "-l", 2) == 0) {
          // -lfoobar
          // file order matters, so add to sources.
          p = argv[optind - 1];
        } else {
          StringBuffer sb;
          sb_init(&sb);
          sb_append(&sb, "-l", optarg);
          sb_append(&sb, optarg, NULL);
          p = sb_to_string(&sb);
        }
        vec_push(opts->sources, p);
      }
      break;
    case 'L':
      vec_push(opts->lib_paths, optarg);
      break;
    case 'x':
      if (strcmp(optarg, "c") == 0) {
        opts->src_type = Clanguage;
      } else {
        error("language not recognized: %s", optarg);
      }
      break;
    case OPT_NODEFAULTLIBS:
      opts->nodefaultlibs = true;
      break;
    case OPT_NOSTDLIB:
      opts->nostdlib = true;
      break;
    case OPT_NOSTDINC:
      opts->nostdinc = true;
      break;
    case OPT_STACK_SIZE:
      {
        int base = 10;
        char *p = optarg;
        if (*p == '0' && (p[1] == 'x' || p[1] == 'X')) {
          base = 16;
          p += 2;
        }
        unsigned long size = strtoul(p, &p, base);
        if (size <= 0) {
          error("stack-size must be positive: %ld", size);
        }
        if (*p != '\0') {
          error("positive integer expected: %s", optarg);
        }
        opts->stack_size = size;
      }
      break;
    case OPT_IMPORT_MODULE_NAME:
      opts->import_module_name = optarg;
      break;
    case OPT_VERBOSE:
      verbose = true;
      break;
    case OPT_ENTRY_POINT:
      opts->entry_point = optarg;
      break;
    case '?':
      if (strcmp(argv[optind - 1], "-") == 0) {
        if (opts->src_type == UnknownSource) {
          error("-x required");
        }
        vec_push(opts->sources, NULL);
      } else {
        fprintf(stderr, "Warning: unknown option: %s\n", argv[optind - 1]);
      }
      break;

    case 'f':
    case OPT_FNO:
      if (optarg == NULL) {
        fprintf(stderr, "Warning: missing argument for -f\n");
        break;
      }
      if (!parse_fopt(optarg, opt == 'f')) {
        // Silently ignored.
        // fprintf(stderr, "Warning: unknown option for -f: %s\n", optarg);
      }
      break;

    case 'W':
      if (optarg != NULL) {
        if (strcmp(optarg, "error") == 0) {
          cc_flags.warn_as_error = true;
          break;
        }
        if (strcmp(optarg, "all") == 0) {
          // Assume all members are bool.
          for (bool *p = (bool*)&cc_flags.warn; p < (bool*)(&cc_flags.warn + 1); ++p)
            *p = true;
          break;
        }

        const char *arg = argv[optind - 1];
        if (strncmp(arg, "-Wl,", 4) == 0) {
          parse_linker_options(arg + 4, &opts->linker_opts);
        }
      }
      // Fallthrough
    case OPT_WNO:
      if (optarg == NULL) {
        fprintf(stderr, "Warning: missing argument for -W\n");
        break;
      }
      if (strcmp(optarg, "error") == 0) {
        cc_flags.warn_as_error = true;
      } else if (!parse_wopt(optarg, opt == 'W')) {
        // Silently ignored.
        // fprintf(stderr, "Warning: unknown option for -W: %s\n", optarg);
      }
      break;

    case OPT_OPTIMIZE:
    case OPT_DEBUGINFO:
    case OPT_ANSI:
    case OPT_STD:
    case OPT_PEDANTIC:
    case OPT_MMD:
      // Silently ignored.
      break;
    }
  }
}

static int do_link(Vector *obj_files, Options *opts) {
#if USE_EMCC_AS_LINKER
  UNUSED(add_lib);

  char *finalfn = (char*)opts->ofn;
  if (finalfn == NULL) {
    finalfn = "a.wasm";
  } else {
    finalfn = change_ext(finalfn, "wasm");
  }

  const char *cc = "emcc";

  vec_insert(obj_files, 0, cc);
  vec_insert(obj_files, 1, "-o");
  vec_insert(obj_files, 2, finalfn);
  vec_push(obj_files, NULL);
  char **argv = (char**)obj_files->data;
  execvp(cc, argv);
  perror(cc);
  return 1;
#else
  if (!opts->nostdlib)
    add_lib(opts->lib_paths, "wcrt0.a", obj_files);
  if (!opts->nodefaultlibs && !opts->nostdlib)
    add_lib(opts->lib_paths, "wlibc.a", obj_files);

  const char *outfn = opts->ofn != NULL ? opts->ofn : "a.wasm";

  functypes = new_vector();
  tags = new_vector();
  tables = new_vector();
  table_init(&indirect_function_table);

  WasmLinker linker_body;
  WasmLinker *linker = &linker_body;
  linker_init(linker);
  linker->options = opts->linker_opts;

  for (int i = 0; i < obj_files->len; ++i) {
    const char *objfn = obj_files->data[i];
    if (!read_wasm_obj(linker, objfn)) {
      fprintf(stderr, "error: failed to read wasm object file: %s\n", objfn);
      return 1;
    }
  }

  if (!link_wasm_objs(linker, opts->exports, opts->stack_size) ||
      !linker_emit_wasm(linker, outfn, opts->exports))
    return 2;
  return 0;
#endif
}

static int do_compile(Options *opts) {
  Vector *obj_files = new_vector();

  int error_count = 0;
  for (int i = 0; i < opts->sources->len; ++i) {
    char *src = opts->sources->data[i];
    const char *outfn = opts->ofn;
    if (src != NULL) {
      if (*src == '\0')
        continue;
      if (*src == '-' && src[1] != '\0') {
        assert(src[1] == 'l');
#if USE_EMCC_AS_LINKER
        UNUSED(search_library);
        vec_push(obj_files, src);
#else
        const char *path = search_library(opts->lib_paths, optarg);
        if (path != NULL) {
          vec_push(obj_files, path);
        } else {
          fprintf(stderr, "%s: library not found\n", optarg);
          ++error_count;
        }
#endif
        continue;
      }

      if (outfn == NULL) {
        if (opts->out_type == OutObject)
          outfn = change_ext(basename(src), "o");
      }
    }

    enum SourceType st = opts->src_type;
    if (src != NULL) {
      char *ext = get_ext(src);
      if      (strcasecmp(ext, "c") == 0)  st = Clanguage;
      else if (strcasecmp(ext, "o") == 0)  st = ObjectFile;
      else if (strcasecmp(ext, "a") == 0)  st = ArchiveFile;
    }

    switch (st) {
    case UnknownSource:
      fprintf(stderr, "Unknown source type: %s\n", src);
      return 1;  // exit
    case Clanguage:
      {
        int res = compile_csource(src, outfn, obj_files, opts);
        if (res != 0)
          return 1;  // exit
      }
      break;
    case ObjectFile:
    case ArchiveFile:
      if (opts->out_type >= OutExecutable)
        vec_push(obj_files, src);
      break;
    }
  }
  if (error_count > 0)
    return 1;

  if (opts->out_type < OutExecutable)
    return 0;

  if (opts->entry_point == NULL) {
    opts->entry_point = "_start";
  }
  if (opts->entry_point != NULL && *opts->entry_point != '\0') {
    vec_push(opts->exports, alloc_name(opts->entry_point, NULL, false));
  } else if (opts->exports->len == 0) {
    error("no exports (require -e<xxx>)\n");
  }

  return do_link(obj_files, opts);
}

int main(int argc, char *argv[]) {
#if defined(__wasm)
  const char *root = "/usr";
#else
  const char *root = dirname(strdup(argv[0]));
  if (!is_fullpath(root)) {
    char *cwd = getcwd(NULL, 0);
    root = JOIN_PATHS(cwd, root);
  }
#endif

  Vector *defines = new_vector();
  static const char *kPredefinedMacros[] = {
    "__XCC",
    "__ILP32__",
    "__wasm",
    "__wasm32",
    "__STDC__",
    "__STDC_VERSION__=199901L",
    "__SIZEOF_POINTER__=4",
    "__SIZEOF_INT__=4",
    "__SIZEOF_LONG__=4",
    "__SIZEOF_LONG_LONG__=8",
    "__SIZEOF_SIZE_T__=4",
#if defined(__NO_FLONUM)
    "__NO_FLONUM",
#endif
#if defined(__NO_BITFIELD)
    "__NO_BITFIELD",
#endif
#if defined(__NO_VLA)
    "__NO_VLA",
    "__STDC_NO_VLA__",
#endif
#if defined(__NO_WCHAR)
    "__NO_WCHAR",
#endif
  };
  for (size_t i = 0; i < ARRAY_SIZE(kPredefinedMacros); ++i)
    vec_push(defines, kPredefinedMacros[i]);

  Options opts = {
    .exports = new_vector(),
    .lib_paths = new_vector(),
    .defines = defines,
    .sources = new_vector(),
    .root = root,
    .ofn = NULL,
    .import_module_name = DEFAULT_IMPORT_MODULE_NAME,
    .entry_point = NULL,
    .out_type = OutExecutable,
    .src_type = UnknownSource,
    .stack_size = DEFAULT_STACK_SIZE,
    .nodefaultlibs = false,
    .nostdlib = false,
    .nostdinc = false,
  };
  parse_options(argc, argv, &opts);

  if (opts.sources->len == 0) {
    fprintf(stderr, "No input files\n\n");
    usage(stderr);
    return 1;
  }

  if (!opts.nostdinc) {
    add_inc_path(INC_AFTER, JOIN_PATHS(root, "include"));
  }

  VERBOSES("### Exports\n");
  for (int i = 0; i < opts.exports->len; ++i) {
    const Name *name = opts.exports->data[i];
    VERBOSE("%.*s\n", NAMES(name));
  }
  VERBOSES("\n");

  if (opts.out_type >= OutExecutable) {
    vec_push(opts.lib_paths, JOIN_PATHS(root, "lib"));
  }

  atexit(remove_tmp_files);

  return do_compile(&opts);
}
