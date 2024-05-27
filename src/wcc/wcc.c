#include "../config.h"
#include "wcc.h"

#include <assert.h>
#include <libgen.h>  // dirname
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>  // exit, free
#include <string.h>  // strdup
#include <strings.h>  // strcasecmp
#include <sys/stat.h>

#include "fe_misc.h"
#include "lexer.h"
#include "parser.h"
#include "preprocessor.h"
#include "table.h"
#include "util.h"
#include "var.h"
#include "wasm.h"
#include "wasm_linker.h"

// #define USE_EMCC_AS_LINKER  1
// #define USE_WCCLD_AS_LINKER  1

static const char DEFAULT_IMPORT_MODULE_NAME[] = "env";

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
      "  --entry-point=<name>  Specify entry point (Defulat: _start)\n"
      "  --stack-size=<size>   Output object file (Default: 8192)\n"
  );
}

static void remove_tmp_files(void) {
  for (int i = 0; i < remove_on_exit.len; ++i) {
    const char *fn = remove_on_exit.data[i];
    remove(fn);
  }
}

static void init_compiler(void) {
  table_init(&func_info_table);
  table_init(&gvar_info_table);
  functypes = new_vector();
  tags = new_vector();
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

static void compile1(FILE *ifp, const char *filename, Vector *decls) {
  set_source_file(ifp, filename);
  parse(decls);
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

static void preprocess_and_compile(FILE *ppout, const char *filename, Vector *toplevel) {
  // Preprocess.
  FILE *ifp;
  if (filename != NULL) {
    if (!is_file(filename) || (ifp = fopen(filename, "r")) == NULL)
      error("Cannot open file: %s\n", filename);
  } else {
    ifp = stdin;
    filename = "*stdin*";
  }
  fprintf(ppout, "# 1 \"%s\" 1\n", filename);
  preprocess(ifp, filename);
  if (ifp != stdin)
    fclose(ifp);
  if (fseek(ppout, 0, SEEK_SET) != 0) {
    error("fseek failed");
  }

  // Set lexer for compiler.
  init_lexer();

  // Compile.
  FILE *ppin = ppout;
  compile1(ppin, "*", toplevel);
  if (compile_error_count != 0)
    exit(1);
}

int compile_csource(const char *src, enum OutType out_type, const char *ofn, Vector *ld_cmd,
                    const char *import_module_name) {
  FILE *ppout = platform_mktempfile();
  if (ppout == NULL)
    error("cannot open temporary file");

  init_preprocessor(ppout);
  define_macro("__ILP32__");
  define_macro("__WASM");
#if defined(__NO_FLONUM)
  define_macro("__NO_FLONUM");
#endif
#if defined(__NO_BITFIELD)
  define_macro("__NO_BITFIELD");
#endif
#if defined(__NO_VLA)
  define_macro("__NO_VLA");
  define_macro("__STDC_NO_VLA__");
#endif
  define_macro("__SIZEOF_POINTER__=4");
  define_macro("__SIZEOF_INT__=4");
  define_macro("__SIZEOF_LONG__=4");
  define_macro("__SIZEOF_LONG_LONG__=8");
  define_macro("__SIZEOF_SIZE_T__=4");

  init_compiler();

  Vector *toplevel = new_vector();

  preprocess_and_compile(ppout, src, toplevel);

  fclose(ppout);

  traverse_ast(toplevel);
  if (compile_error_count != 0)
    return 1;

  gen(toplevel);

  if (compile_error_count != 0)
    return 1;
  if (error_warning && compile_warning_count != 0)
    return 2;

  FILE *ofp;
  const char *outfn = NULL;
  if (out_type >= OutExecutable) {
    char* tmpfn = NULL;
    ofp = platform_mktempfile2(".o", &tmpfn, "wb");
    if (ofp == NULL) {
      perror("Failed to open output file");
      exit(1);
    }
    outfn = tmpfn;
    vec_push(&remove_on_exit, tmpfn);
  } else {
    outfn = ofn;
    if (outfn == NULL) {
      // char *src = sources->data[0];  // TODO:
      outfn = src != NULL ? change_ext(basename((char*)src), "o") : "a.o";
    }
    ofp = fopen(outfn, "wb");
  }
  if (ofp == NULL) {
    error("Cannot open output file");
  } else {
    emit_wasm(ofp, import_module_name);
    assert(compile_error_count == 0);
    fclose(ofp);
  }

  vec_push(ld_cmd, outfn);
  return 0;
}

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
  Vector *sources;
  const char *root;
  const char *ofn;
  const char *import_module_name;
  const char *entry_point;
  enum OutType out_type;
  enum SourceType src_type;
  uint32_t stack_size;
  bool nodefaultlibs, nostdlib, nostdinc;
} Options;

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

    OPT_WARNING,
    OPT_OPTIMIZE,
    OPT_DEBUGINFO,
    OPT_ANSI,
    OPT_STD,
    OPT_PEDANTIC,
    OPT_MMD,
  };
  static const struct option kOptions[] = {
    {"c", no_argument},  // Output .o
    {"I", required_argument},  // Add include path
    {"isystem", required_argument, OPT_ISYSTEM},  // Add system include path
    {"idirafter", required_argument, OPT_IDIRAFTER},  // Add include path (after)
    {"L", required_argument},  // Add library path
    {"D", required_argument},  // Define macro
    {"o", required_argument},  // Specify output filename
    {"x", required_argument},  // Specify code type
    {"e", required_argument},  // Export names
    {"W", required_argument, OPT_WARNING},
    {"nodefaultlibs", no_argument, OPT_NODEFAULTLIBS},
    {"nostdlib", no_argument, OPT_NOSTDLIB},
    {"nostdinc", no_argument, OPT_NOSTDINC},
    {"-import-module-name", required_argument, OPT_IMPORT_MODULE_NAME},
    {"-verbose", no_argument, OPT_VERBOSE},
    {"-entry-point", required_argument, OPT_ENTRY_POINT},
    {"-stack-size", required_argument, OPT_STACK_SIZE},
    {"-help", no_argument, OPT_HELP},
    {"-version", no_argument, OPT_VERSION},
    {"dumpversion", no_argument, OPT_DUMP_VERSION},

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
      show_version("wcc");
      exit(0);
    case OPT_DUMP_VERSION:
      show_version(NULL);
      exit(0);
    case 'o':
      opts->ofn = optarg;
      break;
    case 'c':
      opts->out_type = OutObject;
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
      define_macro(optarg);
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
    case OPT_WARNING:
      if (strcmp(optarg, "error") == 0) {
        error_warning = true;
      } else {
        // Silently ignored.
        // fprintf(stderr, "Warning: unknown option for -W: %s\n", optarg);
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
        int size = atoi(optarg);
        if (size <= 0) {
          error("stack-size must be positive");
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
#if USE_EMCC_AS_LINKER || USE_WCCLD_AS_LINKER
  UNUSED(add_lib);

  char *finalfn = (char*)opts->ofn;
  if (finalfn == NULL) {
    finalfn = "a.wasm";
  } else {
    finalfn = change_ext(finalfn, "wasm");
  }

#if USE_EMCC_AS_LINKER
  char *cc = "emcc";
#else  // if USE_WCCLD_AS_LINKER
  const char *root = opts->root;
  char *cc = JOIN_PATHS(root, "wcc-ld");
#endif

  vec_insert(obj_files, 0, cc);
  vec_insert(obj_files, 1, "-o");
  vec_insert(obj_files, 2, finalfn);
#if USE_WCCLD_AS_LINKER
  vec_push(obj_files, JOIN_PATHS(root, "lib/wcrt0.a"));
  vec_push(obj_files, JOIN_PATHS(root, "lib/wlibc.a"));
#endif
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
  table_init(&indirect_function_table);

  WasmLinker linker_body;
  WasmLinker *linker = &linker_body;
  linker_init(linker);

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

  for (int i = 0; i < opts->sources->len; ++i) {
    char *src = opts->sources->data[i];
    const char *outfn = opts->ofn;
    if (src != NULL) {
      if (*src == '\0')
        continue;
      if (*src == '-') {
        assert(src[1] == 'l');
        vec_push(obj_files, src);
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
        int res = compile_csource(src, opts->out_type, outfn, obj_files, opts->import_module_name);
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

  if (opts->out_type < OutExecutable)
    return 0;
  return do_link(obj_files, opts);
}

int main(int argc, char *argv[]) {
  const char *root = dirname(strdup(argv[0]));
  if (!is_fullpath(root)) {
    char *cwd = platform_getcwd(NULL, 0);
    root = JOIN_PATHS(cwd, root);
  }

  Options opts = {
    .exports = new_vector(),
    .lib_paths = new_vector(),
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
#if defined(__WASM)
    add_inc_path(INC_AFTER, "/usr/include");
#else
    add_inc_path(INC_AFTER, JOIN_PATHS(root, "include"));
#endif
  }

  if (opts.out_type >= OutExecutable && opts.entry_point == NULL) {
    opts.entry_point = "_start";
  }
  if (opts.entry_point != NULL && *opts.entry_point != '\0')
    vec_push(opts.exports, alloc_name(opts.entry_point, NULL, false));
  if (opts.exports->len == 0 && opts.out_type >= OutExecutable) {
    error("no exports (require -e<xxx>)\n");
  }

  VERBOSES("### Exports\n");
  for (int i = 0; i < opts.exports->len; ++i) {
    const Name *name = opts.exports->data[i];
    VERBOSE("%.*s\n", NAMES(name));
  }
  VERBOSES("\n");

  if (opts.out_type >= OutExecutable) {
#if defined(__WASM)
    vec_push(opts.lib_paths, "/usr/lib");
#else
    vec_push(opts.lib_paths, JOIN_PATHS(root, "./lib"));
#endif
  }

  atexit(remove_tmp_files);

  return do_compile(&opts);
}
