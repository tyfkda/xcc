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
#include <unistd.h>  // getcwd

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
  // Set lexer for preprocess.
  init_lexer_for_preprocessor();

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

int compile_csource(const char *src, enum OutType out_type, const char *ofn, Vector *ld_cmd, const char *import_module_name) {
  FILE *ppout = tmpfile();
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
    char template[] = "/tmp/xcc-XXXXXX.o";
    int obj_fd = mkstemps(template, 2);
    if (obj_fd == -1) {
      perror("Failed to open output file");
      exit(1);
    }
    char *tmpfn = strdup(template);
    ofp = fdopen(obj_fd, "wb");
    outfn = tmpfn;
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

int main(int argc, char *argv[]) {
  const char *root = dirname(strdup(argv[0]));
  if (!is_fullpath(root)) {
    char *cwd = getcwd(NULL, 0);
    root = JOIN_PATHS(cwd, root);
  }

  const char *ofn = NULL;
  const char *import_module_name = DEFAULT_IMPORT_MODULE_NAME;
  Vector *exports = new_vector();
  uint32_t stack_size = DEFAULT_STACK_SIZE;
  const char *entry_point = NULL;
  bool nodefaultlibs = false, nostdlib = false, nostdinc = false;
  Vector *lib_paths = new_vector();
  enum OutType out_type = OutExecutable;

  Vector *obj_files = new_vector();

  enum SourceType {
    UnknownSource,
    // Assembly,
    Clanguage,
    ObjectFile,
    ArchiveFile,
  };
  enum SourceType src_type = UnknownSource;

  enum {
    OPT_VERBOSE = 256,
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
  static const struct option options[] = {
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

    // Suppress warnings
    {"O", required_argument, OPT_OPTIMIZE},
    {"g", required_argument, OPT_DEBUGINFO},
    {"ansi", no_argument, OPT_ANSI},
    {"std", required_argument, OPT_STD},
    {"pedantic", no_argument, OPT_PEDANTIC},
    {"MMD", no_argument, OPT_MMD},

    {NULL},
  };
  Vector *sources = new_vector();
  for (;;) {
    int opt = optparse(argc, argv, options);
    if (opt == -1) {
      if (optind >= argc)
        break;

      vec_push(sources, argv[optind++]);
      continue;
    }

    switch (opt) {
    default: assert(false); break;
    case 'o':
      ofn = optarg;
      break;
    case 'c':
      out_type = OutObject;
      break;
    case 'e':
      if (*optarg != '\0') {
        const char *s = optarg;
        for (;;) {
          const char *p = strchr(s, ',');
          const Name *name = alloc_name(s, p, false);
          vec_push(exports, name);
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
      vec_push(lib_paths, optarg);
      break;
    case 'x':
      if (strcmp(optarg, "c") == 0) {
        src_type = Clanguage;
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
      nodefaultlibs = true;
      break;
    case OPT_NOSTDLIB:
      nostdlib = true;
      break;
    case OPT_NOSTDINC:
      nostdinc = true;
      break;
    case OPT_STACK_SIZE:
      {
        int size = atoi(optarg);
        if (size <= 0) {
          error("stack-size must be positive");
        }
        stack_size = size;
      }
      break;
    case OPT_IMPORT_MODULE_NAME:
      import_module_name = optarg;
      break;
    case OPT_VERBOSE:
      verbose = true;
      break;
    case OPT_ENTRY_POINT:
      entry_point = optarg;
      break;
    case '?':
      if (strcmp(argv[optind - 1], "-") == 0) {
        if (src_type == UnknownSource) {
          error("-x required");
        }
        vec_push(sources, NULL);
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

  if (sources->len == 0) {
    fprintf(stderr, "No input files\n\n");
    // usage(stderr);
    return 1;
  }

  if (!nostdinc) {
#if defined(__WASM)
    add_inc_path(INC_AFTER, "/usr/include");
#else
    add_inc_path(INC_AFTER, JOIN_PATHS(root, "include"));
#endif
  }

  if (out_type >= OutExecutable && entry_point == NULL) {
    entry_point = "_start";
  }
  if (entry_point != NULL && *entry_point != '\0')
    vec_push(exports, alloc_name(entry_point, NULL, false));
  if (exports->len == 0 && out_type >= OutExecutable) {
    error("no exports (require -e<xxx>)\n");
  }

  VERBOSES("### Exports\n");
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    VERBOSE("%.*s\n", NAMES(name));
  }
  VERBOSES("\n");

  if (out_type >= OutExecutable) {
#if defined(__WASM)
    vec_push(lib_paths, "/usr/lib");
#else
    vec_push(lib_paths, JOIN_PATHS(root, "./lib"));
#endif
  }

  for (int i = 0; i < sources->len; ++i) {
    char *src = sources->data[i];
    const char *outfn = ofn;
    if (src != NULL) {
      if (*src == '\0')
        continue;
      if (*src == '-') {
        assert(src[1] == 'l');
        vec_push(obj_files, src);
        continue;
      }

      if (outfn == NULL) {
        if (out_type == OutObject)
          outfn = change_ext(basename(src), "o");
      }
    }

    enum SourceType st = src_type;
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
        int res = compile_csource(src, out_type, outfn, obj_files, import_module_name);
        if (res != 0)
          return 1;  // exit
      }
      break;
    case ObjectFile:
    case ArchiveFile:
      if (out_type >= OutExecutable)
        vec_push(obj_files, src);
      break;
    }
  }

  if (out_type >= OutExecutable) {
#if USE_EMCC_AS_LINKER || USE_WCCLD_AS_LINKER
    UNUSED(nodefaultlibs);
    UNUSED(nostdlib);
    UNUSED(add_lib);

    char *finalfn = (char*)ofn;
    if (finalfn == NULL) {
      finalfn = "a.wasm";
    } else {
      finalfn = change_ext(finalfn, "wasm");
    }

#if USE_EMCC_AS_LINKER
    char *cc = "emcc";
#else  // if USE_WCCLD_AS_LINKER
    char *cc = JOIN_PATHS(root, "wcc-ld");
#endif

    char *argv[] = {
      cc,
      "-o", finalfn,
      tmpfn,
#if USE_WCCLD_AS_LINKER
      JOIN_PATHS(root, "lib/wcrt0.a"),
      JOIN_PATHS(root, "lib/wlibc.a"),
#endif
      NULL,
    };
    execvp(cc, argv);
    perror(cc);
    return 1;
#else
    if (!nostdlib)
      add_lib(lib_paths, "wcrt0.a", obj_files);
    if (!nodefaultlibs && !nostdlib)
      add_lib(lib_paths, "wlibc.a", obj_files);

    const char *outfn = ofn != NULL ? ofn : "a.wasm";

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

    if (!link_wasm_objs(linker, exports, stack_size) ||
        !linker_emit_wasm(linker, outfn, exports))
      return 2;
#endif
  }

  return 0;
}
