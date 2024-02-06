#include "../config.h"
#include "wcc.h"

#include <assert.h>
#include <libgen.h>  // dirname
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>  // exit, free
#include <string.h>  // strdup
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

// #define USE_EMCC_AS_LINKER  1

static const char DEFAULT_IMPORT_MODULE_NAME[] = "wasi_snapshot_preview1";

#define DEFAULT_STACK_SIZE  (8 * 1024)

static void init_compiler(void) {
  table_init(&func_info_table);
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

static void export_non_static_functions(Vector *exports) {
  Table table;
  table_init(&table);
  for (int i = 0; i < exports->len; ++i)
    table_put(&table, exports->data[i], exports->data[i]);

  Vector *gvars = global_scope->vars;
  for (int i = 0; i < gvars->len; ++i) {
    VarInfo *vi = gvars->data[i];
    const Name *name = vi->name;
    if (vi->type->kind != TY_FUNC ||
        vi->global.func == NULL || (vi->storage & VS_STATIC) ||
        table_try_get(&builtin_function_table, vi->name, NULL) ||
        table_try_get(&table, name, NULL))
      continue;
    vec_push(exports, name);
  }
}

static void preprocess_and_compile(FILE *ppout, Vector *sources, Vector *toplevel) {
  size_t pos = ftell(ppout);

  // Set lexer for preprocess.
  init_lexer_for_preprocessor();

  // Preprocess.
  for (int i = 0; i < sources->len; ++i) {
    const char *filename = sources->data[i];
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
  }
  if (fseek(ppout, pos, SEEK_SET) != 0) {
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
  bool export_all = false;
  bool export_stack_pointer = false;
  out_type = OutExecutable;

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

  enum SourceType {
    UnknownSource,
    // Assembly,
    Clanguage,
    // ObjectFile,
    // ArchiveFile,
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
    OPT_EXPORT_ALL_NON_STATIC,
    OPT_EXPORT_STACK_POINTER,

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
    {"-export-all-non-static", no_argument, OPT_EXPORT_ALL_NON_STATIC},
    {"-export-stack-pointer", no_argument, OPT_EXPORT_STACK_POINTER},

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
  int opt;
  while ((opt = optparse(argc, argv, options)) != -1) {
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
      // } else if (strcmp(optarg, "assembler") == 0) {
      //   src_type = Assembly;
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
    case OPT_EXPORT_ALL_NON_STATIC:
      export_all = true;
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
    case OPT_EXPORT_STACK_POINTER:
      export_stack_pointer = true;
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

  int iarg = optind;
  for (int i = iarg; i < argc; ++i)
    vec_push(sources, argv[i]);

  if (sources->len == 0) {
    fprintf(stderr, "No input files\n\n");
    // usage(stderr);
    return 1;
  }

  if (!nostdinc) {
#if defined(__WASM)
    add_inc_path(INC_AFTER, "/usr/include");
    add_inc_path(INC_AFTER, "/usr/local/include");
#else
    add_inc_path(INC_AFTER, JOIN_PATHS(root, "include"));
    add_inc_path(INC_AFTER, JOIN_PATHS(root, "libsrc"));
#endif
  }

#if USE_EMCC_AS_LINKER
  bool do_emcc_link = false;
  if (out_type >= OutExecutable) {
    do_emcc_link = true;
    out_type = OutObject;
  }
  if (import_module_name == DEFAULT_IMPORT_MODULE_NAME)
    import_module_name = "env";
#endif
  if (out_type >= OutExecutable && entry_point == NULL)
    entry_point = "_start";
  if (entry_point != NULL && *entry_point != '\0')
    vec_push(exports, alloc_name(entry_point, NULL, false));
  if (exports->len == 0 && !(export_all || out_type < OutExecutable)) {
    error("no exports (require -e<xxx>)\n");
  }

  VERBOSES("### Exports\n");
  for (int i = 0; i < exports->len; ++i) {
    const Name *name = exports->data[i];
    VERBOSE("%.*s\n", NAMES(name));
  }
  VERBOSES("\n");

  Vector *libs = new_vector();
  if (out_type >= OutExecutable) {
#if defined(__WASM)
    vec_push(lib_paths, "/usr/lib");
#else
    vec_push(lib_paths, JOIN_PATHS(root, "./libsrc/_wasm"));
#endif
    if (!nostdlib)
      add_lib(lib_paths, "crt0.c", libs);
    if (!nodefaultlibs && !nostdlib)
      add_lib(lib_paths, "libc.c", libs);
  }

  Vector *toplevel = new_vector();

  preprocess_and_compile(ppout, sources, toplevel);
  if (export_all || out_type < OutExecutable)
    export_non_static_functions(exports);
  preprocess_and_compile(ppout, libs, toplevel);

  fclose(ppout);

  uint32_t address_bottom = traverse_ast(toplevel, exports, stack_size);
  if (compile_error_count != 0)
    return 1;

  if (export_stack_pointer) {
    GVarInfo *info = get_gvar_info_from_name(alloc_name(SP_NAME, NULL, false));
    info->flag |= GVF_EXPORT;
  }

  gen(toplevel);
  if (out_type >= OutExecutable && unresolved_gvar_table.count > 0) {
    const Name *name;
    VarInfo *varinfo;
    for (int it = 0;
         (it = table_iterate(&unresolved_gvar_table, it, &name, (void**)&varinfo)) != -1; ) {
      fprintf(stderr, "Global variable not resolved: %.*s\n", NAMES(name));
    }
    ++compile_error_count;
  }

  if (compile_error_count != 0)
    return 1;
  if (error_warning && compile_warning_count != 0)
    return 2;

  FILE *ofp;
#if USE_EMCC_AS_LINKER
  char *tmpfn;
  if (do_emcc_link) {
    char template[] = "/tmp/xcc-XXXXXX.o";
    int obj_fd = mkstemps(template, 2);
    if (obj_fd == -1) {
      perror("Failed to open output file");
      exit(1);
    }
    tmpfn = strdup(template);
    ofp = fdopen(obj_fd, "wb");
  } else
#endif
  {
    const char *outfn = ofn;
    if (outfn == NULL) {
      if (out_type == OutObject) {
        char *src = sources->data[0];  // TODO:
        outfn = change_ext(basename(src), "o");
      } else {
        outfn = "a.wasm";
      }
    }
    ofp = fopen(outfn, "wb");
  }
  if (ofp == NULL) {
    error("Cannot open output file");
  } else {
    emit_wasm(ofp, exports, import_module_name, address_bottom);
    assert(compile_error_count == 0);
    fclose(ofp);
  }

#if USE_EMCC_AS_LINKER
  if (do_emcc_link) {
    char *finalfn = (char*)ofn;
    if (finalfn == NULL) {
      finalfn = "a.wasm";
    } else {
      finalfn = change_ext(finalfn, "wasm");
    }

    char *argv[] = {
      "emcc",
      "-o", finalfn,
      tmpfn,
      NULL,
    };
    return execvp("emcc", argv);
  }
#endif

  return 0;
}
