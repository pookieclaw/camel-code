/*
 * fff_stubs.c — OCaml C stubs for fff-c (Freakin Fast File Finder)
 *
 * Uses dlopen to load libfff_c at runtime. If the library is not found,
 * is_available returns false and all operations fail gracefully.
 */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <dlfcn.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>

/* ================================================================
 * fff-c type declarations (matching cbindgen output)
 * ================================================================ */

typedef struct {
    bool      success;
    char     *error;
    void     *handle;
    int64_t   int_value;
} FffResult;

typedef struct {
    char     *path;
    char     *relative_path;
    char     *file_name;
    char     *git_status;
    uint64_t  size;
    uint64_t  modified;
    int64_t   access_frecency_score;
    int64_t   modification_frecency_score;
    int64_t   total_frecency_score;
    bool      is_binary;
} FffFileItem;

typedef struct {
    int32_t   total;
    int32_t   base_score;
    int32_t   filename_bonus;
    int32_t   special_filename_bonus;
    int32_t   frecency_boost;
    int32_t   distance_penalty;
    int32_t   current_file_penalty;
    int32_t   combo_match_boost;
    bool      exact_match;
    char     *match_type;
} FffScore;

typedef struct {
    uint8_t   tag;
    int32_t   line;
    int32_t   col;
    int32_t   end_line;
    int32_t   end_col;
} FffLocation;

typedef struct {
    FffFileItem *items;
    FffScore    *scores;
    uint32_t     count;
    uint32_t     total_matched;
    uint32_t     total_files;
    FffLocation  location;
} FffSearchResult;

typedef struct {
    uint32_t  start;
    uint32_t  end;
} FffMatchRange;

typedef struct {
    char          *path;
    char          *relative_path;
    char          *file_name;
    char          *git_status;
    char          *line_content;
    FffMatchRange *match_ranges;
    char         **context_before;
    char         **context_after;
    uint64_t       size;
    uint64_t       modified;
    int64_t        total_frecency_score;
    int64_t        access_frecency_score;
    int64_t        modification_frecency_score;
    uint64_t       line_number;
    uint64_t       byte_offset;
    uint32_t       col;
    uint32_t       match_ranges_count;
    uint32_t       context_before_count;
    uint32_t       context_after_count;
    uint16_t       fuzzy_score;
    bool           has_fuzzy_score;
    bool           is_binary;
    bool           is_definition;
} FffGrepMatch;

typedef struct {
    FffGrepMatch *items;
    uint32_t      count;
    uint32_t      total_matched;
    uint32_t      total_files_searched;
    uint32_t      total_files;
    uint32_t      filtered_file_count;
    uint32_t      next_file_offset;
    char         *regex_fallback_error;
} FffGrepResult;

/* ================================================================
 * Function pointer types
 * ================================================================ */

typedef FffResult*          (*fn_create_t)(const char*, const char*, const char*, bool, bool, bool);
typedef void                (*fn_destroy_t)(void*);
typedef FffResult*          (*fn_scan_t)(void*);
typedef FffResult*          (*fn_wait_scan_t)(void*, uint64_t);
typedef FffSearchResult*    (*fn_search_t)(void*, const char*, const char*, uint32_t, uint32_t, uint32_t, int32_t, uint32_t);
typedef FffGrepResult*      (*fn_grep_t)(void*, const char*, uint8_t, uint64_t, uint32_t, bool, uint32_t, uint32_t, uint64_t, uint32_t, uint32_t, bool);
typedef FffGrepResult*      (*fn_mgrep_t)(void*, const char*, const char*, uint64_t, uint32_t, bool, uint32_t, uint32_t, uint64_t, uint32_t, uint32_t, bool);
typedef void                (*fn_free_result_t)(FffResult*);
typedef void                (*fn_free_search_t)(FffSearchResult*);
typedef void                (*fn_free_grep_t)(FffGrepResult*);

/* ================================================================
 * Global state
 * ================================================================ */

static void *lib_handle      = NULL;
static int   load_attempted  = 0;
static void *fff_instance    = NULL;

static fn_create_t      sym_create       = NULL;
static fn_destroy_t     sym_destroy      = NULL;
static fn_scan_t        sym_scan         = NULL;
static fn_wait_scan_t   sym_wait_scan    = NULL;
static fn_search_t      sym_search       = NULL;
static fn_grep_t        sym_grep         = NULL;
static fn_mgrep_t       sym_mgrep        = NULL;
static fn_free_result_t sym_free_result  = NULL;
static fn_free_search_t sym_free_search  = NULL;
static fn_free_grep_t   sym_free_grep    = NULL;

/* ================================================================
 * Dynamic string buffer
 * ================================================================ */

typedef struct { char *data; size_t len; size_t cap; } Buf;

static void buf_init(Buf *b, size_t init_cap) {
    b->cap  = init_cap;
    b->data = (char *)malloc(b->cap);
    b->len  = 0;
    b->data[0] = '\0';
}

static void buf_append(Buf *b, const char *s) {
    size_t slen = strlen(s);
    while (b->len + slen + 1 > b->cap) {
        b->cap *= 2;
        b->data = (char *)realloc(b->data, b->cap);
    }
    memcpy(b->data + b->len, s, slen);
    b->len += slen;
    b->data[b->len] = '\0';
}

static void buf_appendf(Buf *b, const char *fmt, ...) {
    char tmp[1024];
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(tmp, sizeof(tmp), fmt, ap);
    va_end(ap);
    buf_append(b, tmp);
}

static void buf_free(Buf *b) { free(b->data); }

/* ================================================================
 * Library loading
 * ================================================================ */

static int load_library(void) {
    if (load_attempted) return (lib_handle != NULL);
    load_attempted = 1;

    /* Try env override first */
    const char *custom = getenv("FFF_LIB_PATH");
    if (custom) {
        lib_handle = dlopen(custom, RTLD_LAZY);
        if (lib_handle) goto resolve;
    }

    /* Standard search paths */
    static const char *paths[] = {
        "libfff_c.so", "libfff_c.dylib",
        "/usr/local/lib/libfff_c.so",
        "/usr/local/lib/libfff_c.dylib",
        NULL
    };
    for (int i = 0; paths[i]; i++) {
        lib_handle = dlopen(paths[i], RTLD_LAZY);
        if (lib_handle) goto resolve;
    }
    return 0;

resolve:
    sym_create      = (fn_create_t)     dlsym(lib_handle, "fff_create_instance");
    sym_destroy     = (fn_destroy_t)    dlsym(lib_handle, "fff_destroy");
    sym_scan        = (fn_scan_t)       dlsym(lib_handle, "fff_scan_files");
    sym_wait_scan   = (fn_wait_scan_t)  dlsym(lib_handle, "fff_wait_for_scan");
    sym_search      = (fn_search_t)     dlsym(lib_handle, "fff_search");
    sym_grep        = (fn_grep_t)       dlsym(lib_handle, "fff_live_grep");
    sym_mgrep       = (fn_mgrep_t)      dlsym(lib_handle, "fff_multi_grep");
    sym_free_result = (fn_free_result_t)dlsym(lib_handle, "fff_free_result");
    sym_free_search = (fn_free_search_t)dlsym(lib_handle, "fff_free_search_result");
    sym_free_grep   = (fn_free_grep_t)  dlsym(lib_handle, "fff_free_grep_result");

    if (!sym_create || !sym_destroy || !sym_search || !sym_grep || !sym_free_result) {
        dlclose(lib_handle);
        lib_handle = NULL;
        return 0;
    }
    return 1;
}

/* ================================================================
 * OCaml stubs — lifecycle
 * ================================================================ */

CAMLprim value caml_fff_is_available(value unit) {
    CAMLparam1(unit);
    load_library();
    CAMLreturn(Val_bool(lib_handle != NULL));
}

CAMLprim value caml_fff_is_initialized(value unit) {
    CAMLparam1(unit);
    CAMLreturn(Val_bool(fff_instance != NULL));
}

CAMLprim value caml_fff_init(value v_base_path) {
    CAMLparam1(v_base_path);

    if (!load_library())
        caml_failwith("fff: native library not found");

    char *base = strdup(String_val(v_base_path));
    const char *home = getenv("HOME");
    char frecency[512], history[512];
    snprintf(frecency, sizeof(frecency), "%s/.camel/fff_frecency.db",
             home ? home : ".");
    snprintf(history, sizeof(history), "%s/.camel/fff_history.db",
             home ? home : ".");

    FffResult *r = sym_create(base, frecency, history, false, true, true);
    free(base);

    if (!r || !r->success) {
        char errbuf[512];
        snprintf(errbuf, sizeof(errbuf), "fff init failed: %s",
                 (r && r->error) ? r->error : "unknown error");
        if (r) sym_free_result(r);
        caml_failwith(errbuf);
    }

    fff_instance = r->handle;
    sym_free_result(r);

    /* Trigger background scan */
    if (sym_scan) {
        FffResult *sr = sym_scan(fff_instance);
        if (sr) sym_free_result(sr);
    }

    /* Wait up to 5s for initial scan */
    if (sym_wait_scan) {
        FffResult *wr = sym_wait_scan(fff_instance, 5000);
        if (wr) sym_free_result(wr);
    }

    CAMLreturn(Val_unit);
}

CAMLprim value caml_fff_destroy(value unit) {
    CAMLparam1(unit);
    if (fff_instance && sym_destroy) {
        sym_destroy(fff_instance);
        fff_instance = NULL;
    }
    CAMLreturn(Val_unit);
}

/* ================================================================
 * OCaml stubs — search (M3)
 * ================================================================ */

CAMLprim value caml_fff_search(value v_query, value v_max) {
    CAMLparam2(v_query, v_max);
    CAMLlocal1(v_result);

    if (!fff_instance)
        caml_failwith("fff: not initialized");

    char *query = strdup(String_val(v_query));
    uint32_t max_results = (uint32_t)Int_val(v_max);

    /* page_size = max_results, everything else default */
    FffSearchResult *sr = sym_search(
        fff_instance, query, NULL,
        0,             /* max_threads (0 = auto) */
        0,             /* page_index */
        max_results,   /* page_size */
        0,             /* combo_boost_multiplier */
        0              /* min_combo_count */
    );
    free(query);

    if (!sr) caml_failwith("fff: search returned null");

    Buf buf;
    buf_init(&buf, 4096);

    for (uint32_t i = 0; i < sr->count && i < max_results; i++) {
        FffFileItem *item = &sr->items[i];
        if (item->relative_path) {
            buf_append(&buf, item->relative_path);
            buf_append(&buf, "\n");
        }
    }

    if (sr->total_matched > sr->count) {
        buf_appendf(&buf, "\n(%u of %u files shown)\n",
                    sr->count, sr->total_matched);
    }

    v_result = caml_copy_string(buf.data);
    buf_free(&buf);
    if (sym_free_search) sym_free_search(sr);

    CAMLreturn(v_result);
}

/* ================================================================
 * OCaml stubs — grep (M4)
 * ================================================================ */

CAMLprim value caml_fff_grep(value v_query, value v_max, value v_before, value v_after) {
    CAMLparam4(v_query, v_max, v_before, v_after);
    CAMLlocal1(v_result);

    if (!fff_instance)
        caml_failwith("fff: not initialized");

    char *query = strdup(String_val(v_query));
    uint32_t max_matches  = (uint32_t)Int_val(v_max);
    uint32_t before_ctx   = (uint32_t)Int_val(v_before);
    uint32_t after_ctx    = (uint32_t)Int_val(v_after);

    /* mode 0 = plain text, smart_case = true, time_budget = 500ms */
    FffGrepResult *gr = sym_grep(
        fff_instance, query,
        0,             /* mode: plain text */
        (uint64_t)10 * 1024 * 1024,  /* max_file_size: 10MB */
        max_matches,   /* max_matches_per_file */
        true,          /* smart_case */
        0,             /* file_offset */
        max_matches,   /* page_limit */
        500,           /* time_budget_ms */
        before_ctx,
        after_ctx,
        true           /* classify_definitions */
    );
    free(query);

    if (!gr) caml_failwith("fff: grep returned null");

    Buf buf;
    buf_init(&buf, 8192);

    for (uint32_t i = 0; i < gr->count; i++) {
        FffGrepMatch *m = &gr->items[i];
        const char *path = m->relative_path ? m->relative_path : m->path;
        const char *content = m->line_content ? m->line_content : "";

        /* Context before */
        for (uint32_t j = 0; j < m->context_before_count && m->context_before; j++) {
            if (m->context_before[j]) {
                uint64_t ctx_line = m->line_number > m->context_before_count
                    ? m->line_number - m->context_before_count + j : j + 1;
                buf_appendf(&buf, "%s-%lu-%s\n", path, (unsigned long)ctx_line,
                           m->context_before[j]);
            }
        }

        /* Match line */
        if (m->is_definition)
            buf_appendf(&buf, "%s:%lu:%u:[def] %s\n", path,
                       (unsigned long)m->line_number, m->col, content);
        else
            buf_appendf(&buf, "%s:%lu:%u:%s\n", path,
                       (unsigned long)m->line_number, m->col, content);

        /* Context after */
        for (uint32_t j = 0; j < m->context_after_count && m->context_after; j++) {
            if (m->context_after[j]) {
                buf_appendf(&buf, "%s-%lu-%s\n", path,
                           (unsigned long)(m->line_number + 1 + j),
                           m->context_after[j]);
            }
        }
    }

    if (gr->total_matched > gr->count) {
        buf_appendf(&buf, "\n(%u of %u matches shown, %u files searched)\n",
                    gr->count, gr->total_matched, gr->total_files_searched);
    }

    v_result = caml_copy_string(buf.data);
    buf_free(&buf);
    if (sym_free_grep) sym_free_grep(gr);

    CAMLreturn(v_result);
}

/* ================================================================
 * OCaml stubs — multi-grep (M5)
 * ================================================================ */

CAMLprim value caml_fff_multi_grep(value v_patterns, value v_max, value v_before, value v_after) {
    CAMLparam4(v_patterns, v_max, v_before, v_after);
    CAMLlocal1(v_result);

    if (!fff_instance)
        caml_failwith("fff: not initialized");
    if (!sym_mgrep)
        caml_failwith("fff: multi_grep not available");

    char *patterns = strdup(String_val(v_patterns));
    uint32_t max_matches = (uint32_t)Int_val(v_max);
    uint32_t before_ctx  = (uint32_t)Int_val(v_before);
    uint32_t after_ctx   = (uint32_t)Int_val(v_after);

    FffGrepResult *gr = sym_mgrep(
        fff_instance, patterns,
        "",            /* constraints */
        (uint64_t)10 * 1024 * 1024,
        max_matches,
        true,          /* smart_case */
        0,             /* file_offset */
        max_matches,   /* page_limit */
        500,           /* time_budget_ms */
        before_ctx,
        after_ctx,
        true           /* classify_definitions */
    );
    free(patterns);

    if (!gr) caml_failwith("fff: multi_grep returned null");

    /* Same formatting as single grep */
    Buf buf;
    buf_init(&buf, 8192);

    for (uint32_t i = 0; i < gr->count; i++) {
        FffGrepMatch *m = &gr->items[i];
        const char *path = m->relative_path ? m->relative_path : m->path;
        const char *content = m->line_content ? m->line_content : "";

        for (uint32_t j = 0; j < m->context_before_count && m->context_before; j++) {
            if (m->context_before[j]) {
                uint64_t ctx_line = m->line_number > m->context_before_count
                    ? m->line_number - m->context_before_count + j : j + 1;
                buf_appendf(&buf, "%s-%lu-%s\n", path, (unsigned long)ctx_line,
                           m->context_before[j]);
            }
        }

        if (m->is_definition)
            buf_appendf(&buf, "%s:%lu:%u:[def] %s\n", path,
                       (unsigned long)m->line_number, m->col, content);
        else
            buf_appendf(&buf, "%s:%lu:%u:%s\n", path,
                       (unsigned long)m->line_number, m->col, content);

        for (uint32_t j = 0; j < m->context_after_count && m->context_after; j++) {
            if (m->context_after[j]) {
                buf_appendf(&buf, "%s-%lu-%s\n", path,
                           (unsigned long)(m->line_number + 1 + j),
                           m->context_after[j]);
            }
        }
    }

    if (gr->total_matched > gr->count) {
        buf_appendf(&buf, "\n(%u of %u matches shown, %u files searched)\n",
                    gr->count, gr->total_matched, gr->total_files_searched);
    }

    v_result = caml_copy_string(buf.data);
    buf_free(&buf);
    if (sym_free_grep) sym_free_grep(gr);

    CAMLreturn(v_result);
}
