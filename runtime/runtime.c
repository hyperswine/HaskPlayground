/*
 * FP-RISC minimal runtime for RISC-V / spike+pk.
 *
 * Object layout (all heap pointers, 8-byte aligned):
 *   [tag:u32 | refcnt:u32 | payload...]
 *
 * Tags:
 *   0 = Int (BoxedInt)
 *   1 = Float (BoxedFloat)
 *   2 = String (BoxedString)
 *   3 = Con (BoxedCon)
 *   4 = Tuple (BoxedTuple)
 *   5 = Closure (BoxedClosure)
 *
 * True/False constructors: tag=1/tag=0 (0-payload con objects).
 *
 * Closure layout:
 *   [tag=5:u32 | refcnt:u32 | n_captured:u64 | fn_ptr:u64 | captured[0]...]
 *   fn_ptr is the _centry trampoline; at offset 16.
 *   captured[0] is at offset 24, captured[1] at 32, etc.
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* ---- object tags ---- */
#define TAG_INT     0
#define TAG_FLOAT   1
#define TAG_STRING  2
#define TAG_CON     3
#define TAG_TUPLE   4
#define TAG_CLOSURE 5

/* ---- object structs ---- */
typedef struct { uint32_t tag; uint32_t refcnt; int64_t  value; } BoxedInt;
typedef struct { uint32_t tag; uint32_t refcnt; double   value; } BoxedFloat;
typedef struct { uint32_t tag; uint32_t refcnt; uint64_t len; char data[]; } BoxedString;
typedef struct { uint32_t tag; uint32_t refcnt; uint64_t n_fields; void *fields[]; } BoxedCon;
typedef struct { uint32_t tag; uint32_t refcnt; uint64_t n;        void *slots[];  } BoxedTuple;
typedef struct {
    uint32_t tag;
    uint32_t refcnt;
    uint64_t n_captured;
    void    *fn_ptr;      /* _centry trampoline */
    void    *captured[];  /* captured[0] at offset 24, [1] at 32, ... */
} BoxedClosure;

/* ---- helpers ---- */
static void *fpr_alloc(size_t sz) {
    void *p = malloc(sz);
    if (!p) { exit(1); }
    return p;
}

/* ---- rt_box_int ---- */
void *rt_box_int(int64_t n) {
    BoxedInt *p = fpr_alloc(sizeof(BoxedInt));
    p->tag    = TAG_INT;
    p->refcnt = 1;
    p->value  = n;
    return p;
}

/* ---- rt_box_float ---- */
void *rt_box_float(double f) {
    BoxedFloat *p = fpr_alloc(sizeof(BoxedFloat));
    p->tag    = TAG_FLOAT;
    p->refcnt = 1;
    p->value  = f;
    return p;
}

/* ---- rt_box_string_static ---- */
/*
 * The generated code passes a pointer to a .rodata string literal:
 *   .quad <len>
 *   .string "..."
 * We just wrap it as a BoxedString backed by the static data.
 */
void *rt_box_string_static(void *static_ptr) {
    /* static_ptr points to: [len:8 bytes][chars...].
     * We build a BoxedString that references it. */
    uint64_t len = *(uint64_t *)static_ptr;
    BoxedString *p = fpr_alloc(sizeof(BoxedString) + len + 1);
    p->tag    = TAG_STRING;
    p->refcnt = 1;
    p->len    = len;
    memcpy(p->data, (char *)static_ptr + 8, len + 1);
    return p;
}

/* ---- arithmetic primitives ---- */
void *rt_prim_add_int(void *a, void *b) {
    return rt_box_int(((BoxedInt *)a)->value + ((BoxedInt *)b)->value);
}
void *rt_prim_sub_int(void *a, void *b) {
    return rt_box_int(((BoxedInt *)a)->value - ((BoxedInt *)b)->value);
}
void *rt_prim_mul_int(void *a, void *b) {
    return rt_box_int(((BoxedInt *)a)->value * ((BoxedInt *)b)->value);
}
void *rt_prim_div_int(void *a, void *b) {
    int64_t den = ((BoxedInt *)b)->value;
    if (den == 0) return rt_box_int(0);
    return rt_box_int(((BoxedInt *)a)->value / den);
}
void *rt_prim_negate_int(void *a) {
    return rt_box_int(-((BoxedInt *)a)->value);
}
void *rt_prim_add_float(void *a, void *b) {
    return rt_box_float(((BoxedFloat *)a)->value + ((BoxedFloat *)b)->value);
}
void *rt_prim_sub_float(void *a, void *b) {
    return rt_box_float(((BoxedFloat *)a)->value - ((BoxedFloat *)b)->value);
}
void *rt_prim_mul_float(void *a, void *b) {
    return rt_box_float(((BoxedFloat *)a)->value * ((BoxedFloat *)b)->value);
}
void *rt_prim_div_float(void *a, void *b) {
    return rt_box_float(((BoxedFloat *)a)->value / ((BoxedFloat *)b)->value);
}

/* ---- comparison primitives (return BoxedBool: tag=1 True / tag=0 False) ---- */
static void *box_bool(int b) {
    BoxedCon *p = fpr_alloc(sizeof(BoxedCon));
    p->tag      = b ? 1 : 0;   /* True=1, False=0 */
    p->refcnt   = 1;
    p->n_fields = 0;
    return p;
}
void *rt_prim_eq (void *a, void *b) { return box_bool(((BoxedInt*)a)->value == ((BoxedInt*)b)->value); }
void *rt_prim_neq(void *a, void *b) { return box_bool(((BoxedInt*)a)->value != ((BoxedInt*)b)->value); }
void *rt_prim_lt (void *a, void *b) { return box_bool(((BoxedInt*)a)->value <  ((BoxedInt*)b)->value); }
void *rt_prim_gt (void *a, void *b) { return box_bool(((BoxedInt*)a)->value >  ((BoxedInt*)b)->value); }
void *rt_prim_le (void *a, void *b) { return box_bool(((BoxedInt*)a)->value <= ((BoxedInt*)b)->value); }
void *rt_prim_ge (void *a, void *b) { return box_bool(((BoxedInt*)a)->value >= ((BoxedInt*)b)->value); }
void *rt_prim_and(void *a, void *b) { return box_bool(((BoxedCon*)a)->tag && ((BoxedCon*)b)->tag); }
void *rt_prim_or (void *a, void *b) { return box_bool(((BoxedCon*)a)->tag || ((BoxedCon*)b)->tag); }
void *rt_prim_str_concat(void *a, void *b) {
    BoxedString *sa = (BoxedString *)a, *sb = (BoxedString *)b;
    uint64_t total = sa->len + sb->len;
    BoxedString *p = fpr_alloc(sizeof(BoxedString) + total + 1);
    p->tag    = TAG_STRING;
    p->refcnt = 1;
    p->len    = total;
    memcpy(p->data,           sa->data, sa->len);
    memcpy(p->data + sa->len, sb->data, sb->len);
    p->data[total] = '\0';
    return p;
}

/* ---- closures ---- */
/*
 * rt_make_closure(a0=fn_centry, a1=n_captured, a2=cap0, a3=cap1, ...)
 * We support up to 4 captured args (a2..a5) which covers all practical cases
 * after lambda lifting.
 */
void *rt_make_closure(void *fn_centry, int64_t n_captured,
                      void *cap0, void *cap1, void *cap2, void *cap3) {
    BoxedClosure *cl = fpr_alloc(sizeof(BoxedClosure) + (size_t)n_captured * sizeof(void *));
    cl->tag        = TAG_CLOSURE;
    cl->refcnt     = 1;
    cl->n_captured = (uint64_t)n_captured;
    cl->fn_ptr     = fn_centry;
    if (n_captured > 0) cl->captured[0] = cap0;
    if (n_captured > 1) cl->captured[1] = cap1;
    if (n_captured > 2) cl->captured[2] = cap2;
    if (n_captured > 3) cl->captured[3] = cap3;
    return cl;
}

void *rt_apply_closure(void *closure, void *arg) {
    /* Generic apply: load fn_ptr and call it with t0=closure, a0=arg.
     * This is a C-level helper; the generated asm does the dispatch inline. */
    typedef void *(*FnEntry)(void *);
    BoxedClosure *cl = (BoxedClosure *)closure;
    FnEntry fn = (FnEntry)cl->fn_ptr;
    (void)arg;   /* arg passed in a0 by caller; fn_ptr reads it directly */
    return fn(arg);  /* NOTE: caller must set t0 themselves for asm dispatch */
}

void *rt_apply_closure_n(void *closure, int64_t n, ...) {
    (void)closure; (void)n;
    return (void *)0;   /* stub */
}

/* ---- allocation ---- */
void *rt_alloc(int64_t tag, int64_t size) {
    BoxedCon *p = fpr_alloc(sizeof(BoxedCon) + (size_t)size * sizeof(void *));
    p->tag      = (uint32_t)tag;
    p->refcnt   = 1;
    p->n_fields = (uint64_t)size;
    return p;
}

void *rt_alloc_con(int64_t tag, int64_t n_fields) {
    return rt_alloc(tag, n_fields);
}

void *rt_alloc_tuple(int64_t n, ...) {
    /* args: a0=first_elem, a1=second_elem, ..., a{n-1}=last_elem, a{n}=n */
    /* In the generated asm the fields are already in a0..a{n-1} and n in a{n}.
     * We receive them as regular C args here (first n args then count). */
    BoxedTuple *p = fpr_alloc(sizeof(BoxedTuple) + (size_t)n * sizeof(void *));
    p->tag    = TAG_TUPLE;
    p->refcnt = 1;
    p->n      = (uint64_t)n;
    /* Note: actual field values loaded by caller before calling us;
     * for a 2-tuple (swap) the asm loads a0=second, a1=first, a2=2. */
    return p;
}

void rt_rc_inc(void *p) { if (p) ((BoxedInt *)p)->refcnt++; }
void rt_rc_dec(void *p) { (void)p; }   /* no-op for now */

/* ---- lists (tier 1 Cons cells) ---- */
/*
 * List nodes are constructor objects:
 *   Nil:  tag=0, no fields
 *   Cons: tag=1, fields[0]=head, fields[1]=tail
 */

/* Global Nil singleton */
static BoxedCon _list_nil = { 0, 1, 0 };   /* tag=0, refcnt=1, n_fields=0 */
BoxedCon *List_dNil = &_list_nil;

void *rt_list_nil(void) {
    return List_dNil;
}

void *rt_list_cons(void *head, void *tail) {
    BoxedCon *node = fpr_alloc(sizeof(BoxedCon) + 2 * sizeof(void *));
    node->tag      = 1;    /* Cons tag */
    node->refcnt   = 1;
    node->n_fields = 2;
    node->fields[0] = head;
    node->fields[1] = tail;
    return node;
}

void *rt_list_head(void *list) {
    return ((BoxedCon *)list)->fields[0];
}

void *rt_list_tail(void *list) {
    return ((BoxedCon *)list)->fields[1];
}

void *rt_list_length(void *list) {
    int64_t n = 0;
    BoxedCon *node = (BoxedCon *)list;
    while (node->tag == 1) { n++; node = (BoxedCon *)node->fields[1]; }
    return rt_box_int(n);
}

/* Stub the rest of the list/SIMD/stream API so the linker is satisfied. */
void *rt_list_map(void *f, void *xs)           { (void)f; (void)xs; return List_dNil; }
void *rt_list_filter(void *f, void *xs)        { (void)f; (void)xs; return List_dNil; }
void *rt_list_fold(void *f, void *z, void *xs) { (void)f; (void)z; (void)xs; return z; }
void *rt_list_foldl(void *f, void *z, void *xs){ (void)f; (void)z; (void)xs; return z; }
void *rt_list_foldr(void *f, void *z, void *xs){ (void)f; (void)z; (void)xs; return z; }
void *rt_list_append(void *a, void *b)         { (void)a; return b; }
void *rt_list_reverse(void *xs)                { (void)xs; return List_dNil; }
void *rt_list_zip(void *a, void *b)            { (void)a; (void)b; return List_dNil; }
void *rt_list_zip_with(void *f, void *a, void *b) { (void)f;(void)a;(void)b; return List_dNil; }
void *rt_list_any(void *f, void *xs)           { (void)f; (void)xs; return box_bool(0); }
void *rt_list_all(void *f, void *xs)           { (void)f; (void)xs; return box_bool(1); }
void *rt_list_concat(void *xss)                { (void)xss; return List_dNil; }

/* SoA / stream / SIMD stubs */
void *rt_list_soa_create(void *n)    { (void)n; return (void*)0; }
void *rt_list_soa_get(void *s, void *i)    { (void)s;(void)i; return rt_box_int(0); }
void *rt_list_soa_set(void *s, void *i, void *v) { (void)s;(void)i;(void)v; return (void*)0; }
void *rt_list_soa_map(void *f, void *s)    { (void)f;(void)s; return (void*)0; }
void *rt_list_soa_map_all(void *f, void *s){ (void)f;(void)s; return (void*)0; }
void *rt_list_soa_filter(void *f, void *s) { (void)f;(void)s; return (void*)0; }
void *rt_list_soa_fold(void *f, void *z, void *s) { (void)f;(void)z;(void)s; return z; }
void *rt_list_soa_to_cons(void *s)   { (void)s; return List_dNil; }
void *rt_list_cons_to_soa(void *xs)  { (void)xs; return (void*)0; }
void *rt_stream_from_list(void *xs)  { (void)xs; return (void*)0; }
void *rt_stream_map(void *f, void *s){ (void)f;(void)s; return (void*)0; }
void *rt_stream_filter(void *f, void *s){ (void)f;(void)s; return (void*)0; }
void *rt_stream_fold(void *f, void *z, void *s){ (void)f;(void)z;(void)s; return z; }
void *rt_stream_zip_with(void *f, void *a, void *b){ (void)f;(void)a;(void)b; return (void*)0; }
void *rt_stream_take(void *n, void *s){ (void)n;(void)s; return (void*)0; }
void *rt_stream_force(void *s)       { (void)s; return List_dNil; }
void *rt_simd_map_add_int(void *a, void *b)  { (void)a;(void)b; return (void*)0; }
void *rt_simd_map_mul_int(void *a, void *b)  { (void)a;(void)b; return (void*)0; }
void *rt_simd_map_add_float(void *a, void *b){ (void)a;(void)b; return (void*)0; }
void *rt_simd_map_mul_float(void *a, void *b){ (void)a;(void)b; return (void*)0; }
void *rt_simd_fold_sum_int(void *s)   { (void)s; return rt_box_int(0); }
void *rt_simd_fold_sum_float(void *s) { (void)s; return rt_box_float(0.0); }
void *rt_simd_zip_add_int(void *a, void *b)  { (void)a;(void)b; return (void*)0; }
void *rt_simd_zip_mul_float(void *a, void *b){ (void)a;(void)b; return (void*)0; }
void *rt_simd_dot_product(void *a, void *b)  { (void)a;(void)b; return rt_box_float(0.0); }

/* ---- unbox helpers (used by main.c) ---- */
int64_t fpr_unbox_int(void *v)    { return ((BoxedInt *)v)->value; }
double  fpr_unbox_float(void *v)  { return ((BoxedFloat *)v)->value; }
const char *fpr_unbox_str(void *v){ return ((BoxedString *)v)->data; }
