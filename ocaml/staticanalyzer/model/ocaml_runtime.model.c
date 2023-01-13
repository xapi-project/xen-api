/* Describes the behaviour of OCaml C runtime functions for
 * the goblint static analyzer.
 *
 * This only describes a simplified behaviour relevant to static analyses.
 */
#define DEBUG
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/threads.h>

#include <caml/unixsupport.h>
#include <caml/version.h>

#include <caml/gc.h>

#include <assert.h>
#include <errno.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>

#if OCAML_VERSION < 40800
#error "static analysis model for OCaml runtime requires OCaml >= 4.08"
#endif
/* it'd require a lot more ifdefs to support older versions */

/* See
 * https://goblint.readthedocs.io/en/stable/user-guide/annotating/#functions */
#include <goblint.h>

int __VERIFIER_nondet_int(void);
#define STUB __attribute__((goblint_stub))

STUB void caml_failed_assert(char *msg, char *os, int n)
{
    /* always fail assertion when called by CAMLassert */
    assert(!msg);
    assert(os);
    (void)n;
    abort();
}

/* very important to not have ';' before CAMLnoreturn_end, or the attribute
 * doesn't end up on the function!
 * Also this is just 'noreturn' instead of 'abort', because the entire program
 * may not necessarily terminate, e.g. if there is an exception handler */
CAMLnoreturn_start void __caml_exception_raised() CAMLnoreturn_end STUB;

STUB void __access_Val(value v)
{
    if ( !Is_block(v) )
        (void)Tag_val(v);
}

static header_t __atoms[Num_tags];

/* the static analyzer will ensure that all these caml_ functions check
 * that the runtime lock is held, can't easily express that as an assertion
 * (except with a trylock, but that is not modeled either)
 */

STUB value caml_alloc_atom(tag_t tag)
{
    assert(tag < Num_tags);
    header_t *hp = &__atoms[tag];
    __goblint_assume(*hp == Make_header(0, tag, 0));
    return Val_hp(hp);
}

/* could be a linked list, for simplicity it is not.
 * this should be enough for may-points-to analysis to pick up the ops */
static struct
{
    const struct custom_operations *ops;
    value v;
} a_custom_op;

static int __custom_ops_running;

STUB static void __caml_maybe_run_finalizer(void)
{
    const struct custom_operations *ops = a_custom_op.ops;
    value v = a_custom_op.v;
    uintnat bsize_32, bsize_64;

    if ( !ops || !Is_block(v) )
        return;
    /* only call finalizer once */
    a_custom_op.ops = NULL;
    a_custom_op.v = Val_unit;
    __goblint_assume(Custom_ops_val(v) == ops);

    /* See https://v2.ocaml.org/manual/intfc.html#ss:c-custom-ops
     * these functions are not allowed to trigger a GC */
    assert(!__custom_ops_running);
    __custom_ops_running = 1;
    /* Before finalizing check that other custom ops work if defined.
     * However they can raise exceptions, so use a nondeterministic int
     * to decide whether to call it or not, to ensure the finalizer is actually
     * reachable.
     * */
    if ( ops->compare && __VERIFIER_nondet_int() )
        (void)ops->compare(v, v);
    if ( ops->compare_ext && __VERIFIER_nondet_int() )
        (void)ops->compare_ext(v, v);
    if ( ops->hash && __VERIFIER_nondet_int() )
        (void)ops->hash(v);
    if ( ops->serialize && __VERIFIER_nondet_int() )
    {
        void *dst;
        uintnat size;
        ops->serialize(v, &bsize_32, &bsize_64);
        size = sizeof(void *) == 8 ? bsize_64 : bsize_32;
        dst = malloc(size);
        if ( !dst )
            caml_raise_out_of_memory();
        if ( ops->deserialize && __VERIFIER_nondet_int() )
        {
            uintnat ret = ops->deserialize(dst);
            assert(ret == size);
            /* should be initialized */
            (void)memchr(dst, 0, size);
        }
        free(dst);

        if ( ops->fixed_length )
            (void)*ops->fixed_length;
    }

    if ( ops->finalize )
        ops->finalize(v);
    __custom_ops_running = 0;
}

STUB static void __caml_move(value arg, volatile value *dest)
{
    if ( !Is_block(arg) )
        return;
    if ( arg == a_custom_op.v )
    {
        /* reachable, remove it */
        a_custom_op.v = Val_unit;
        a_custom_op.ops = NULL;
    }
    mlsize_t len = Bhsize_wosize(Wosize_val(arg));
    header_t *p = malloc(len);
    if ( !p )
        caml_raise_out_of_memory();
    void *orig = Hp_val(arg);
    memcpy(p, orig, len);
    *dest = Val_hp(p);
    free(orig);
}

#ifndef CAML_LOCAL_ROOTS
#define CAML_LOCAL_ROOTS caml_local_roots
#endif

/* anything can happen, including more allocations, etc. */
STUB void __caml_maybe_run_gc(void)
{
    if ( !__VERIFIER_nondet_int() )
        return;

    struct caml__roots_block *lr;
    int i, j;
    value *sp;

    for ( lr = CAML_LOCAL_ROOTS; lr != NULL; lr = lr->next )
    {
        for ( i = 0; i < lr->ntables; i++ )
        {
            for ( j = 0; j < lr->nitems; j++ )
            {
                sp = &(lr->tables[i][j]);
                if ( *sp != 0 )
                {
                    __caml_move(*sp, sp);
                }
            }
        }
    }

    __caml_maybe_run_finalizer();
}

STUB value caml_alloc_shr(mlsize_t wosize, tag_t tag)
{
    /* See https://v2.ocaml.org/manual/intfc.html#sss:c-simple-allocation
     * have to use Atom(t) for 0 sized blocks */
    assert(wosize > 0);
    assert(tag < Num_tags);
    assert(wosize <= Max_wosize);
    __caml_maybe_run_gc();
    /* Byte+header size from word size */
    value *p = malloc(Bhsize_wosize(wosize));
    if ( !p )
        caml_raise_out_of_memory();
    __goblint_assume(!((intnat)p & 1));
    Hd_hp(p) = Make_header(wosize, tag, 0);

    return Val_hp(p);
}

STUB value caml_alloc_small(mlsize_t wosize, tag_t tag)
{
    assert(wosize <= Max_young_wosize);
    /* alloc_small is just an optimization,
     * so for the static analyzer these are equivalent */
    return caml_alloc_shr(wosize, tag);
}

STUB value caml_alloc(mlsize_t wosize, tag_t tag)
{
    unsigned i;
    value p = caml_alloc_shr(wosize, tag);
    if ( tag < No_scan_tag )
    {
        for ( i = 0; i < wosize; i++ )
            Field(p, i) = Val_unit;
    }
    return Val_hp(p);
}

#if OCAML_VERSION < 50000
value caml_alloc_custom(struct custom_operations *ops, uintnat size,
STUB                         mlsize_t mem, mlsize_t max)
#else
value caml_alloc_custom(const struct custom_operations * ops, uintnat size,
STUB                         mlsize_t mem, mlsize_t max)
#endif
{
    assert(!!ops);
    assert(size <= Bsize_wsize(Max_wosize));
    value result = caml_alloc_shr(1 + Bsize_wsize(size + sizeof(value) - 1),
                                  Custom_tag);
    Custom_ops_val(result) = ops;
    (void)strlen(ops->identifier);
    /* make finalizer reachable from global, so the static analyzer can check
     * it */
    a_custom_op.ops = ops;
    a_custom_op.v = result;
    (void)mem;
    (void)max;
    return result;
}

STUB value caml_alloc_tuple(mlsize_t wosize) { return caml_alloc(wosize, 0); }

STUB value caml_alloc_string(mlsize_t len)
{
    /* Sys.max_string_length */
    assert(len < Bsize_wsize(Max_wosize));

    mlsize_t wosize = Wsize_bsize(len + sizeof(value));
    value *result = malloc(Bhsize_wosize(wosize));
    if ( !result )
        caml_raise_out_of_memory();
    __goblint_assume(!((intnat)result & 1));
    Hd_hp(result) = Make_header(wosize, String_tag, 0);
    Field(result, wosize - 1) = 0;
    return Val_hp(result);
}

STUB value caml_alloc_initialized_string(mlsize_t len, const char *p)
{
    value result = caml_alloc_string(len);
    memcpy(Bytes_val(result), p, len);
    return result;
}

STUB value caml_copy_string(const char *s)
{
    assert(!!s);
    return caml_alloc_initialized_string(strlen(s), s);
}


STUB value caml_copy_double(double f)
{
    value v = caml_alloc_small(Double_wosize, Double_tag);
    Store_double_val(v, f);
    return v;
}

static struct custom_operations default_ops = { "default",
                                                custom_finalize_default,
                                                custom_compare_default,
                                                custom_hash_default,
                                                custom_serialize_default,
                                                custom_deserialize_default,
                                                custom_compare_ext_default,
                                                custom_fixed_length_default };

STUB value caml_copy_int32(int32_t i)
{
    value v = caml_alloc_custom(&default_ops, sizeof(i), 0, 1);
    Int32_val(v) = i;
    return v;
}

STUB value caml_copy_int64(int64_t i)
{
    value v = caml_alloc_custom(&default_ops, sizeof(i), 0, 1);
    Int64_val(v) = i;
    return v;
}

STUB value caml_copy_nativeint(intnat i)
{
    value v = caml_alloc_custom(&default_ops, sizeof(i), 0, 1);
    Nativeint_val(v) = i;
    return v;
}

/* constness is different causing a compile error with 5.0,
 * unless we use the correct definition */
#if OCAML_VERSION < 50000
STUB value caml_alloc_array(value (*funct)(char const *), char const **array)
#else
value caml_alloc_array (value (*funct) (char const *),
STUB                         char const * const * array)
#endif
{
    CAMLparam0();
    CAMLlocal2(v, p);
    mlsize_t i, n = 0;
    while ( array[n] )
        n++;

    p = caml_alloc(n, 0);
    for ( i = 0; i < n; i++ )
    {
        v = funct(array[n]);
        assert(Tag_val(v) != Double_tag);
        caml_modify(&Field(p, n), v);
    }
    CAMLreturn(p);
}

#if OCAML_VERSION < 50000
STUB value caml_copy_string_array(char const **arr)
#else
STUB value caml_copy_string_array (char const * const* arr)
#endif
{
    return caml_alloc_array(caml_copy_string, arr);
}

STUB value caml_alloc_float_array(mlsize_t n)
{
    /* no flat float array */
    return caml_alloc(n, 0);
}

#ifndef Tag_some
#define Tag_some 0
#endif

STUB value caml_alloc_some(value v)
{
    value r = caml_alloc_small(1, Tag_some);
    Field(r, 0) = v;
    return r;
}

STUB void caml_raise_with_arg(value exn, value arg)
{
    assert(Is_block(exn));
    __access_Val(exn);
    __access_Val(arg);
    __caml_exception_raised();
}

STUB void caml_raise_with_string(value exn, const char *s)
{
    CAMLparam1(exn);
    CAMLlocal1(str);
    str = caml_copy_string(s);
    caml_raise_with_arg(exn, str);
    CAMLnoreturn;
}

static value __exn_Failure, __exn_Invalid_arg, __exn_Unix_error;

STUB void caml_failwith(const char *msg)
{
    caml_raise_with_string(__exn_Failure, msg);
}

STUB void caml_invalid_argument(const char *msg)
{
    caml_raise_with_string(__exn_Invalid_arg, msg);
}

STUB void caml_raise_constant(value exn)
{
    assert(Is_block(exn));
    __access_Val(exn);
    __caml_exception_raised();
}

STUB void caml_raise_with_args(value exn, int nargs, value arg[])
{
    int i;
    assert(Is_block(exn));
    assert(nargs >= 0);
    __access_Val(exn);
    for ( i = 0; i < nargs; i++ )
        __access_Val(arg[i]);
    __caml_exception_raised();
}

STUB void caml_unix_error(int errcode, const char *cmdname, value arg)
{
    CAMLparam1(arg);
    CAMLlocal1(str);
    str = caml_copy_string(cmdname);
    value args[3] = { Val_int(errcode), str, arg };
    caml_raise_with_args(__exn_Unix_error, 3, args);
    CAMLnoreturn;
}

STUB void caml_uerror(const char *cmdname, value arg)
{
    caml_unix_error(errno, cmdname, arg);
}

/* TODO: for 5.0 this needs to simulate multiple domains and threads instead */
pthread_mutex_t __VERIFIER_ocaml_runtime_lock = PTHREAD_MUTEX_INITIALIZER;

void __caml_run_other_thread(void);

STUB static void *__caml_maybe_call_gc(void *arg)
{
    (void)arg;
    int rc;
    rc = pthread_mutex_lock(&__VERIFIER_ocaml_runtime_lock);
    __goblint_assume(!rc);

    __caml_maybe_run_gc();

    rc = pthread_mutex_unlock(&__VERIFIER_ocaml_runtime_lock);
    __goblint_assume(!rc);
    return NULL;
}

STUB static void __caml_maybe_run_another_thread(void)
{
    pthread_attr_t attr;
    pthread_t thread;
    int rc;
    /* create thread detached, so no join will be needed */
    rc = pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
    __goblint_assume(!rc);
    /* Make it very obvious that another thread might run here, by creating one
     */
    rc = pthread_create(&thread, &attr, __caml_maybe_call_gc, NULL);
    __goblint_assume(!rc);
}

STUB void caml_enter_blocking_section(void)
{
    int rc;
    __caml_maybe_run_another_thread();
    rc = pthread_mutex_unlock(&__VERIFIER_ocaml_runtime_lock);
    __goblint_assume(!rc);
}

STUB void caml_leave_blocking_section(void)
{
    int rc;
    __caml_maybe_run_another_thread();
    rc = pthread_mutex_lock(&__VERIFIER_ocaml_runtime_lock);
    __goblint_assume(!rc);
}

STUB caml_stat_block caml_stat_alloc(asize_t s)
{
    char* p = malloc(s + 2);
    if (!p)
        caml_raise_out_of_memory();
    return (p+2); /* ensure pointer cannot be passed to free as is */
}

/* only this and caml_enter_blocking_section can be called without runtime lock
 * held! (the caml_stat_alloc_noexn too, but not implemented here) */
STUB void caml_stat_free(caml_stat_block b)
{
    assert(b);
    char* p = (b - 2);
    assert(p);
    free(p);
}

/* see sv-comp.c, the use of uninitialized value here is on purpose */
STUB int32_t __VERIFIER_nondet_int32(void)
{ int32_t val; return val; }

STUB int64_t __VERIFIER_nondet_int64(void)
{ int64_t val; return val; }

STUB value __VERIFIER_nondet_value(void)
{ value val; return val; }

static int __in_noalloc;

STUB int caml_noalloc_begin(void)
{
    __in_noalloc++;
}

STUB int caml_noalloc_end(int *noalloc)
{
    --__in_noalloc;
    __goblint_assert(__in_noalloc == *noalloc);
}

STUB int caml_alloc_point_here(void)
{
    __goblint_assert(!__in_noalloc);
}
