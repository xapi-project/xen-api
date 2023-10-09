#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/threads.h>
#include <stdint.h>

/* Perform fixed amount of work proportional to argument.
   Avoid statements that take non-determinstic amount of time to complete:
   * syscalls
   * rdtsc, pause (interceptable by hypervisor, pause can also take variable length of time)
   * memory accesses (they depend on cache hit rate)

   Must have a return value that depends on the work that was performed
   to prevent the compiler from optimizing away the entire function.
 */
static uint32_t fixed_work(long work)
{
  long i;
  uint64_t state = 0;
  uint32_t output = 0;
  
  /* see https://www.pcg-random.org/pdf/hmc-cs-2014-0905.pdf 6.3.2 */
  for (i=0;i<work;i++) {
    state = state * 6364136223846793005ULL + 1;
    output += (state ^ (state >> 22)) >> (22 + (state >> 61));
  }

  return output;
}

CAMLprim value caml_bench_fixed_work(value work)
{
  CAMLparam1(work);
  long output;

  caml_release_runtime_system();
  output = fixed_work(Long_val(work));
  caml_acquire_runtime_system();

  CAMLreturn(Val_long(output));
}