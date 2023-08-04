/* from the goblint package, to simplify testing across multiple compiler
 * versions we do not require goblint to be installed though,
 * so have a local copy */

void __goblint_check(int exp);
void __goblint_assume(int exp);
void __goblint_assert(int exp);

void __goblint_assume_join(/* pthread_t thread */); // undeclared argument to avoid pthread.h interfering with Linux kernel headers

void __goblint_split_begin(int exp);
void __goblint_split_end(int exp);
