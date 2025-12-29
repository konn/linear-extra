// Stolen from primal by lehins https://github.com/lehins/primal/tree/master

#include <stdbool.h>
#include <HsFFI.h>

/* See GCC reference for all of the atomic opartions in this file
 * https://gcc.gnu.org/onlinedocs/gcc-9.3.0/gcc/_005f_005fsync-Builtins.html
 */

void sync_synchronize(void)
{
  return __sync_synchronize();
}

bool sync_cas_bool(HsWord mba[], HsInt i, HsWord old, HsWord new)
{
  return __sync_bool_compare_and_swap(&mba[i], old, new);
}

HsWord sync_cas(HsWord mba[], HsInt i, HsWord old, HsWord new)
{
  return __sync_val_compare_and_swap(&mba[i], old, new);
}

HsWord sync_fetch_add(HsWord mba[], HsInt i, HsWord a)
{
  return __sync_fetch_and_add(&mba[i], a);
}

HsWord sync_fetch_sub(HsWord mba[], HsInt i, HsWord a)
{
  return __sync_fetch_and_sub(&mba[i], a);
}

HsWord sync_add_fetch(HsWord mba[], HsInt i, HsWord a)
{
  return __sync_add_and_fetch(&mba[i], a);
}

HsWord sync_sub_fetch(HsWord mba[], HsInt i, HsWord a)
{
  return __sync_sub_and_fetch(&mba[i], a);
}
