/* This file is necessary otherwise clang instantiate the bindgen tools
 * multiple time concurrently which produce non-deterministic results. */

#include <cblas.h>
#include <lapacke.h>
