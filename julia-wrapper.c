#include "emacs-module.h"
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <dlfcn.h>

#include <julia.h>

int plugin_is_GPL_compatible;

void *libjulia_dlopen(const char *path)
{
  return dlopen(path, RTLD_LAZY | RTLD_GLOBAL);
}

int emacs_module_init(struct emacs_runtime *ert)
{
  /* ######## BEGIN SAFETY BOILERPLATE ######## */
  // https://phst.github.io/emacs-modules#example
  /* Fail if Emacs is too old. */
  assert (ert->size > 0);
  if ((size_t) ert->size < sizeof *ert)
    return 1;
  emacs_env *env = ert->get_environment(ert);
  assert (env->size > 0);
  if ((size_t) env->size < sizeof *env)
    return 2;
  /* Prevent Emacs’s dangerous stack overflow recovery. */
  if (signal (SIGSEGV, SIG_DFL) == SIG_ERR)
    return 3;
  /* ######## END SAFETY BOILERPLATE ######## */

  libjulia_dlopen("/usr/local/lib/libjulia.so");

  jl_init();

  return 0;
}
