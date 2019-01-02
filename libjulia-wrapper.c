#include "emacs-module.h"

#include <signal.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <dlfcn.h>

int plugin_is_GPL_compatible;

static emacs_value error;
static emacs_value nil;

static char *copy_string (emacs_env *env, emacs_value str)
{
  ptrdiff_t length = 0;
  env->copy_string_contents (env, str, NULL, &length);
  if (env->non_local_exit_check (env))
    return NULL;

  char *name = malloc (length);
  env->copy_string_contents (env, str, name, &length);
  if (env->non_local_exit_check (env))
    {
      free (name);
      return NULL;
    }

  return name;
}

static bool get_global (emacs_env *env, emacs_value *valptr, const char *name)
{
  *valptr = env->intern (env, name);
  if (env->non_local_exit_check (env))
    return false;
  *valptr = env->make_global_ref (env, *valptr);
  return !env->non_local_exit_check (env);
}

static void null_finalizer (void *ptr) {}

static emacs_value libjulia_dlopen(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *ignore)
{
  void *handle;

  char *path = copy_string (env, args[0]);
  if (!path)
    return NULL;

  printf("Running dlopen with RTLD_GLOBAL.\n");

  handle = dlopen(path, RTLD_LAZY | RTLD_GLOBAL);
  free (path);

  if (!handle)
    {
      env->non_local_exit_signal (env, error, nil);
      return NULL;
    }
  return env->make_user_ptr (env, null_finalizer, handle);
}

int test_function (void)
{
  return 27;
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

  if (!get_global (env, &nil, "nil")
      || !get_global (env, &error, "error"))
    return -1;

  printf("Loading libjulia wrapper module.\n");

  emacs_value fset = env->intern(env, "fset");
  emacs_value sym = env->intern(env, "libjulia--dlopen");

  emacs_value func = env->make_function(env,
                                        1,  // min args
                                        1,  // max args
                                        libjulia_dlopen,
                                        "dlopen libjulia with RTLD_GLOBAL set.",
                                        NULL);
  emacs_value args[2] = {sym, func};
  env->funcall (env, fset, 2, args);
  return 0;
}
