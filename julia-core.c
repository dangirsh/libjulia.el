#include "emacs-module.h"
#include <stdio.h>

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#include <julia.h>
#include "emacs-module-helpers.h"

int plugin_is_GPL_compatible;

// taken from emacs-sqlite3
static char*
retrieve_string(emacs_env *env, emacs_value str, ptrdiff_t *size)
{
	*size = 0;

	env->copy_string_contents(env, str, NULL, size);
	char *p = malloc(*size);
	if (p == NULL) {
		*size = 0;
		return NULL;
	}
	env->copy_string_contents(env, str, p, size);

	return p;
}

int8_t jl_unbox_bool(jl_value_t *v) JL_NOTSAFEPOINT;


#define JULIA_TO_ELISP(env, jl_value, jl_type, c_to_elisp) \
  if (jl_typeis(jl_value, jl_##jl_type##_type)) {          \
    return c_to_elisp(env, jl_unbox_##jl_type(jl_value));  \
  }                                                        \


static emacs_value
Fjulia_eval (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  ptrdiff_t size;
  char *str_arg = retrieve_string(env, args[0], &size);
  jl_value_t *val = jl_eval_string(str_arg);

  JULIA_TO_ELISP(env, val, int8, env->make_integer);
  JULIA_TO_ELISP(env, val, uint8, env->make_integer);
  JULIA_TO_ELISP(env, val, int16, env->make_integer);
  JULIA_TO_ELISP(env, val, uint16, env->make_integer);
  JULIA_TO_ELISP(env, val, int32, env->make_integer);
  JULIA_TO_ELISP(env, val, uint32, env->make_integer);
  JULIA_TO_ELISP(env, val, int64, env->make_integer);

  JULIA_TO_ELISP(env, val, float32, env->make_float)
  JULIA_TO_ELISP(env, val, float64, env->make_float)

  printf("ERROR: unexpected return type when evaluating Julia string.\n");

  // FIXME: Raise error appropriately here.
  return env->make_float(env, 0.0);
}

static emacs_value Fjulia_sqrt2 (emacs_env *env)
{
  jl_value_t *ret = jl_eval_string("sqrt(2.0)");

  double ret_unboxed = 0.0;

  if (jl_typeis(ret, jl_float64_type)) {
    ret_unboxed = jl_unbox_float64(ret);
    printf("sqrt(2.0) in C: %e \n", ret_unboxed);
  }
  else {
    printf("ERROR: unexpected return type from sqrt(::Float64)\n");
  }

  return env->make_float(env, ret_unboxed);
}

int emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);

  jl_init();

  DEFUN("julia-sqrt2", Fjulia_sqrt2, 0, 0, "testitest\n", 0);
  DEFUN("julia-eval", Fjulia_eval, 1, 1, "eval in julia\n", 0);
  provide(env, "julia-tester");

  return 0;
}
