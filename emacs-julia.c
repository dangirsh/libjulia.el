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

static emacs_value
Fjulia_eval (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  ptrdiff_t size;
  char *str_arg = retrieve_string(env, args[0], &size);
  jl_value_t *ret = jl_eval_string(str_arg);

  double ret_unboxed = 0.0;

  if (jl_typeis(ret, jl_float64_type)) {
    ret_unboxed = jl_unbox_float64(ret);
  }
  // FIXME: Handle non-float return types here!
  else {
    printf("ERROR: unexpected return type when evaluating buffer.\n");
  }

  return env->make_float(env, ret_unboxed);
  /* return env->make_string(env, str_arg); */
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
