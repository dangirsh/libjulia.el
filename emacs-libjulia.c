#include "emacs-module.h"
#include <stdio.h>
#include <julia.h>
#include "emacs-module-helpers.h"

int plugin_is_GPL_compatible;

static emacs_value Fjulia_tester (emacs_env *env)
{

  jl_value_t *ret = jl_eval_string("sqrt(2.0)");

  double ret_unboxed = 0.0;

  /* if (jl_typeis(ret, jl_float64_type)) { */
  /*   double ret_unboxed = jl_unbox_float64(ret); */
  /*   printf("sqrt(2.0) in C: %e \n", ret_unboxed); */
  /* } */
  /* else { */
  /*   printf("ERROR: unexpected return type from sqrt(::Float64)\n"); */
  /* } */

  return env->make_float (env, ret_unboxed);
}

int emacs_module_init(struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment(ert);

  jl_init();

  DEFUN("julia-tester", Fjulia_tester, 1, 1,
        "testitest\n",
        0);
  provide(env, "julia-tester");

  return 0;
}
