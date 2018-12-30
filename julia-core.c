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


#define JL_PRIMITIVE_TO_ELISP(env, jl_value, jl_type, c_to_elisp) \
  if (jl_typeis(jl_value, jl_##jl_type##_type)) {          \
    return c_to_elisp(env, jl_unbox_##jl_type(jl_value));  \
  }                                                        \


static emacs_value
jl_to_elisp(emacs_env *env, jl_value_t *jl_value)
{
  char *jl_type_name = jl_typeof_str(jl_value);
  /* printf("Attepting to convert Julia type %s to Elisp\n.", jl_type_name); */

  JL_PRIMITIVE_TO_ELISP(env, jl_value, int8, env->make_integer);
  JL_PRIMITIVE_TO_ELISP(env, jl_value, uint8, env->make_integer);
  JL_PRIMITIVE_TO_ELISP(env, jl_value, int16, env->make_integer);
  JL_PRIMITIVE_TO_ELISP(env, jl_value, uint16, env->make_integer);
  JL_PRIMITIVE_TO_ELISP(env, jl_value, int32, env->make_integer);
  JL_PRIMITIVE_TO_ELISP(env, jl_value, uint32, env->make_integer);
  JL_PRIMITIVE_TO_ELISP(env, jl_value, int64, env->make_integer);
  JL_PRIMITIVE_TO_ELISP(env, jl_value, uint64, env->make_integer);

  JL_PRIMITIVE_TO_ELISP(env, jl_value, float32, env->make_float);
  JL_PRIMITIVE_TO_ELISP(env, jl_value, float64, env->make_float);


  if (jl_typeis(jl_value, jl_string_type)) {
    return env->make_string(env, jl_string_data(jl_value), jl_string_len(jl_value));
  }

  printf("ERROR: Conversion of Julia type %s to Emacs Lisp is an unsupported.\n", jl_type_name);

  // FIXME: Raise error appropriately here.
  return env->make_float(env, 0.0);
}

static emacs_value
Fjulia_eval (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  ptrdiff_t size;
  char *str_arg = retrieve_string(env, args[0], &size);
  jl_value_t *val = jl_eval_string(str_arg);
  if (jl_exception_occurred()){
    char *exception_str = jl_typeof_str(jl_exception_occurred());
    printf("Exception from jl_eval_string: %s \n", exception_str);
    printf("Returning exception string to Emacs.");
    return env->make_string(env, exception_str, strlen(exception_str));
  }
  else {
    JL_GC_PUSH1(&val);
    emacs_value emacs_val = jl_to_elisp(env, val);
    JL_GC_POP();
    return emacs_val;
  }
}


static emacs_value
Fjulia_eval_blind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  ptrdiff_t size;
  char *str_arg = retrieve_string(env, args[0], &size);
  jl_eval_string(str_arg);
  char *return_message = "Fatal error in Fjulia_eval_blind";
  if (jl_exception_occurred()){
    char *exception_str = jl_typeof_str(jl_exception_occurred());
    printf("Exception from jl_eval_string: %s \n", exception_str);
    printf("Returning exception string to Emacs.");
    return_message = exception_str;
  }
  else {
    return_message = "Successfully evaluated in Julia.";
  }
  return env->make_string(env, return_message, strlen(return_message));
}

// Used by julia-simple-test
static emacs_value Fjulia_sqrt2 (emacs_env *env)
{
  jl_value_t *ret = jl_eval_string("sqrt(2.0)");

  double ret_unboxed = 0.0;

  if (jl_typeis(ret, jl_float64_type)) {
    ret_unboxed = jl_unbox_float64(ret);
  }
  else {
    printf("ERROR: unexpected return type from sqrt(::Float64)\n");
  }

  return env->make_float(env, ret_unboxed);
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

  jl_init();

  DEFUN("julia-sqrt2", Fjulia_sqrt2, 0, 0, "used by julia-simple-test\n", 0);
  DEFUN("julia-eval", Fjulia_eval, 1, 1, "eval in julia and return value.\n", 0);
  DEFUN("julia-eval-blind", Fjulia_eval_blind, 1, 1, "eval in julia, but don't return value.\n", 0);
  provide(env, "julia-tester");

  return 0;
}
