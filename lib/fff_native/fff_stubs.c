#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

/* M1: Minimal stubs — just enough for the build to link.
   Real implementations come in M2+. */

/* Lifecycle */
CAMLprim value caml_fff_create_instance(value base_path, value frecency_path,
                                        value history_path, value ai_mode)
{
  CAMLparam4(base_path, frecency_path, history_path, ai_mode);
  caml_failwith("fff_create_instance: not yet implemented");
  CAMLreturn(Val_unit);
}

CAMLprim value caml_fff_destroy(value handle)
{
  CAMLparam1(handle);
  CAMLreturn(Val_unit);
}

CAMLprim value caml_fff_is_available(value unit)
{
  CAMLparam1(unit);
  CAMLreturn(Val_true);
}
