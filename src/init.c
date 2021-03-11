#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP readrdi_c_read_rdi_meta(SEXP filename, SEXP offset);

static const R_CallMethodDef CallEntries[] = {
    {"readrdi_c_read_rdi", (DL_FUNC) &readrdi_c_read_rdi_meta, 2},
    {NULL, NULL, 0}
};

void R_init_readrdi(DllInfo* dll){
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
