#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP readrdi_c_read_rdi(SEXP filename, SEXP offset, SEXP op);

static const R_CallMethodDef CallEntries[] = {
    {"readrdi_c_read_rdi", (DL_FUNC) &readrdi_c_read_rdi, 3},
    {NULL, NULL, 0}
};

void R_init_readrdi(DllInfo* dll){
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
