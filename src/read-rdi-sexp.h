
#ifndef READ_RDI_SEXP_H
#define READ_RDI_SEXP_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "read-rdi-common.h"

SEXP rdi_create_header(R_xlen_t size);
SEXP rdi_create_fixed_leader_data(R_xlen_t size);
SEXP rdi_create_variable_leader_data(R_xlen_t size);
SEXP rdi_create_bottom_track(R_xlen_t size);

void rdi_set_header(SEXP header_df, R_xlen_t i, rdi_header_t* header, uint16_t* data_offset);
void rdi_set_fixed_leader_data(SEXP fixed_df, R_xlen_t i, rdi_fixed_leader_data_t* fixed);
void rdi_set_variable_leader_data(SEXP variable_df, R_xlen_t i, rdi_variable_leader_data_t* variable);
void rdi_set_bottom_track(SEXP bottom_track_df, R_xlen_t i, rdi_bottom_track_t* bottom_track);


#endif
