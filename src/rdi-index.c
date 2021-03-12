#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "read-rdi-common.h"

SEXP readrdi_read_rdi_index_impl(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;
    rdi_header_t header;

    // seek to initial data->offset

    // while (file still has more) {
    //   search for 0x7f7f
    //   read &header (check for fail -> next)
    //   seek back to 0x7f7f and calculate checksum / check checksum
    //   
    //   add values to output vectors (maybe growing them)
    // }

    return R_NilValue;
}

void readrdi_rdi_index_cleanup(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;
    if (data->handle != NULL) {
        fclose(data->handle);
        data->handle = NULL;
    }
    free(data);
}

SEXP readrdi_c_rdi_index(SEXP filename, SEXP offset) {
    const char* filename_chr = Rf_translateChar(STRING_ELT(filename, 0));
    int offset_int = INTEGER(offset)[0];

    FILE* handle = fopen(filename_chr, "rb");
    if (handle == NULL) {
        Rf_error("Failed to open file '%s'", filename_chr);
    }

    read_rdi_data_t* data = (read_rdi_data_t*) malloc(sizeof(read_rdi_data_t));
    if (data == NULL) {
        Rf_error("Failed to allocate file handle data");
    }

    data->handle = handle;
    data->offset = offset_int;

    return R_ExecWithCleanup(&readrdi_read_rdi_index_impl, data, &readrdi_rdi_index_cleanup, data);
}
