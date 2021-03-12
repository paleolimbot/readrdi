#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "read-rdi-common.h"

int rdi_search_for_header_start(read_rdi_data_t* data) {
    size_t buf_size;
    unsigned char buf[1024];

    while ((buf_size = fread(buf, sizeof(unsigned char), 1024 - 1, data->handle)) > 0) {
        // search for the first byte (0x7f)
        for (size_t i = 0; i < buf_size; i++) {
            // if we have a match on the last character, we need to use the extra
            // buffer slot to read in one character
            if (buf[i] == 0x7f && (i == (buf_size - 1))) {
                if (fread(&(buf[1024 - 1]), 1, 1, data->handle) != 1) {
                    return 0;
                } else if (buf[1024 - 1] == 0x7f) {
                    fseek(data->handle, -2, SEEK_CUR);
                    return 1;
                }
            } else if ((buf[i] == 0x7f) && (buf[i + 1] == 0x7f)) {
                fseek(data->handle, i - buf_size, SEEK_CUR);
            }
        }
    }

    return 0;
}

SEXP readrdi_read_rdi_index_impl(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;
    rdi_header_t header;

    if (fseek(data->handle, data->offset, SEEK_SET) != 0) {
        Rf_error("Can't seek to start offset %d", data->offset);
    }

    while (rdi_search_for_header_start(data)) {
        if (fread(&header, sizeof(rdi_header_t), 1, data->handle) != 1) {
            break;
        }

        
    }

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
