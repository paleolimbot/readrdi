#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include "read-rdi-common.h"

// A wrapper around the file handle and any options passed in
// from R. This is needed for R_ExecWithCleanup() but is also a
// nice abstraction around the file types
typedef struct {
    FILE* handle;
    long offset;
    R_xlen_t n_max;
    SEXP result;
} rdi_index_data_t;

int rdi_search_for_header_start(rdi_index_data_t* data) {
    size_t buf_size;
    unsigned char buf[1024];

    while ((buf_size = fread(buf, sizeof(unsigned char), 1024 - 1, data->handle)) > 0) {
        R_CheckUserInterrupt();

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
                return 1;
            }
        }
    }

    return 0;
}

SEXP readrdi_read_rdi_index_impl(void* data_void) {
    rdi_index_data_t* data = (rdi_index_data_t*) data_void;
    rdi_header_t header;

    if (fseek(data->handle, data->offset, SEEK_SET) != 0) {
        Rf_error("Can't seek to start offset %d", data->offset);
    }

    // Using the R_PreserveObject() mechanism because we may have to
    // reallocate this list if we run out of space. Could also use
    // multiple vectors here but having a list() of double() makes for
    // less realloc code and probably fewer bugs.
    R_xlen_t alloc_len = 128;
    data->result = PROTECT(Rf_allocVector(VECSXP, alloc_len));
    R_PreserveObject(data->result);
    UNPROTECT(1);

    R_xlen_t n = 0;
    long item_offset;
    uint16_t checksum;
    SEXP new_result;
    SEXP item;
    while (rdi_search_for_header_start(data)) {
        if (fread(&header, sizeof(rdi_header_t), 1, data->handle) != 1) {
            break;
        }

        // check if we need more space in the output
        if (n >= alloc_len) {
            new_result = PROTECT(Rf_allocVector(VECSXP, alloc_len * 2 + 1));
            for (R_xlen_t i = 0; i < alloc_len; i++) {
                SET_VECTOR_ELT(new_result, i, VECTOR_ELT(data->result, i));
            }
            alloc_len = alloc_len * 2 + 1;
            R_ReleaseObject(data->result);
            data->result = new_result;
            R_PreserveObject(data->result);
            UNPROTECT(1);
        }

        item_offset = ftell(data->handle) - sizeof(rdi_header_t);

        // before adding the item, try seeking to the end of the ensemble
        // and reading the checksum (if it doesn't exist, unlikely that it is
        // valid)
        if (fseek(data->handle, header.bytes_per_ensemble - sizeof(rdi_header_t), SEEK_CUR) != 0) {
            break;
        }

        if (fread(&checksum, sizeof(uint16_t), 1, data->handle) != 1) {
            break;
        }

        item = PROTECT(Rf_allocVector(REALSXP, 3));
        REAL(item)[0] = item_offset;
        REAL(item)[1] = header.bytes_per_ensemble;
        REAL(item)[2] = checksum;
        SET_VECTOR_ELT(data->result, n, item);
        UNPROTECT(1);

        if ((data->n_max >= 0) && (n >= data->n_max)) {
            break;
        }
        n++;
    }

    if (alloc_len != n) {
        SEXP final_result = PROTECT(Rf_allocVector(VECSXP, n));
        for (R_xlen_t i = 0; i < n; i++) {
            SET_VECTOR_ELT(final_result, i, VECTOR_ELT(data->result, i));
        }
        UNPROTECT(1);
        return final_result;
    } else {
        return data->result;
    }
}

void readrdi_rdi_index_cleanup(void* data_void) {
    rdi_index_data_t* data = (rdi_index_data_t*) data_void;
    if (data->handle != NULL) {
        fclose(data->handle);
        data->handle = NULL;
    }
    if (data->result != R_NilValue) {
        R_ReleaseObject(data->result);
    }
    free(data);
}

SEXP readrdi_c_rdi_index(SEXP filename, SEXP offset, SEXP n_max) {
    const char* filename_chr = Rf_translateChar(STRING_ELT(filename, 0));
    long offset_int = REAL(offset)[0];
    R_xlen_t n_max_int = REAL(n_max)[0];

    FILE* handle = fopen(filename_chr, "rb");
    if (handle == NULL) {
        Rf_error("Failed to open file '%s'", filename_chr);
    }

    rdi_index_data_t* data = (rdi_index_data_t*) malloc(sizeof(rdi_index_data_t));
    if (data == NULL) {
        fclose(handle);
        Rf_error("Failed to allocate file handle data");
    }

    data->handle = handle;
    data->offset = offset_int;
    data->n_max = n_max_int;
    data->result = R_NilValue;

    return R_ExecWithCleanup(&readrdi_read_rdi_index_impl, data, &readrdi_rdi_index_cleanup, data);
}
