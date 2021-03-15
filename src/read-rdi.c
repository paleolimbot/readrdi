
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>

#include "read-rdi-common.h"
#include "read-rdi-sexp.h"

// A wrapper around the file handle and any options passed in
// from R. This is needed for R_ExecWithCleanup() but is also a
// nice abstraction around the file types
typedef struct {
    FILE* handle;
    int offset;
} read_rdi_data_t;

void rdi_seek_absolute(read_rdi_data_t* data, size_t offset) {
    if (fseek(data->handle, offset, SEEK_SET) != 0) {
        Rf_error("Seek to file offset %d failed", offset);
    }
}

void rdi_read_uint8_n(uint8_t* buf, size_t n, read_rdi_data_t* data) {
    size_t size_read = fread(buf, sizeof(uint8_t), n, data->handle);
    if (size_read != n) {
        Rf_error(
            "Read %d 8-bit unsigned integers at offset %d but expected %d",
            size_read,
            ftell(data->handle),
            n
        );
    }
}

void rdi_read_uint16_n(uint16_t* buf, size_t n, read_rdi_data_t* data) {
    size_t size_read = fread(buf, sizeof(uint16_t), n, data->handle);
    if (size_read != n) {
        Rf_error(
            "Read %d 16-bit unsigned integers at offset %d but expected %d",
            size_read,
            ftell(data->handle),
            n
        );
    }
}

void rdi_read_int16_n(int16_t* buf, size_t n, read_rdi_data_t* data) {
    size_t size_read = fread(buf, sizeof(int16_t), n, data->handle);
    if (size_read != n) {
        Rf_error(
            "Read %d 16-bit integers at offset %d but expected %d",
            size_read,
            ftell(data->handle),
            n
        );
    }
}

uint16_t rdi_read_uint16(read_rdi_data_t* data) {
    uint16_t result;
    size_t size_read = fread(&result, sizeof(uint16_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error(
            "Read %d 16-bit integers at offset %d but expected %d",
            size_read,
            ftell(data->handle),
            1
        );
    }

    return result;
}

void rdi_read_header(rdi_header_t* header, read_rdi_data_t* data) {
    size_t size_read = fread(header, sizeof(rdi_header_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error("Incomplete header at offset %d", ftell(data->handle));
    }
}

void rdi_read_fixed_leader_data(rdi_fixed_leader_data_t* fixed, read_rdi_data_t* data) {
    size_t size_read = fread(fixed, sizeof(rdi_fixed_leader_data_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error("Incomplete fixed leader data at offset %d", ftell(data->handle));
    }
}

void rdi_read_variable_leader_data(rdi_variable_leader_data_t* variable, read_rdi_data_t* data) {
    size_t size_read = fread(variable, sizeof(rdi_fixed_leader_data_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error("Incomplete variable leader data at offset %d", ftell(data->handle));
    }
}

void rdi_read_bottom_track(rdi_bottom_track_t* bottom_track, read_rdi_data_t* data) {
    size_t size_read = fread(bottom_track, sizeof(rdi_bottom_track_t), 1, data->handle);
    if (size_read != 1) {
        Rf_error("Incomplete bottom track data at offset %d", ftell(data->handle));
    }
}

// auto-generating these doesn't work well because the
// objects have a variable length depending on values from the leader data
SEXP rdi_create_velocity(R_xlen_t size) {
    const char* velocity_names[] = {"magic_number", "velocity", ""};
    SEXP velocity_df = PROTECT(Rf_mkNamed(VECSXP, velocity_names));
    SET_VECTOR_ELT(velocity_df, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(velocity_df, 1, Rf_allocVector(VECSXP, size));
    UNPROTECT(1);
    return velocity_df;
}

void rdi_read_velocity(SEXP velocity_df, R_xlen_t i, 
                       read_rdi_data_t* data, uint8_t n_beams, uint8_t n_cells) {
    // need to advance the cursor past the magic_number
    uint16_t magic_number = rdi_read_uint16(data);

    int size = n_beams * n_cells;

    int16_t buffer[size];
    rdi_read_int16_n(buffer, size, data);

    SEXP velocity = PROTECT(Rf_allocMatrix(REALSXP, n_beams, n_cells));
    double* velocity_values = REAL(velocity);

    for (int i = 0; i < size; i++) {
        if (buffer[i] == INT16_MIN) {
            velocity_values[i] = NA_REAL;
        } else {
            velocity_values[i] = buffer[i] / 1000.0;
        }
    }

    INTEGER(VECTOR_ELT(velocity_df, 0))[i] = magic_number;    
    SET_VECTOR_ELT(VECTOR_ELT(velocity_df, 1), i, velocity);
    UNPROTECT(1);
}

SEXP rdi_read_uint8_array_sexp(read_rdi_data_t* data, const char* name,
                               uint8_t n_beams, uint8_t n_cells) {
    // need to advance the cursor past the magic_number
    uint16_t magic_number = rdi_read_uint16(data);

    SEXP values = PROTECT(Rf_allocMatrix(RAWSXP, n_beams, n_cells));
    rdi_read_uint8_n(RAW(values), n_beams * n_cells, data);

    const char* df_names[] = {"magic_number", name, ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, df_names));
    SET_VECTOR_ELT(result, 0, Rf_ScalarInteger(magic_number));

    // list-coumn with matrix as the item
    SET_VECTOR_ELT(result, 1, Rf_allocVector(VECSXP, 1));
    SET_VECTOR_ELT(VECTOR_ELT(result, 1), 0, values);

    UNPROTECT(2);
    return result;
}

SEXP rdi_read_ensemble_sexp(read_rdi_data_t* data) {
    rdi_header_t header;
    rdi_read_header(&header, data);

    // check that this is actually an ensemble header
    if (header.magic_number != RDI_ENSEMBLE_HEADER_UINT16) {
        Rf_error(
            "Expected %#04x at start of ensemble but found %#04x",
            RDI_ENSEMBLE_HEADER_UINT16,
            header.magic_number
        );
    }

    uint16_t data_offset[header.n_data_types];
    rdi_read_uint16_n(data_offset, header.n_data_types, data);

    // get machine- and human-readable ids for the data types in this file
    // leave room for the header as the first item
    uint16_t data_type[header.n_data_types];
    const char* data_type_name[header.n_data_types + 2];
    data_type_name[0] = "header";
    data_type_name[header.n_data_types + 1] = "";
    for (uint8_t i = 0; i < header.n_data_types; i++) {
        rdi_seek_absolute(data, data_offset[i]);
        data_type[i] = rdi_read_uint16(data);
        data_type_name[i + 1] = rdi_item_type_label(data_type[i]);
    }

    // allocate container using the human-readable names
    SEXP container = PROTECT(Rf_mkNamed(VECSXP, data_type_name));
    SET_VECTOR_ELT(container, 0, rdi_create_header(1));
    rdi_set_header(VECTOR_ELT(container, 0), 0, &header, data_offset);
    SET_VECTOR_ELT(container, 1, Rf_allocVector(VECSXP, header.n_data_types));

    // for each data type, read the type and convert it to a SEXP
    SEXP item;

    // reading some types requires data from the fixed leader
    // initialize these values so that we can error if the fixed
    // leader hasn't been read yet
    rdi_fixed_leader_data_t fixed;
    fixed.n_beams = 0;
    fixed.n_cells = 0;

    rdi_variable_leader_data_t variable;
    rdi_bottom_track_t bottom_track;
    for (uint8_t i = 0; i < header.n_data_types; i++) {
        rdi_seek_absolute(data, data_offset[i]);
        switch(data_type[i]) {
        case RDI_TYPE_FIXED_LEADER:
            rdi_read_fixed_leader_data(&fixed, data);
            item = PROTECT(rdi_create_fixed_leader_data(1));
            rdi_set_fixed_leader_data(item, 0, &fixed);
            break;
        case RDI_TYPE_VARIABLE_LEADER:
            rdi_read_variable_leader_data(&variable, data);
            item = PROTECT(rdi_create_variable_leader_data(1));
            rdi_set_variable_leader_data(item, 0, &variable);
            break;
        case RDI_TYPE_BOTTOM_TRACK:
            if (fixed.n_beams != 4) {
                Rf_error("Can't read bottom track type with n_beams != 4");
            }
            rdi_read_bottom_track(&bottom_track, data);
            item = PROTECT(rdi_create_bottom_track(1));
            rdi_set_bottom_track(item, 0, &bottom_track);
            break;
        case RDI_TYPE_VELOCITY:
            if (fixed.n_beams == 0) {
                Rf_error("Can't read velocity type without fixed leader data");
            }
            item = PROTECT(rdi_create_velocity(1));
            rdi_read_velocity(item, 0, data, fixed.n_beams, fixed.n_cells);
            break;
        case RDI_TYPE_CORRELATION:
        case RDI_TYPE_ECHO_INTENSITY:
        case RDI_TYPE_PCT_GOOD:
            item = PROTECT(rdi_read_uint8_array_sexp(data, data_type_name[i + 1], fixed.n_beams, fixed.n_cells));
            break;
        default:
            item = PROTECT(R_NilValue);
            break;
        }

        SET_VECTOR_ELT(container, i + 1, item);
        UNPROTECT(1);
    }

    UNPROTECT(1);
    return container;
}

SEXP readrdi_read_rdi_impl(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;
    rdi_seek_absolute(data, data->offset);
    return rdi_read_ensemble_sexp(data);
}

void readrdi_read_rdi_cleanup(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;
    if (data->handle != NULL) {
        fclose(data->handle);
        data->handle = NULL;
    }
    free(data);
}

SEXP readrdi_c_read_rdi(SEXP filename, SEXP offset) {
    const char* filename_chr = Rf_translateChar(STRING_ELT(filename, 0));
    int offset_int = INTEGER(offset)[0];

    FILE* handle = fopen(filename_chr, "rb");
    if (handle == NULL) {
        Rf_error("Failed to open file '%s'", filename_chr);
    }

    read_rdi_data_t* data = (read_rdi_data_t*) malloc(sizeof(read_rdi_data_t));
    if (data == NULL) {
        fclose(handle);
        Rf_error("Failed to allocate file handle data");
    }

    data->handle = handle;
    data->offset = offset_int;

    return R_ExecWithCleanup(&readrdi_read_rdi_impl, data, &readrdi_read_rdi_cleanup, data);
}
