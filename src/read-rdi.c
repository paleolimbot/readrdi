
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <stdlib.h>

#include "read-rdi-common.h"
#include "read-rdi-sexp.h"

// A wrapper around the file handle and any options passed in
// from R. This is needed for R_ExecWithCleanup().
typedef struct {
    FILE* handle;
    SEXP offset;
    SEXP types;
} read_rdi_data_t;

void rdi_seek_absolute(FILE* handle, size_t offset) {
    if (fseek(handle, offset, SEEK_SET) != 0) {
        Rf_error("Seek to file offset %d failed", offset);
    }
}

void rdi_read_uint8_n(uint8_t* buf, size_t n, FILE* handle) {
    size_t size_read = fread(buf, sizeof(uint8_t), n, handle);
    if (size_read != n) {
        Rf_error(
            "Read %d 8-bit unsigned integers at offset %d but expected %d",
            size_read,
            ftell(handle),
            n
        );
    }
}

void rdi_read_uint16_n(uint16_t* buf, size_t n, FILE* handle) {
    size_t size_read = fread(buf, sizeof(uint16_t), n, handle);
    if (size_read != n) {
        Rf_error(
            "Read %d 16-bit unsigned integers at offset %d but expected %d",
            size_read,
            ftell(handle),
            n
        );
    }
}

void rdi_read_int16_n(int16_t* buf, size_t n, FILE* handle) {
    size_t size_read = fread(buf, sizeof(int16_t), n, handle);
    if (size_read != n) {
        Rf_error(
            "Read %d 16-bit integers at offset %d but expected %d",
            size_read,
            ftell(handle),
            n
        );
    }
}

uint16_t rdi_read_uint16(FILE* handle) {
    uint16_t result;
    size_t size_read = fread(&result, sizeof(uint16_t), 1, handle);
    if (size_read != 1) {
        Rf_error(
            "Read %d 16-bit integers at offset %d but expected %d",
            size_read,
            ftell(handle),
            1
        );
    }

    return result;
}

void rdi_read_header(rdi_header_t* header, FILE* handle) {
    size_t size_read = fread(header, sizeof(rdi_header_t), 1, handle);
    if (size_read != 1) {
        Rf_error("Incomplete header at offset %d", ftell(handle));
    }
}

void rdi_read_fixed_leader_data(rdi_fixed_leader_data_t* fixed, FILE* handle) {
    size_t size_read = fread(fixed, sizeof(rdi_fixed_leader_data_t), 1, handle);
    if (size_read != 1) {
        Rf_error("Incomplete fixed leader data at offset %d", ftell(handle));
    }
}

void rdi_read_variable_leader_data(rdi_variable_leader_data_t* variable, FILE* handle) {
    size_t size_read = fread(variable, sizeof(rdi_fixed_leader_data_t), 1, handle);
    if (size_read != 1) {
        Rf_error("Incomplete variable leader data at offset %d", ftell(handle));
    }
}

void rdi_read_bottom_track(rdi_bottom_track_t* bottom_track, FILE* handle) {
    size_t size_read = fread(bottom_track, sizeof(rdi_bottom_track_t), 1, handle);
    if (size_read != 1) {
        Rf_error("Incomplete bottom track data at offset %d", ftell(handle));
    }
}

// auto-generating these doesn't work well because the
// objects have a variable length depending on values from the leader data
SEXP rdi_create_array_container(R_xlen_t size, const char* name) {
    const char* df_names[] = {"magic_number", name, ""};
    SEXP result = PROTECT(Rf_mkNamed(VECSXP, df_names));
    SET_VECTOR_ELT(result, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(result, 1, Rf_allocVector(VECSXP, size));
    UNPROTECT(1);
    return result;
}

void rdi_read_velocity(SEXP velocity_df, R_xlen_t i, 
                       FILE* handle, uint8_t n_beams, uint8_t n_cells) {
    // need to advance the cursor past the magic_number
    uint16_t magic_number = rdi_read_uint16(handle);

    int size = n_beams * n_cells;

    int16_t buffer[size];
    rdi_read_int16_n(buffer, size, handle);

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

void rdi_read_uint8_array_sexp(SEXP container_df, R_xlen_t i, 
                               FILE* handle, uint8_t n_beams, uint8_t n_cells) {
    // need to advance the cursor past the magic_number
    uint16_t magic_number = rdi_read_uint16(handle);
    INTEGER(VECTOR_ELT(container_df, 0))[i] = magic_number;

    SEXP values = PROTECT(Rf_allocMatrix(RAWSXP, n_beams, n_cells));
    rdi_read_uint8_n(RAW(values), n_beams * n_cells, handle);
    SET_VECTOR_ELT(VECTOR_ELT(container_df, 1), i, values);
    UNPROTECT(1);
}

int rdi_container_type_index(read_rdi_data_t* data, uint16_t data_type) {
    int n_data_types = Rf_length(data->types);
    for (int i = 0; i < n_data_types; i++) {
        if (INTEGER(data->types)[i] == data_type) {
            return i;
        }
    }

    return -1;
}

void rdi_read_ensemble_sexp(read_rdi_data_t* data, SEXP container, R_xlen_t ensemble_id) {
    FILE* handle = data->handle;

    rdi_header_t header;
    rdi_read_header(&header, handle);

    // check that this is actually an ensemble header
    if (header.magic_number != RDI_ENSEMBLE_HEADER_UINT16) {
        Rf_error(
            "Expected %#04x at start of ensemble but found %#04x",
            RDI_ENSEMBLE_HEADER_UINT16,
            header.magic_number
        );
    }

    uint16_t data_offset[header.n_data_types];
    rdi_read_uint16_n(data_offset, header.n_data_types, handle);

    // get machine- and human-readable ids for the data types in this file
    // leave room for the header as the first item
    uint16_t data_type[header.n_data_types];
    int container_type_index[header.n_data_types];
    for (uint8_t i = 0; i < header.n_data_types; i++) {
        rdi_seek_absolute(handle, data_offset[i]);
        data_type[i] = rdi_read_uint16(handle);
        container_type_index[i] = rdi_container_type_index(data, data_type[i]);
    }

    int container_header_index = rdi_container_type_index(data, RDI_ENSEMBLE_HEADER_UINT16);
    if (container_header_index != -1) {
        rdi_set_header(
            VECTOR_ELT(container, container_header_index), 
            ensemble_id, &header, data_offset, data_type
        );
    }

    // reading some types requires data from the fixed leader
    // initialize these values so that we can error if the fixed
    // leader hasn't been read yet
    SEXP item;
    rdi_fixed_leader_data_t fixed;
    fixed.n_beams = 0;
    fixed.n_cells = 0;

    rdi_variable_leader_data_t variable;
    rdi_bottom_track_t bottom_track;
    for (uint8_t i = 0; i < header.n_data_types; i++) {
        if (container_type_index[i] == -1) {
            // fill in empty spaces!
            continue;
        }

        item = VECTOR_ELT(container, container_type_index[i]);

        rdi_seek_absolute(handle, data_offset[i]);
        switch(data_type[i]) {
        case RDI_TYPE_FIXED_LEADER:
            rdi_read_fixed_leader_data(&fixed, handle);
            rdi_set_fixed_leader_data(item, ensemble_id, &fixed);
            break;
        case RDI_TYPE_VARIABLE_LEADER:
            rdi_read_variable_leader_data(&variable, handle);
            rdi_set_variable_leader_data(item, ensemble_id, &variable);
            break;
        case RDI_TYPE_BOTTOM_TRACK:
            if (fixed.n_beams != 4) {
                Rf_error("Can't read bottom track type with n_beams != 4");
            }
            rdi_read_bottom_track(&bottom_track, handle);
            rdi_set_bottom_track(item, ensemble_id, &bottom_track);
            break;
        case RDI_TYPE_VELOCITY:
            if (fixed.n_beams == 0) {
                Rf_error("Can't read velocity type without fixed leader data");
            }
            rdi_read_velocity(item, ensemble_id, handle, fixed.n_beams, fixed.n_cells);
            break;
        case RDI_TYPE_CORRELATION:
        case RDI_TYPE_ECHO_INTENSITY:
        case RDI_TYPE_PCT_GOOD:
            rdi_read_uint8_array_sexp(item, ensemble_id, handle, fixed.n_beams, fixed.n_cells);
            break;
        default:
            // unsupported columns are reported elsewhere
            break;
        }
    }
}

SEXP readrdi_read_rdi_impl(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;

    int n_data_types = Rf_length(data->types);
    int* data_type = INTEGER(data->types);
    R_xlen_t n_ensembles = Rf_xlength(data->offset);

    // alloc the container
    SEXP container = PROTECT(Rf_allocVector(VECSXP, n_data_types));
    Rf_setAttrib(container, R_NamesSymbol, Rf_getAttrib(data->types, R_NamesSymbol));

    // alloc the individual components
    SEXP item;
    for (int i = 0; i < n_data_types; i++) {
        switch(data_type[i]) {
        case RDI_ENSEMBLE_HEADER_UINT16:
            item = PROTECT(rdi_create_header(n_ensembles));
            break;
        case RDI_TYPE_FIXED_LEADER:
            item = PROTECT(rdi_create_fixed_leader_data(n_ensembles));
            break;
        case RDI_TYPE_VARIABLE_LEADER:
            item = PROTECT(rdi_create_variable_leader_data(n_ensembles));
            break;
        case RDI_TYPE_BOTTOM_TRACK:
            item = PROTECT(rdi_create_bottom_track(n_ensembles));
            break;
        case RDI_TYPE_VELOCITY:
            item = PROTECT(rdi_create_array_container(n_ensembles, "velocity"));
            break;
        case RDI_TYPE_CORRELATION:
            item = PROTECT(rdi_create_array_container(n_ensembles, "correlation"));
            break;
        case RDI_TYPE_ECHO_INTENSITY:
            item = PROTECT(rdi_create_array_container(n_ensembles, "echo_intensity"));
            break;
        case RDI_TYPE_PCT_GOOD:
            item = PROTECT(rdi_create_array_container(n_ensembles, "pct_good"));
            break;
        default:
            Rf_error("Unsupported rdi data type: %d", data_type[i]);
        }

        SET_VECTOR_ELT(container, i, item);
        UNPROTECT(1);
    }

    // read each ensemble pointed to by offset
    for (R_xlen_t i = 0; i < n_ensembles; i++) {
        rdi_seek_absolute(data->handle, REAL(data->offset)[i]);
        rdi_read_ensemble_sexp(data, container, i);
    }

    UNPROTECT(1);
    return container;
}

void readrdi_read_rdi_cleanup(void* data_void) {
    read_rdi_data_t* data = (read_rdi_data_t*) data_void;
    if (data->handle != NULL) {
        fclose(data->handle);
        data->handle = NULL;
    }
    free(data);
}

SEXP readrdi_c_read_rdi(SEXP filename, SEXP offset, SEXP types) {
    const char* filename_chr = Rf_translateChar(STRING_ELT(filename, 0));

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
    data->offset = offset;
    data->types = types;

    return R_ExecWithCleanup(&readrdi_read_rdi_impl, data, &readrdi_read_rdi_cleanup, data);
}
