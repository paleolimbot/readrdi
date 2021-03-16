
#ifndef READ_RDI_SEXP_H
#define READ_RDI_SEXP_H

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "read-rdi-common.h"

SEXP rdi_create_header(R_xlen_t size) {
    const char* header_names[] = {"bytes_per_ensemble", "n_data_types", "data_offset", "data_type", ""};
    SEXP header_df = PROTECT(Rf_mkNamed(VECSXP, header_names));
    SET_VECTOR_ELT(header_df, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(header_df, 1, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(header_df, 2, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(header_df, 3, Rf_allocVector(VECSXP, size));
    UNPROTECT(1);
    return header_df;
}

void rdi_set_header(SEXP header_df, R_xlen_t i, rdi_header_t* header, 
                    uint16_t* data_offset, uint16_t* data_type) {
    INTEGER(VECTOR_ELT(header_df, 0))[i] = header->bytes_per_ensemble;
    INTEGER(VECTOR_ELT(header_df, 1))[i] = header->n_data_types;
    SEXP r_data_offset = PROTECT(Rf_allocVector(INTSXP, header->n_data_types));
    for (uint16_t i = 0; i < header->n_data_types; i++) {
        INTEGER(r_data_offset)[i] = data_offset[i];
    }
    SET_VECTOR_ELT(VECTOR_ELT(header_df, 2), i, r_data_offset);
    UNPROTECT(1);

    SEXP r_data_type = PROTECT(Rf_allocVector(INTSXP, header->n_data_types));
    for (uint16_t i = 0; i < header->n_data_types; i++) {
        INTEGER(r_data_type)[i] = data_type[i];
    }
    SET_VECTOR_ELT(VECTOR_ELT(header_df, 3), i, r_data_type);
    UNPROTECT(1);
}

// start generated by data-raw/read-rdi-tools.R
SEXP rdi_create_fixed_leader_data(R_xlen_t size) {
    const char* fixed_df_names[] = { "magic_number", "firmware_version", "system_config", "real_sim_flag", "lag_length", "n_beams", "n_cells", "pings_per_ensemble", "cell_size", "blank_after_transmit", "profiling_mode", "low_corr_thresh", "n_code_reps", "pct_gd_min", "error_velocity_maximum", "tpp_minutes", "tpp_seconds", "tpp_hundredths", "coord_transform", "heading_alignment", "heading_bias", "sensor_source", "sensors_available", "bin1_distance", "transmit_pulse_length", "wp_ref_layer_average", "false_target_threshold", "transmit_lag_distance", "cpu_board_serial_number", "system_bandwidth", "system_power", "serial_number", "beam_angle"  , ""};
    SEXP fixed_df = PROTECT(Rf_mkNamed(VECSXP, fixed_df_names));
    SET_VECTOR_ELT(fixed_df, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(fixed_df, 1, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(fixed_df, 2, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(fixed_df, 3, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 4, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 5, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 6, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 7, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(fixed_df, 8, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(fixed_df, 9, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(fixed_df, 10, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 11, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 12, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 13, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 14, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(fixed_df, 15, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 16, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 17, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 18, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 19, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(fixed_df, 20, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(fixed_df, 21, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 22, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 23, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(fixed_df, 24, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(fixed_df, 25, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(fixed_df, 26, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 27, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(fixed_df, 28, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(fixed_df, 29, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(fixed_df, 30, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(fixed_df, 31, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(fixed_df, 32, Rf_allocVector(RAWSXP, size));
    UNPROTECT(1);
    return fixed_df;
}

SEXP rdi_create_variable_leader_data(R_xlen_t size) {
    const char* variable_df_names[] = { "magic_number", "ensemble_number", "real_time_clock", "ensemble_number_msb", "bit_result", "sound_speed", "transducer_depth", "heading", "pitch", "roll", "salinity", "temperature", "heading_std", "pitch_std", "roll_std", "transmit_current", "transmit_voltage", "ambient_temperature", "pressure_plus", "pressure_minus", "attitude_temp", "attitude", "contamination_sensor", "pressure", "pressure_std"  , ""};
    SEXP variable_df = PROTECT(Rf_mkNamed(VECSXP, variable_df_names));
    SET_VECTOR_ELT(variable_df, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(variable_df, 1, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(variable_df, 2, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(variable_df, 3, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 4, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(variable_df, 5, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(variable_df, 6, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(variable_df, 7, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(variable_df, 8, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(variable_df, 9, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(variable_df, 10, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(variable_df, 11, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(variable_df, 12, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 13, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(variable_df, 14, Rf_allocVector(REALSXP, size));
    SET_VECTOR_ELT(variable_df, 15, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 16, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 17, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 18, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 19, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 20, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 21, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 22, Rf_allocVector(RAWSXP, size));
    SET_VECTOR_ELT(variable_df, 23, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(variable_df, 24, Rf_allocVector(VECSXP, size));
    UNPROTECT(1);
    return variable_df;
}

SEXP rdi_create_bottom_track(R_xlen_t size) {
    const char* bottom_track_df_names[] = { "magic_number", "range_lsb", "bottom_track_velocity", "bc", "ba", "bg", "range_msb"  , ""};
    SEXP bottom_track_df = PROTECT(Rf_mkNamed(VECSXP, bottom_track_df_names));
    SET_VECTOR_ELT(bottom_track_df, 0, Rf_allocVector(INTSXP, size));
    SET_VECTOR_ELT(bottom_track_df, 1, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(bottom_track_df, 2, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(bottom_track_df, 3, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(bottom_track_df, 4, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(bottom_track_df, 5, Rf_allocVector(VECSXP, size));
    SET_VECTOR_ELT(bottom_track_df, 6, Rf_allocVector(VECSXP, size));
    UNPROTECT(1);
    return bottom_track_df;
}

void rdi_set_fixed_leader_data(SEXP fixed_df, R_xlen_t i, rdi_fixed_leader_data_t* fixed) {
    INTEGER(VECTOR_ELT(fixed_df, 0))[i] = fixed->magic_number;
    SEXP r_firmware_version = PROTECT(Rf_allocVector(INTSXP, 2));
    for (int j = 0; j < 2; j++) {
        INTEGER(r_firmware_version)[j] = fixed->firmware_version[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(fixed_df, 1), i, r_firmware_version);
    UNPROTECT(1);
    SEXP r_system_config = PROTECT(Rf_allocVector(INTSXP, 2));
    for (int j = 0; j < 2; j++) {
        INTEGER(r_system_config)[j] = fixed->system_config[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(fixed_df, 2), i, r_system_config);
    UNPROTECT(1);
    RAW(VECTOR_ELT(fixed_df, 3))[i] = fixed->real_sim_flag;
    RAW(VECTOR_ELT(fixed_df, 4))[i] = fixed->lag_length;
    RAW(VECTOR_ELT(fixed_df, 5))[i] = fixed->n_beams;
    RAW(VECTOR_ELT(fixed_df, 6))[i] = fixed->n_cells;
    INTEGER(VECTOR_ELT(fixed_df, 7))[i] = fixed->pings_per_ensemble;
    REAL(VECTOR_ELT(fixed_df, 8))[i] = fixed->cell_size / 100.0;
    REAL(VECTOR_ELT(fixed_df, 9))[i] = fixed->blank_after_transmit / 100.0;
    RAW(VECTOR_ELT(fixed_df, 10))[i] = fixed->profiling_mode;
    RAW(VECTOR_ELT(fixed_df, 11))[i] = fixed->low_corr_thresh;
    RAW(VECTOR_ELT(fixed_df, 12))[i] = fixed->n_code_reps;
    RAW(VECTOR_ELT(fixed_df, 13))[i] = fixed->pct_gd_min;
    INTEGER(VECTOR_ELT(fixed_df, 14))[i] = fixed->error_velocity_maximum;
    RAW(VECTOR_ELT(fixed_df, 15))[i] = fixed->tpp_minutes;
    RAW(VECTOR_ELT(fixed_df, 16))[i] = fixed->tpp_seconds;
    RAW(VECTOR_ELT(fixed_df, 17))[i] = fixed->tpp_hundredths;
    RAW(VECTOR_ELT(fixed_df, 18))[i] = fixed->coord_transform;
    REAL(VECTOR_ELT(fixed_df, 19))[i] = fixed->heading_alignment / 100.0;
    REAL(VECTOR_ELT(fixed_df, 20))[i] = fixed->heading_bias / 100.0;
    RAW(VECTOR_ELT(fixed_df, 21))[i] = fixed->sensor_source;
    RAW(VECTOR_ELT(fixed_df, 22))[i] = fixed->sensors_available;
    REAL(VECTOR_ELT(fixed_df, 23))[i] = fixed->bin1_distance / 100.0;
    REAL(VECTOR_ELT(fixed_df, 24))[i] = fixed->transmit_pulse_length / 100.0;
    INTEGER(VECTOR_ELT(fixed_df, 25))[i] = fixed->wp_ref_layer_average;
    RAW(VECTOR_ELT(fixed_df, 26))[i] = fixed->false_target_threshold;
    INTEGER(VECTOR_ELT(fixed_df, 27))[i] = fixed->transmit_lag_distance;
    SEXP r_cpu_board_serial_number = PROTECT(Rf_allocVector(INTSXP, 8));
    for (int j = 0; j < 8; j++) {
        INTEGER(r_cpu_board_serial_number)[j] = fixed->cpu_board_serial_number[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(fixed_df, 28), i, r_cpu_board_serial_number);
    UNPROTECT(1);
    INTEGER(VECTOR_ELT(fixed_df, 29))[i] = fixed->system_bandwidth;
    RAW(VECTOR_ELT(fixed_df, 30))[i] = fixed->system_power;
    SEXP r_serial_number = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_serial_number)[j] = fixed->serial_number[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(fixed_df, 31), i, r_serial_number);
    UNPROTECT(1);
    RAW(VECTOR_ELT(fixed_df, 32))[i] = fixed->beam_angle;
}

void rdi_set_variable_leader_data(SEXP variable_df, R_xlen_t i, rdi_variable_leader_data_t* variable) {
    INTEGER(VECTOR_ELT(variable_df, 0))[i] = variable->magic_number;
    INTEGER(VECTOR_ELT(variable_df, 1))[i] = variable->ensemble_number;
    SEXP r_real_time_clock = PROTECT(Rf_allocVector(INTSXP, 7));
    for (int j = 0; j < 7; j++) {
        INTEGER(r_real_time_clock)[j] = variable->real_time_clock[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(variable_df, 2), i, r_real_time_clock);
    UNPROTECT(1);
    RAW(VECTOR_ELT(variable_df, 3))[i] = variable->ensemble_number_msb;
    INTEGER(VECTOR_ELT(variable_df, 4))[i] = variable->bit_result;
    INTEGER(VECTOR_ELT(variable_df, 5))[i] = variable->sound_speed;
    INTEGER(VECTOR_ELT(variable_df, 6))[i] = variable->transducer_depth;
    REAL(VECTOR_ELT(variable_df, 7))[i] = variable->heading / 100.0;
    REAL(VECTOR_ELT(variable_df, 8))[i] = variable->pitch / 100.0;
    REAL(VECTOR_ELT(variable_df, 9))[i] = variable->roll / 100.0;
    INTEGER(VECTOR_ELT(variable_df, 10))[i] = variable->salinity;
    REAL(VECTOR_ELT(variable_df, 11))[i] = variable->temperature / 100.0;
    RAW(VECTOR_ELT(variable_df, 12))[i] = variable->heading_std;
    REAL(VECTOR_ELT(variable_df, 13))[i] = variable->pitch_std / 10.0;
    REAL(VECTOR_ELT(variable_df, 14))[i] = variable->roll_std / 10.0;
    RAW(VECTOR_ELT(variable_df, 15))[i] = variable->transmit_current;
    RAW(VECTOR_ELT(variable_df, 16))[i] = variable->transmit_voltage;
    RAW(VECTOR_ELT(variable_df, 17))[i] = variable->ambient_temperature;
    RAW(VECTOR_ELT(variable_df, 18))[i] = variable->pressure_plus;
    RAW(VECTOR_ELT(variable_df, 19))[i] = variable->pressure_minus;
    RAW(VECTOR_ELT(variable_df, 20))[i] = variable->attitude_temp;
    RAW(VECTOR_ELT(variable_df, 21))[i] = variable->attitude;
    RAW(VECTOR_ELT(variable_df, 22))[i] = variable->contamination_sensor;
    SEXP r_pressure = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_pressure)[j] = variable->pressure[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(variable_df, 23), i, r_pressure);
    UNPROTECT(1);
    SEXP r_pressure_std = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_pressure_std)[j] = variable->pressure_std[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(variable_df, 24), i, r_pressure_std);
    UNPROTECT(1);
}

void rdi_set_bottom_track(SEXP bottom_track_df, R_xlen_t i, rdi_bottom_track_t* bottom_track) {
    INTEGER(VECTOR_ELT(bottom_track_df, 0))[i] = bottom_track->magic_number;
    SEXP r_range_lsb = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_range_lsb)[j] = bottom_track->range_lsb[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(bottom_track_df, 1), i, r_range_lsb);
    UNPROTECT(1);
    SEXP r_bottom_track_velocity = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_bottom_track_velocity)[j] = bottom_track->bottom_track_velocity[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(bottom_track_df, 2), i, r_bottom_track_velocity);
    UNPROTECT(1);
    SEXP r_bc = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_bc)[j] = bottom_track->bc[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(bottom_track_df, 3), i, r_bc);
    UNPROTECT(1);
    SEXP r_ba = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_ba)[j] = bottom_track->ba[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(bottom_track_df, 4), i, r_ba);
    UNPROTECT(1);
    SEXP r_bg = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_bg)[j] = bottom_track->bg[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(bottom_track_df, 5), i, r_bg);
    UNPROTECT(1);
    SEXP r_range_msb = PROTECT(Rf_allocVector(INTSXP, 4));
    for (int j = 0; j < 4; j++) {
        INTEGER(r_range_msb)[j] = bottom_track->range_msb[j];
    }
    SET_VECTOR_ELT(VECTOR_ELT(bottom_track_df, 6), i, r_range_msb);
    UNPROTECT(1);
}
// end generated by data-raw/read-rdi-tools.R

#endif
