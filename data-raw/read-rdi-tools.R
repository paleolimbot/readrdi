
library(tidyverse)
library(glue)

fld_struct <- read_file("src/read-rdi-common.h") %>%
  str_match("typedef struct \\{([^\\}]+)\\}\\s*rdi_fixed_leader_data_t") %>%
  .[, 2] %>%
  str_replace_all("//.*?\n", "\n") %>%
  read_lines() %>%
  str_subset("[^\\s]+")

vld_struct <- read_file("src/read-rdi-common.h") %>%
  str_match("typedef struct \\{([^\\}]+)\\}\\s*rdi_variable_leader_data_t") %>%
  .[, 2] %>%
  str_replace_all("//.*?\n", "\n") %>%
  read_lines() %>%
  str_subset("[^\\s]+")

btm_track_struct <- read_file("src/read-rdi-common.h") %>%
  str_match("typedef struct \\{([^\\}]+)\\}\\s*rdi_bottom_track_t") %>%
  .[, 2] %>%
  str_replace_all("//.*?\n", "\n") %>%
  read_lines() %>%
  str_subset("[^\\s]+")

rdi_item_struct <- c(fld_struct, vld_struct, btm_track_struct) %>%
  str_match("([A-Za-z0-9_]+)\\s+([A-Za-z0-9_]+)(\\[[0-9]+\\])?") %>%
  .[, 2:4] %>%
  as.data.frame() %>%
  setNames(c("c_type", "c_name", "c_size")) %>%
  mutate(
    type = c(
      rep("fixed", length(fld_struct)),
      rep("variable", length(vld_struct)),
      rep("bottom_track", length(btm_track_struct))
    )
  ) %>%
  filter(!str_detect(c_name, "padding|unknown")) %>%
  group_by(type) %>%
  mutate(
    c_index = seq_len(n()) - 1,
    c_size = parse_number(c_size),
    r_c_type = case_when(
      !is.na(c_size) ~ "VECSXP",
      c_type %in% c("uint16_scaled_by_100_t", "int16_scaled_by_100_t", "uint8_scaled_by_10_t") ~ "REALSXP",
      c_type == "uint8_t" ~ "RAWSXP",
      TRUE ~ "INTSXP"
    ),
    r_c_ptr = case_when(
      !is.na(c_size) ~ NA_character_,
      c_type %in% c("uint16_scaled_by_100_t", "int16_scaled_by_100_t", "uint8_scaled_by_10_t") ~ "REAL",
      c_type == "uint8_t" ~ "REAL",
      TRUE ~ "INTEGER"
    ),
    r_c_create = glue(
      "SET_VECTOR_ELT({ type }_df, { c_index }, Rf_allocVector({ r_c_type }, size));"
    ),
    r_c_initialize = case_when(
      r_c_type == "VECSXP" ~ "// VECSXP already initialized to R_NilValue",
      r_c_type == "RAWSXP" ~ as.character(glue("memset(RAW(VECTOR_ELT({ type }_df, { c_index })), 0, size);")),
      TRUE ~ as.character(glue(
        "
        SEXP item{ c_index } = VECTOR_ELT({ type }_df, { c_index }); for (R_xlen_t i = 0; i < size; i++) { r_c_ptr }(item{ c_index })[i] = NA_{ r_c_ptr };
        "
      ))
    ),
    r_c_set = case_when(
      r_c_type == "VECSXP" ~
        glue(
          "
            SEXP r_{ c_name } = PROTECT(Rf_allocVector(INTSXP, { c_size }));
            for (int j = 0; j < { c_size }; j++) {{
                INTEGER(r_{ c_name })[j] = { type }->{ c_name }[j];
            }}
            SET_VECTOR_ELT(VECTOR_ELT({ type }_df, { c_index }), i, r_{ c_name });
            UNPROTECT(1);
            "
        ) %>%
        # indenting here makes the auto-generated code a lot prettier!
        unclass() %>% str_replace_all("\n", "\n    "),
      r_c_type == "REALSXP" & c_type %in% c("uint16_scaled_by_100_t", "int16_scaled_by_100_t") ~ glue(
        "REAL(VECTOR_ELT({ type }_df, { c_index }))[i] = { type }->{ c_name } / 100.0;"
      ) %>% unclass(),
      r_c_type == "REALSXP" & c_type == "uint8_scaled_by_10_t" ~ glue(
        "REAL(VECTOR_ELT({ type }_df, { c_index }))[i] = { type }->{ c_name } / 10.0;"
      ) %>% unclass(),
      r_c_type == "RAWSXP" ~ glue(
        "RAW(VECTOR_ELT({ type }_df, { c_index }))[i] = { type }->{ c_name };"
      ) %>% unclass(),
      TRUE ~ glue(
        "INTEGER(VECTOR_ELT({ type }_df, { c_index }))[i] = { type }->{ c_name };"
      ) %>% unclass()
    )
  )

make_fixed_df <- glue_data(
  rdi_item_struct %>% filter(type == "fixed"),
  "
SEXP rdi_create_fixed_leader_data(R_xlen_t size) {{
    const char* { type[1] }_df_names[] = {{ { paste0('\"', c_name , '\"', collapse = ', ') }  , \"\"}};
    SEXP { type[1] }_df = PROTECT(Rf_mkNamed(VECSXP, { type[1] }_df_names));
{ paste0('    ', r_c_create, collapse = '\n') }
{ paste0('    ', r_c_initialize, collapse = '\n') }
    UNPROTECT(1);
    return { type[1] }_df;
}}
")

set_fixed_df <- glue_data(
  rdi_item_struct %>% filter(type == "fixed"),
  "
void rdi_set_fixed_leader_data(SEXP { type[1] }_df, R_xlen_t i, rdi_fixed_leader_data_t* fixed) {{
{ paste0('    ', r_c_set, collapse = '\n')}
}}
")

make_variable_df <- glue_data(
  rdi_item_struct %>% filter(type == "variable"),
  "
SEXP rdi_create_variable_leader_data(R_xlen_t size) {{
    const char* { type[1] }_df_names[] = {{ { paste0('\"', c_name , '\"', collapse = ', ') }  , \"\"}};
    SEXP { type[1] }_df = PROTECT(Rf_mkNamed(VECSXP, { type[1] }_df_names));
{ paste0('    ', r_c_create, collapse = '\n') }
{ paste0('    ', r_c_initialize, collapse = '\n') }
    UNPROTECT(1);
    return { type[1] }_df;
}}
")

set_variable_df <- glue_data(
  rdi_item_struct %>% filter(type == "variable"),
  "
void rdi_set_variable_leader_data(SEXP { type[1] }_df, R_xlen_t i, rdi_variable_leader_data_t* variable) {{
{ paste0('    ', r_c_set, collapse = '\n')}
}}
")

make_bottom_track_df <- glue_data(
  rdi_item_struct %>% filter(type == "bottom_track"),
  "
SEXP rdi_create_bottom_track(R_xlen_t size) {{
    const char* { type[1] }_df_names[] = {{ { paste0('\"', c_name , '\"', collapse = ', ') }  , \"\"}};
    SEXP { type[1] }_df = PROTECT(Rf_mkNamed(VECSXP, { type[1] }_df_names));
{ paste0('    ', r_c_create, collapse = '\n') }
{ paste0('    ', r_c_initialize, collapse = '\n') }
    UNPROTECT(1);
    return { type[1] }_df;
}}
")

set_bottom_track_df <- glue_data(
  rdi_item_struct %>% filter(type == "bottom_track"),
  "
void rdi_set_bottom_track(SEXP { type[1] }_df, R_xlen_t i, rdi_bottom_track_t* bottom_track) {{
{ paste0('    ', r_c_set, collapse = '\n')}
}}
")


final_include <- glue("
// start generated by data-raw/read-rdi-tools.R
{ make_fixed_df }

{ make_variable_df }

{ make_bottom_track_df }

{ set_fixed_df }

{ set_variable_df }

{ set_bottom_track_df }
// end generated by data-raw/read-rdi-tools.R
")

# replace the auto-generated part of read-rdi-tools.c
read_file("src/read-rdi-sexp.c") %>%
  str_replace(
    regex(
      "// start generated by data-raw/read-rdi-tools\\.R.*?// end generated by data-raw/read-rdi-tools.R",
      dotall = TRUE
    ),
    final_include
  ) %>%
  write_file("src/read-rdi-sexp2.c")

unlink("src/read-rdi-sexp.c")
file.rename("src/read-rdi-sexp2.c", "src/read-rdi-sexp.c")
