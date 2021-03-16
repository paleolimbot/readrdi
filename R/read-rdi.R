
#' Read RDI files
#'
#' @param file A filename from which to read
#' @param types The variable IDs to extract from the file as a named
#'   list. Most usefully the output of [rdi_detect_data_types()]
#'   or a subset thereof.
#' @param only_valid Trim the index only to valid ensembles based on
#'   checksum. Use `TRUE` to silence the warning indicating how many
#'   ensembles were removed.
#' @param index An index created using [rdi_index()] or a subset thereof.
#' @param offset Where to start looking for ensemble start indicators
#'   for [rdi_index()] or a vector of ensemble start offsets (probably derived
#'   from [rdi_index()]) for [read_rdi()].
#' @param n_max Maximum number of ensembles to index
#'
#' @return A `list()` with one component for each
#'
#' @author
#' This was written using the explicit and implicit documentation
#' provided by `oce::read.adp.rdi()` written by Daniel Kelley.
#'
#' @export
#'
#' @examples
#' rdi_file <- system.file("extdata/19101018.rdi", package = "readrdi")
#' rdi <- read_rdi(rdi_file)
#'
read_rdi <- function(file, index = rdi_index(file),
                     types = rdi_detect_data_types(file, index)) {
  rdi <- read_rdi_internal(file, index, types)

  # get rid of 'magic number' columns
  rdi <- lapply(rdi, "[", -1)
  rdi
}

#' @rdname read_rdi
#' @export
rdi_index <- function(file, offset = 0, n_max = -1, only_valid = NA) {
  file <- path.expand(file)
  index_transposed <- .Call(
    "readrdi_c_rdi_index",
    file,
    as.double(offset)[1],
    as.double(n_max)[1]
  )

  result <- new_data_frame(
    list(
      offset = vapply(index_transposed, "[", 1L, FUN.VALUE = double(1)),
      size = vapply(index_transposed, "[", 2L, FUN.VALUE = double(1)),
      checksum = vapply(index_transposed, "[", 3L, FUN.VALUE = double(1)),
      checksum_calc = vapply(index_transposed, "[", 4L, FUN.VALUE = double(1))
    )
  )

  checksum_valid <- result$checksum == result$checksum_calc

  if (isTRUE(only_valid)) {
    result[checksum_valid, , drop = FALSE]
  } else if (identical(only_valid, NA)) {
    warning(
      sprintf(
        "Removing %d invalid ensembles based on checksum (of %d)",
        sum(!checksum_valid), length(checksum_valid)
      ),
      call. = FALSE,
      immediate. = TRUE
    )
    result[checksum_valid, , drop = FALSE]
  } else {
    result
  }
}

#' @rdname read_rdi
#' @export
rdi_detect_data_types <- function(file, index = rdi_index(file, n_max = 100)) {
  headers <- read_rdi_internal(
    file,
    index = index,
    data_types = c("header" = as.integer(0x7f7f))
  )

  all_headers <- c(as.integer(0x7f7f), unique(unlist(headers$header$data_type)))

  known_headers <- c(
    "header" = 0x7f7f,
    "fixed_leader" = 0x0000,
    "variable_leader" = 0x0080,
    "velocity" = 0x0100,
    "correlation" = 0x0200,
    "echo_intensity" = 0x0300,
    "pct_good" = 0x0400,
    "bottom_track" = 0x0600
  )

  all_names <- names(known_headers)[match(all_headers, known_headers)]
  unknown <- is.na(all_names)
  if (any(unknown)) {
    unknown_types <- paste0(
      sprintf("%#04x", all_headers[unknown]),
      collapse = ", "
    )
    warning(
      sprintf("Unknown data types in '%s':\n%s", file, unknown_types),
      call. = FALSE,
      immediate. = TRUE
    )
  }

  all_headers <- all_headers[!unknown]
  names(all_headers) <- all_names[!unknown]
  all_headers
}


read_rdi_internal <- function(file, index, data_types) {
  stopifnot(
    length(file) == 1, file.exists(file),
    is.numeric(index$offset),
    is.numeric(data_types)
  )

  mode(data_types) <- "integer"

  file <- path.expand(file)
  rdi <- .Call(
    "readrdi_c_read_rdi",
    file,
    as.numeric(index$offset),
    data_types
  )

  # Should really be done in C but difficult to auto-generate
  is_fixed_leader <- names(rdi) == "fixed_leader"
  rdi[is_fixed_leader] <- lapply(rdi[is_fixed_leader], read_rdi_fix_fixed_leader)
  is_variable_leader <- names(rdi) == "variable_leader"
  rdi[is_variable_leader] <- lapply(rdi[is_variable_leader], read_rdi_fix_variable_leader)
  is_bottom_track <- names(rdi) == "bottom_track"
  rdi[is_bottom_track] <- lapply(rdi[is_bottom_track],read_rdi_fix_bottom_track)

  lapply(rdi, new_data_frame)
}

read_rdi_fix_fixed_leader <- function(item) {
  item$serial_number <- vapply(
    item$serial_number,
    function(element) {
      if (is.null(element)) return(NA_integer_)
      readBin(
        as.raw(element),
        "integer", n = 1, size = 4, endian = "little", signed = TRUE
      )
    },
    integer(1)
  )

  item$firmware_version <- vapply(
    item$firmware_version,
    function(element) paste0(element, collapse = "."),
    character(1)
  )

  item$cpu_board_serial_number <- vapply(
    item$cpu_board_serial_number,
    function(element) paste0(as.raw(element), collapse = "."),
    character(1)
  )

  # interpretation of system config is device dependent, but this can be stored
  # as an integer and interpreted with bit masking if needed (not implemented)
  item$system_config <- vapply(
    item$system_config,
    function(element) {
      if (is.null(element)) return(NA_integer_)
      readBin(
        as.raw(element),
        "integer", n = 1, size = 2, endian = "big", signed = FALSE
      )
    },
    integer(1)
  )

  item
}

read_rdi_fix_variable_leader <- function(item) {
  # leave parsing to a higher level of abstraction
  item$real_time_clock <- vapply(
    item$real_time_clock,
    function(rtc) {
      if (is.null(rtc)) return(NA_character_)
      sprintf(
        # "%y-%m-%d %H:%M:%OS"
        "%02d-%02d-%02d %02d:%02d:%02d.%02d",
        rtc[1], rtc[2], rtc[3], rtc[4], rtc[5], rtc[6], rtc[7]
      )
    },
    character(1)
  )

  item$transducer_depth <- item$transducer_depth / 10.0

  item$pressure <- vapply(
    item$pressure,
    function(element) {
      if (is.null(element)) return(NA_real_)
      readBin(
        as.raw(element),
        "integer", n = 1, size = 4, endian = "little", signed = TRUE
      ) / 1000.0
    },
    double(1)
  )

  item$pressure_std <- vapply(
    item$pressure_std,
    function(element) {
      if (is.null(element)) return(NA_real_)
      readBin(
        as.raw(element),
        "integer", n = 1, size = 4, endian = "little", signed = TRUE
      ) / 1000.0
    },
    double(1)
  )

  item
}

read_rdi_fix_bottom_track <- function(item) {
  item$bottom_track_velocity <- lapply(item$bottom_track_velocity, "/", 1000.0)
  item
}
