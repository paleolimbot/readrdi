
#' Read RDI files
#'
#' @param file A filename from which to read
#' @param types The variable types to extract from the file.
#'   Defaults to all variable types found in the first file.
#' @param offset Where to start looking for ensemble start indicators
#'   for [rdi_index()] or a vector of ensemble start offsets (probably derived
#'   from [rdi_index()]) for [read_rdi()].
#' @param n_max Maximum number of ensembles to index
#'
#' @return A `list()` with one component for each
#'
#' @export
#'
#' @examples
#' rdi_file <- system.file("extdata/19101018.rdi", package = "readrdi")
#' rdi <- read_rdi(rdi_file)
#'
read_rdi <- function(file, types = guess_rdi_types(file)) {
  rdi <- read_rdi_internal(file)

  if (!is.null(types)) {
    rdi <- rdi[intersect(types, names(rdi))]
  }

  # get rid of 'magic number' columns
  rdi <- lapply(rdi, "[", -1)
  rdi
}

#' @rdname read_rdi
#' @export
guess_rdi_types <- function(file) {
  setdiff(names(read_rdi_internal(file)), "header")
}

#' @rdname read_rdi
#' @export
rdi_index <- function(file, offset = 0, n_max = -1) {
  file <- path.expand(file)
  index_transposed <- .Call(
    "readrdi_c_rdi_index",
    file,
    as.double(offset)[1],
    as.double(n_max)[1]
  )

  new_data_frame(
    list(
      offset = vapply(index_transposed, "[", 1L, FUN.VALUE = double(1)),
      size = vapply(index_transposed, "[", 2L, FUN.VALUE = double(1)),
      checksum = vapply(index_transposed, "[", 3L, FUN.VALUE = double(1))
    )
  )
}

# Currently the C code just reads one ensemble at a time and reads
# everything. All the BSRTO files are a single ensemble when uploaded
# so it works well here. This was written using the
# explicit and implicit documentation provided by oce::read.adp.rdi()
# by Daniel Kelley.
# https://github.com/dankelley/oce/blob/develop/R/adp.rdi.R
# https://github.com/dankelley/oce/blob/develop/src/ldc_rdi_in_file.cpp
read_rdi_internal <- function(file, offset = 0L) {
  file <- path.expand(file)
  rdi <- .Call("readrdi_c_read_rdi", file, as.integer(offset)[1])

  # Should really be done in C if this starts to limit speed
  is_fixed_leader <- names(rdi) == "fixed_leader"
  rdi[is_fixed_leader] <- lapply(rdi[is_fixed_leader], read_rdi_fix_fixed_leader)
  is_variable_leader <- names(rdi) == "variable_leader"
  rdi[is_variable_leader] <- lapply(rdi[is_variable_leader], read_rdi_fix_variable_leader)
  is_bottom_track <- names(rdi) == "bottom_track"
  rdi[is_bottom_track] <- lapply(rdi[is_bottom_track],read_rdi_fix_bottom_track)

  lapply(rdi, new_data_frame)
}

read_rdi_fix_fixed_leader <- function(item) {
  item$serial_number <- readBin(
    as.raw(item$serial_number[[1]]),
    "integer", n = 1, size = 4, endian = "little", signed = TRUE
  )
  item$firmware_version <-
    as.numeric(paste0(item$firmware_version[[1]], collapse = "."))
  item$cpu_board_serial_number <-
    paste0(as.raw(item$cpu_board_serial_number[[1]]), collapse = ".")

  # interpretation of system config is device dependent, but this can be stored
  # as an integer and interpreted with bit masking if needed (not implemented)
  item$system_config <- readBin(
    as.raw(item$system_config[[1]]),
    "integer", n = 1, size = 2, endian = "big", signed = FALSE
  )

  item
}

read_rdi_fix_variable_leader <- function(item) {
  rtc <- item$real_time_clock[[1]]
  # I think parsing is better left to a higher level of abstraction
  item$real_time_clock <- sprintf(
    # "%y-%m-%d %H:%M:%OS"
    "%02d-%02d-%02d %02d:%02d:%02d.%02d",
    rtc[1], rtc[2], rtc[3], rtc[4], rtc[5], rtc[6], rtc[7]
  )

  item$transducer_depth <- item$transducer_depth / 10.0

  item$pressure <- readBin(
    as.raw(item$pressure[[1]]),
    "integer", n = 1, size = 4, endian = "little", signed = TRUE
  ) / 1000.0

  item$pressure_std <- readBin(
    as.raw(item$pressure_std[[1]]),
    "integer", n = 1, size = 4, endian = "little", signed = TRUE
  ) / 1000.0

  item
}

read_rdi_fix_bottom_track <- function(item) {
  item$bottom_track_velocity <- lapply(item$bottom_track_velocity, "/", 1000)
  item
}
