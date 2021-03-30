
#' Unpack an RDI System Configuration
#'
#' @param system_config The `system_config` column from the `fixed_leader`
#'   item read from [read_rdi()].
#'
#' @return A data frame with one column per unpacked value.
#' @export
#'
#' @examples
#' rdi_unpack_system_config(51777)
#'
rdi_unpack_system_config <- function(system_config) {
  configs <- list(
    frequency = config_freq,
    angle = config_angle,
    beam_pattern = config_beam_pattern,
    beam_config = config_beam_config
  )

  new_data_frame(lapply(configs, config_unpack_one, as.integer(system_config)))
}

config_unpack_one <- function(config, x) {
  mask <- bitwShiftL(as.integer((2 ^ config$bits) - 1), config$shift)
  bits <- bitwShiftR(bitwAnd(x, mask), config$shift)
  config$values[bits + 1]
}

# 75: 000 ... 38: 110
config_freq <- list(
  shift = 8,
  bits = 3,
  values = c(75L, 150L, 300L, 600L, 1200L, 2400L, 3800L)
)

config_angle <- list(
  shift = 0,
  bits = 2,
  values = c(15L, 20L, 30L)
)

config_beam_pattern <- list(
  shift = 11,
  bits = 1,
  values = c("concave", "convex")
)

config_beam_config <- list(
  shift = 4,
  bits = 4,
  values = rep(NA_character_, 2^4)
)
config_beam_config$values[5] <- "janus" # 0100
config_beam_config$values[6] <- "janus demod" # 0101
config_beam_config$values[2^4] <- "janus 2 demod" # 1111
