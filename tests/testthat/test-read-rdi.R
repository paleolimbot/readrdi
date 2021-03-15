
test_that("rdi_index() works on single-ensemble files", {
  single_file <- system.file("extdata/19101018.rdi", package = "readrdi")
  expect_identical(
    rdi_index(single_file, offset = 0),
    data.frame(offset = 0, size = 739, checksum = 10383, checksum_calc = 10383)
  )

  expect_identical(
    rdi_index(single_file, offset = 1),
    data.frame(
      offset = double(),
      size = double(),
      checksum = double(),
      checksum_calc = double()
    )
  )
})

test_that("rdi_index() works on multiple ensembles in one file", {
  # need to combine a few versions of the file to get a more realistic thing
  # to index
  single_file <- system.file("extdata/19101018.rdi", package = "readrdi")
  file_in <- file(single_file, "rb")
  single_file_bin <- readBin(file_in, "raw", file.size(single_file))
  close(file_in)

  checksum <- as.integer(
    paste0(
      "0x",
      single_file_bin[length(single_file_bin)],
      single_file_bin[length(single_file_bin) - 1]
    )
  )

  tmp_multi <- tempfile()
  file_out <- file(tmp_multi, open = "wb")
  writeBin(single_file_bin, file_out)
  writeBin(single_file_bin, file_out)
  # write some garbage to make sure some searching has to happen
  writeBin(as.raw(1:5), file_out)
  writeBin(single_file_bin, file_out)
  close(file_out)

  # the ensemble size is two bytes less than one might expect
  # because there is a two byte checksum and the number of bytes
  # indicated by the header either doesn't include the 0x7f7f or
  # doesn't include the checksum.
  index <- rdi_index(tmp_multi)

  expect_true(all(index$size == 739))
  expect_true(all(index$checksum == checksum))
  expect_identical(nrow(index), 3L)
  expect_identical(index$offset[1], 0)
  expect_identical(index$offset[2], 739 + 2)
  expect_identical(index$offset[3], 739 + 2 + 739 + 2 + 5)

  unlink(tmp_multi)
})

test_that("read_rdi() works", {
  file <- system.file("extdata/19101018.rdi", package = "readrdi")

  rdi <- read_rdi(file)

  expect_identical(
    rdi$variable_leader$real_time_clock,
    "19-10-10 18:00:03.08"
  )
})

test_that("read_rdi() works for filenames with non ASCII characters", {
  file <- system.file("extdata/19101018.rdi", package = "readrdi")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  file.copy(file, file.path(temp_dir, "soméoddname.rdi"))

  expect_identical(
    read_rdi(file.path(temp_dir, "soméoddname.rdi")),
    read_rdi(file)
  )

  unlink(temp_dir)
})

test_that("read_rdi_internal() aligns with results from oce::read.adp.rdi()", {
  file <- system.file("extdata/19101018.rdi", package = "readrdi")
  # debug(oce:::decodeHeaderRDI)
  # oce_rdi <- oce::read.adp.rdi(file)

  rdi <- read_rdi_internal(file, offset = 0)

  # pick values towards the end of the structs
  # that are likely to be misaligned if any error occurred

  # fixed leader

  expect_identical(
    rdi$header$data_offset[[1]],
    c(20L, 79L, 144L, 346L, 448L, 550L, 652L)
  )

  expect_identical(
    rdi$fixed_leader$cpu_board_serial_number[[1]],
    "3a.00.00.02.80.86.01.09"
  )

  expect_identical(
    rdi$fixed_leader$serial_number,
    9088L
  )

  # variable leader

  expect_identical(
    rdi$variable_leader$transducer_depth,
    61.3
  )

  expect_identical(
    rdi$variable_leader$contamination_sensor,
    as.raw(159L)
  )

  expect_identical(
    rdi$variable_leader$pressure,
    61.535
  )

  # bottom track

  expect_identical(
    rdi$bottom_track$bottom_track_velocity[[1]],
    c(-0.357, -0.279, 0.006, -0.001)
  )

  expect_identical(
    rdi$bottom_track$bc[[1]],
    c(254L, 254L, 255L, 254L)
  )

  expect_identical(
    rdi$bottom_track$ba[[1]],
    c(78L, 79L, 82L, 76L)
  )

  expect_identical(
    rdi$bottom_track$bg[[1]],
    c(0L, 0L, 0L, 100L)
  )

  expect_identical(
    rdi$bottom_track$range_lsb[[1]],
    c(6179L, 6082L, 6106L, 6130L)
  )

  expect_identical(
    rdi$bottom_track$range_msb[[1]],
    c(0L, 0L, 0L, 0L)
  )

  # velocity

  # (note transposed relative to oce_rdi@data$[v, q, g, ])
  expect_identical(
    rdi$velocity$velocity[[1]][, 25],
    c(-0.147, -0.039, 0.014, NA)
  )

  expect_identical(
    rdi$correlation$correlation[[1]][, 25],
    as.raw(c(75L, 56L, 50L, 73L))
  )

  expect_identical(
    rdi$echo_intensity$echo_intensity[[1]][, 25],
    as.raw(c(91L, 82L, 86L, 91L))
  )

  expect_identical(
    rdi$pct_good$pct_good[[1]][, 25],
    as.raw(c(18L, 0L, 81L, 0L))
  )
})

test_that("read_rdi_internal() errors when passed an invalid offset", {
  file <- system.file("extdata/19101018.rdi", package = "readrdi")
  expect_error(
    read_rdi_internal(file, offset = 1),
    "Expected 0x7f7f"
  )
})
