
test_that("unpack configuration works", {
  config <- rdi_unpack_system_config(51777)
  expect_identical(config$frequency, 300L)
  expect_identical(config$angle, 20L)
  expect_identical(config$beam_pattern, "convex")
  expect_identical(config$beam_config, "janus")
})


test_that("unpack configuration is vectorized", {
  expect_identical(
    nrow(rdi_unpack_system_config(rep(51777, 5))),
    5L
  )
})

test_that("unpack coord transform works", {
  coord <- rdi_unpack_coord_transform(0x17)
  expect_identical(coord$coord_system, "sfm")
  expect_identical(coord$tilt_used, TRUE)
  expect_identical(coord$bin_mapping_used, TRUE)
  expect_identical(coord$three_beam_used, TRUE)
})
