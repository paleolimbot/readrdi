
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
