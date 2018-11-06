setwd(system.file("extdata", package = "fars"))

test_that("fars_map_state() works correctly", {
  prints_text(fars_map_state(18, 2015))
  expect_error(fars_map_state(52, 2015))
  expect_error(fars_map_state(1, 2016))
})


