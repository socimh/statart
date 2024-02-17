test_that("numeric variable", {
  expect_equal(
    codebook(tibble::tibble(a = 1:10)),
    tibble::tibble(
      variable = "a",
      type = "integer",
      n = 10,
      unique = 10
    )
  )
})
