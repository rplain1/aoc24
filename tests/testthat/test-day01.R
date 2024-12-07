test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("base_01_input reads file correctly", {
  # Create a temporary file
  temp_file <- tempfile()
  writeLines(c("3 4", "4 3", "2 5", "1 3", "3 9", "3 3"), temp_file)

  # Test if the function reads the file as expected
  result <- base_01_input(temp_file)
  expect_equal(nrow(result), 6)  # Expect 6 rows
  expect_equal(ncol(result), 1)  # Expect 2 columns

  unlink(temp_file)  # Clean up
})
