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

test_that("base_process works", {
  tmp <- data.frame(V1 = c("3   4", "4   3", "2   5", "1   3", "3   9", "3   3"))
  result <- base_01_process(tmp)

  expect_equal(ncol(result), 2)

})

test_that("base_distance is calculated correctly", {
  tmp <- structure(c(3, 4, 2, 1, 3, 3, 4, 3, 5, 3, 9, 3), dim = c(6L,
                                                                  2L))
  result <- base_01_distance(tmp)

  expect_equal(result, 11)
})

test_that("base_similarity is calculated correctly", {
  tmp <- structure(c(3, 4, 2, 1, 3, 3, 4, 3, 5, 3, 9, 3), dim = c(6L,
                                                                  2L))
  result <- base_01_similarity(tmp)

  expect_equal(result, 31)
})


test_that("day 1 data.table and base functions work correctly", {
  tmp <- structure(c(3, 4, 2, 1, 3, 3, 4, 3, 5, 3, 9, 3), dim = c(6L,
                                                                  2L))
  data_table_tmp <- setDT(data.frame(V1 = tmp[, 1], V2 = tmp[, 2]))


  base_distance <- base_01_distance(tmp)
  data_table_distance <- dt_01_distance(data_table_tmp)

  expect_equal(base_distance, data_table_distance)

  base_similarity <- base_01_similarity(tmp)
  data_table_similarity <- dt_01_similarity(data_table_tmp)

  expect_equal(base_similarity, data_table_similarity)
})
