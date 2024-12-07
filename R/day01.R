#' Day 01 Solutions: Historian Hysteria
#'
#' Functions to solve Day 01 puzzles.
#'
#' @section Base R Functions:
#' - `base_01_input()`: Reads the input file.
#' - `base_01_distance()`: Computes the total distance.
#' - `base_01_similarity()`: Computes the similarity score.
#'
#' @section data.table Functions:
#' - `dt_01_input()`: Reads the input file using data.table.
#' - `dt_01_distance()`: Computes the total distance using data.table.
#' - `dt_01_similarity()`: Computes the similarity score using data.table.
#'
#' @examples
#' # Example file path
#' example_file <- tempfile()
#' writeLines(c("3   4", "4   3", "2   5", "1   3", "3   9", "3   3"), example_file)
#'
#' # Base R Workflow
#' base_data <- base_01_input(example_file)
#' processed_data <- base_01_process(base_data)
#' base_01_distance(processed_data) # Expected result: 11
#' base_01_similarity(processed_data) # Expected result: 31
#'
#' # data.table Workflow
#' dt_data <- dt_01_input(example_file)
#' dt_01_distance(dt_data) # Expected result: 11
#' dt_01_similarity(dt_data) # Expected result: 31
#'
#' unlink(example_file)
#'
#' @name day01
NULL

# Base R Functions -------------------------------------------------------------

#' @rdname day01
#' @export
base_01_input <- function(x) {
  if (!file.exists(x)) stop("File does not exist.")
  read.delim(x, header = FALSE)
}

#' @rdname day01
#' @export
base_01_process <- function(x) {
  x[, 1] |>
    strsplit("\\s+") |>
    lapply(as.numeric) |>
    do.call(rbind, args = _)
}

#' @rdname day01
#' @export
base_01_distance <- function(x) {
  (sort(x[, 1]) - sort(x[, 2])) |>
    abs() |>
    sum()
}

#' @rdname day01
#' @export
base_01_similarity <- function(x) {
  nbins <- max(x)
  a <- tabulate(x[, 1], nbins)
  b <- tabulate(x[, 2], nbins)
  sum(a * b * seq_len(nbins))
}

# data.table Functions ---------------------------------------------------------

#' @rdname day01
#' @import data.table
#' @export
dt_01_input <- function(x) fread('inst/input01.txt', header = FALSE)

#' @rdname day01
#' @export
dt_01_distance <- function(x){
  sum(abs(sort(x[[1]]) - sort(x[[2]])))
}

#' @rdname day01
#' @export
dt_01_similarity <- function(x) {
  CJ(dat$V1, dat$V2)[
    V1 == V2,
    .(n = .N, weighted_sum = V1 * .N),
    by = V1
  ][
    ,
    sum(weighted_sum)
  ]

}
