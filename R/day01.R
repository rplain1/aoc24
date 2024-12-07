library(data.table)

base_01_input <- function(x) {
  #TODO: add type check
  read.delim(x, header = FALSE)
}

base_01_helper <- function(x) {
  x[, 1] |>
    strsplit('\\s+') |>
    lapply(as.numeric) |>
    do.call(rbind, args = _)
}

# solution is 1388114
base_01_distance <- function(x) {
  (sort(x[, 1]) - sort(x[, 2])) |>
    abs() |>
    sum()
}

# solution is 23529853
base_01_similarity <- function(x) {

  nbins = max(x)
  a <- tabulate(x[, 1], nbins)
  b <- tabulate(x[, 2], nbins)
  sum(a * b * seq_len(nbins))
}

dt_01_input <- function(x) fread('inst/input01.txt', header = FALSE)

dt_01_distance <- function(x){
  sum(abs(sort(x[[1]]) - sort(x[[2]])))
}

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

input <- 'inst/input01.txt'
(
  dt_01_input(input) |>
    dt_01_distance()
) == (
  base_01_input(input) |>
    base_01_helper() |>
    base_01_distance()
)

(
  dt_01_input(input) |>
    dt_01_similarity()
) == (
  base_01_input(input) |>
    base_01_helper() |>
    base_01_similarity()
)



