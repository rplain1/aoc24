base_02_input <- function(x) {
  readLines(x) |>
    str_split(' ') |>
    lapply(as.numeric)
}

is_safe <- function(report) {
  d <- diff(report)

  (all(d > 0) || all(d < 0)) && all(between(abs(d),1, 3))
}

is_safe2 <- function(report) {
  is_safe(report) || (
    any(map_lgl(seq_along(report), \(i) is_safe(report[-i])))
    )
}

b02_safe_a <- function(x) {
  lapply(x, is_safe2) |>
    do.call(rbind, args = _) |>
    sum()
}


b02_safe_b <- function(x) {
  lapply(x, is_safe2) |>
    do.call(rbind, args = _) |>
    sum()
}



