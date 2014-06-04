countComparisons <- function(v, pivot.strategy="median.of.three") {
  k <- 0
  qsort <- function(v) {
    n = length(v)
    if (n > 1) {
      k <<- k + n - 1
      # performing 3 scans through vector
      pivot = choosePivot(v, n, pivot.strategy)
      c(qsort(v[v < pivot]), v[v == pivot], qsort(v[v > pivot]))
    } else v
  }
  qsort(v)
  list(k, v)
}

qsort <- function(v, pivot.strategy="median.of.three") {
  n = length(v)
  if (n > 1) {
    pivot = choosePivot(v, n, pivot.strategy)
    # performing 3 scans through vector
    c(qsort(v[v < pivot]), v[v == pivot], qsort(v[v > pivot]))
  } else v
}

choosePivot <- function(v, n, pivot.strategy) {
  if (pivot.strategy == "first") {
    v[1]
  } else if (pivot.strategy == "last") {
    v[n]
  } else if (pivot.strategy == "median.of.three") {
    if (n < 3) {
      v[1]
    } else {
      median(c(v[1], middle(v, n), v[n]))
    }
  }
}

middle <- function(v, n = length(v)) {
  if (is.even(n)) {
    v[n / 2]
  } else {
    v[(n + 1) / 2]
  }
}

is.even <- function(x) x %% 2 == 0