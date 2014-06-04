countComparisons <- function(v, pivot.strategy="median.of.three") {
  k <- 0
  i <- NULL
  
  swap <- function(i, j) {
    temp <- v[i]
    v[i] <<- v[j]
    v[j] <<- temp
  }
  
  partition <- function(l, r) {
    k <<- k + r - l
    if ((r - l) < 3) {
      return()
    }
    p <- v[l]
    i <<- l + 1
    for (j in (l + 1):r) {
      if (v[j] < p) {
        swap(i, j)
        i <<- i + 1
      }
    }
    swap(l, i - 1)
    partition(l, i - 2)
    partition(i, r)
  }
  
  qsort <- function(l, r) {
    n = r - l + 1
    pi <- NULL
    pivot <- NULL
    
    middleIndex <- function() {
      if (is.even(n)) {
        n / 2
      } else {
        (n + 1) / 2
      }
    }
    
    choosePivot <- function() {
      if (pivot.strategy == "median.of.three" && n > 2) {
        mi <- middleIndex()
        first <- v[l]
        middle <- v[mi]
        last <- v[r]
        pivot <<- median(c(first, middle, last))
        if (pivot == first) {
          pi <<- l
        } else if (pivot == middle) {
          pi <<- mi
        } else {
          pi <<- r
        }
      } else if (pivot.strategy == "last") {
        pi <<- r
        pivot <<- v[r]
      } else {
        pi <<- l
        pivot <<- v[l]
      }
    }
    
    if (n > 1) {
      #k <<- k + n - 1
      choosePivot()
      if (pi > l) {
        swap(l, pi)
      }
      partition(l, r)
    }
  }
  
  qsort(1, length(v))
  list(k, v)
}

is.even <- function(x) x %% 2 == 0
