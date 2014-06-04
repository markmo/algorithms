countComparisons <- function(v, pivot.strategy="median.of.three") {
  k <- 0
  
  swap <- function(i, j) {
    temp <- v[i]
    v[i] <<- v[j]
    v[j] <<- temp
  }
  
  partition <- function(l, r) {
    p <- v[l]
    i <- l + 1
    for (j in (l + 1):r) {
      #k <<- k + 1
      if (v[j] < p) {
        swap(i, j)
        i <- i + 1
      }
    }
    swap(l, i - 1)
    i - 1
  }
  
  qsort <- function(l, r) {
    #print(v)
    #print(paste("qsort from", l, "to", r, sep=" "))
    n = r - l + 1
    
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
        middle <- v[l + mi - 1]
        last <- v[r]
        pivot <- median(c(first, middle, last))
        if (pivot == first) {
          l
        } else if (pivot == middle) {
          l + mi - 1
        } else {
          r
        }
      } else if (pivot.strategy == "last") {
        r
      } else {
        l
      }
    }
    
    if (n > 1) {
      k <<- k + n - 1
      pi <- choosePivot()
      #print(pi - 1)
      if (pi > l) {
        swap(l, pi)
      }
      i <- partition(l, r)
      #print(i)
      if ((i - 1) > l) {
        qsort(l, i - 1)
      }
      if ((i + 1) < r) {
        qsort(i + 1, r)
      }
    }
  }
  
  qsort(1, length(v))
  list(k, v)
}

is.even <- function(x) x %% 2 == 0
