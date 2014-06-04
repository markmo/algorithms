#library(futile.logger)
#flog.threshold(WARN)

# Counts the number of inversions in a vector.
#
# Args:
#   a - vector
#   n - length of vector
#
# Returns a list of the sorted vector [[1]] and the inversion count [[2]].
countInversions <- function(a, n=length(a)) {
  # count is 0 in base case where vector contains a single element
  #flog.trace("n: %s", n)
  if (n == 1) {
    list(a, 0)
  } else {

    # split the vector in two
    l <- as.integer(n / 2)
    #flog.trace("l: %s", l)
    m <- n - l
    
    # a recursive call to count the number of inversions in the left hand side
    # returns a list of the sorted vector [[1]] and the inversion count [[2]]
    x <- countInversions(a[1:l], l)
    #flog.trace("x: %s", x)
    
    # a recursive call to count the number of inversions in the right hand side
    # returns a list of the sorted vector [[1]] and the inversion count [[2]]
    y <- countInversions(a[(l + 1):n], m)
    #flog.trace("y: %s", y)
    
    # counts the number of inversions that are split across both sides
    # returns a list of the sorted vector [[1]] and the inversion count [[2]]
    z <- countSplitInversions(x[[1]], l, y[[1]], m, n)
    #flog.trace("z: %s", z)
    
    list(z[[1]], x[[2]] + y[[2]] + z[[2]])
  }
}

countSplitInversions <- function(b, blen, c, clen, n) {
  s = vector(length=n)  # the sorted combined vector
  x <- 0                # the inversion count
  i <- 1
  j <- 1
  for (k in 1:n) {
    if (i > blen) {
      s[k] <- c[j]
      j <- j + 1
    } else if (j > clen) {
      s[k] <- b[i]
      i <- i + 1
    } else if (b[i] < c[j]) {
      s[k] <- b[i]
      i <- i + 1
    } else {
      s[k] <- c[j]
      j <- j + 1
      x <- x + blen - i + 1
    }
  }
  list(s, x)
}