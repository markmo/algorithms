source("qsort2.R")

validate <- function(v) {
  valid <- T
  for (i in 1:length(v)) {
    if (v[i] != i) {
      valid <- F
      break
    }
  }
  if (valid) {
    #print("OK")
  } else {
    print("####### invalid!")
    #print(v)
  }
}

x <- read.table("QuickSort.txt")
x <- x[, 1]
n <- length(x)

print(paste("first, n=", n))
a <- countComparisons(x, pivot.strategy="first")
print(a[[1]])
validate(a[[2]])

print(paste("last, n=", n))
a <- countComparisons(x, pivot.strategy="last")
print(a[[1]])
validate(a[[2]])

print(paste("median.of.three, n=", n))
a <- countComparisons(x, pivot.strategy="median.of.three")
print(a[[1]])
validate(a[[2]])
