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

x <- read.table("testcases/10.txt")
x <- x[, 1]

print("first, n=10")
a <- countComparisons(x, pivot.strategy="first")
print(a[[1]])
validate(a[[2]])

print("last, n=10")
a <- countComparisons(x, pivot.strategy="last")
print(a[[1]])
validate(a[[2]])

print("median.of.three, n=10")
a <- countComparisons(x, pivot.strategy="median.of.three")
print(a[[1]])
validate(a[[2]])

c <- read.table("testcases/100.txt")
c <- c[, 1]

print("first, n=100")
a <- countComparisons(c, pivot.strategy="first")
print(a[[1]])
validate(a[[2]])

print("last, n=100")
a <- countComparisons(c, pivot.strategy="last")
print(a[[1]])
validate(a[[2]])

print("median.of.three, n=100")
a <- countComparisons(c, pivot.strategy="median.of.three")
print(a[[1]])
validate(a[[2]])

m <- read.table("testcases/1000.txt")
m <- m[, 1]

print("first, n=1000")
a <- countComparisons(m, pivot.strategy="first")
print(a[[1]])
validate(a[[2]])

print("last, n=1000")
a <- countComparisons(m, pivot.strategy="last")
print(a[[1]])
validate(a[[2]])

print("median.of.three, n=1000")
a <- countComparisons(m, pivot.strategy="median.of.three")
print(a[[1]])
validate(a[[2]])
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

x <- read.table("IntegerArray.txt")
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
