integers <- read.table("IntegerArray.txt")
v <- integers[, 1]

source("count_inversions.R")
print(system.time(x <- countInversions(v)))

print(x[[2]])
print(x[[1]][1:100])