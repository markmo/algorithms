#Pkg.add("Benchmark")

using Benchmark

integers = readcsv("IntegerArray.txt", Int)
include("/Users/markmo/src/coursera/Algorithms/InversionCountJulia/count_inversions.jl")
a, n = countinversions(integers)
println(a[1:100])
println(n)

f1() = countinversions(integers)
b = benchmark(f1, "Count Inversions", "countinversions(integers)", 10)
println(b["AverageWall"])
