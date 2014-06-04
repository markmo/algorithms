test.adjlist <- list(
  c(2, 3, 4, 7),
  c(1, 3, 4),
  c(1, 2, 4),
  c(1, 2, 3, 5),
  c(4, 6, 7, 8),
  c(5, 7, 8),
  c(1, 5, 6, 8),
  c(5, 6, 7))

test.adjlist2 <- list(
  c(2, 3),
  c(1, 3, 4),
  c(1, 2, 4),
  c(2, 3))

edges <- function(g) {
  x = c()
  for (a in 1:length(g)) {
    for (b in g[[a]]) {
      if (a < b) {
        x <- c(x, a, b)
      } else {
        x <- c(x, b, a)
      }
    }
  }
  dim(x) <- c(2, length(x) / 2)
  x <- t(x)
  x <- x[!duplicated(x), ]
  df <- data.frame(x)
  names(df) <- c("A", "B")
  df
}

mincut <- function(E) {
  print(E)
  s <- sample(nrow(E), 1)
  #print(s)
  e <- E[s, ]
  print(e)
  E <- E[-s, ]
  #print(E)
  if (nrow(E[E$A == e$B, ]) > 0) {
    E[E$A == e$B, ]$A <- e$A
  }
  if (nrow(E[E$B == e$B, ]) > 0) {
    E[E$B == e$B, ] <- e$A
  }
  E <- E[!duplicated(E) & E$A != E$B, ]
  print(E)
  V <- unique(c(E$A, E$B))
  if (length(V) > 2) {
    mincut(E)
  } else {
    E
  }
}