levenshtein_matrix<- function (a, b) {
  len_a = length(a)
  len_b = length(b)

  costs = matrix(NA, nrow=len_a + 1, ncol=len_b + 1)
  costs[1, ] = 0:(len_b)
  for (i in 2:(len_a + 1)) {
    costs[i, 1] = i - 1
    for (j in 2:(len_b + 1)) {
      tmp = ifelse(a[i-1] == b[j-1], costs[i-1, j-1], costs[i-1, j-1] + 1000)
      costs[i, j] = min(1 + min(costs[i-1, j], costs[i, j-1]), tmp)
    }
  }
  rownames(costs) <- c("", a)
  colnames(costs) <- c("", b)
  return(costs)
}

# piezzol p1i2z4z5a0
levenshtein_align <- function (a, b) {
  m <- levenshtein_matrix(a, b)
  n_r <- nrow(m)
  n_c <- ncol(m)
  i <- n_r
  j <- n_c
  align_out <- rep(NA, i-1)
  while (!(i==1 & j==1)) {
    cost <- m[i, j]
    # deletion
    if (i > 1) {
      up <- m[i - 1, j]
    } else {
      up <- cost + 1
    }
    # insertion
    if (j > 1) {
      left <- m[i, j - 1]
    } else {
      left <- cost + 1
    }
    # diag
    if (i > 1 & j > 1) {
      diag <- m[i - 1, j - 1]
    } else {
      diag <- cost + 1
    }
    if (diag == cost - 1) {
      align_out[i - 1] = j - 1
      i = i - 1
      j = j - 1
    } else if (left == cost - 1) {
      j = j - 1
    } else if (up == cost - 1) {
      i = i - 1
    } else if (diag == cost) {
      align_out[i - 1] = j - 1
      i = i - 1
      j = j - 1
    }
  }
  return(align_out)
}
