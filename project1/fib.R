f <- function(x) (exp((x / 3)^3) * 1.36 * (x - 10) / 3) / 2.6^(x / 3)

phi_values <- c(0, 1)

phi <- function(n) {
  if (length(phi_values) >= n) {
    return(phi_values[n])
  } else {
    for (i in seq(length(phi_values), n + 1)) {
      phi_values <<- c(phi_values, phi_values[i - 2] + phi_values[i - 1])
    }
    return(phi_values[n])
  }
}

fib_k <- function(lower, upper, tol) {
  i <- 1
  l <- upper - lower
  while (phi(i) < l / tol) {
    i <- i + 1
  }
  return(i + 1)
}

fibonacci <- function(f, lower, upper, tol, max = FALSE, logging = FALSE, plotting = FALSE) {
  if (max) f <- function(x) -f(x)

  k <- fib_k(lower, upper, tol)
  c <- upper - phi(k - 1) / phi(k) * (upper - lower)
  d <- lower + upper - c
  for (i in seq(0, k - 4)) {
    if (f(c) < f(d)) {
      upper <- d
      i <- i + 1
    } else {
      lower <- c
      i <- i + 1
    }
    c <- upper - phi(k - 1) / phi(k) * (upper - lower)
    d <- lower + upper - c
    if (logging) cat("[", i, "] c=", c, ";\t d=", d, "\n", sep = "")
  }
  return((lower + upper) / 2)
}

fib_minima <- fibonacci(f, 2, 10, 1e-10, logging = TRUE)
sprintf("%.10f", fib_minima)
