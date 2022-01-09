f_exp <- expression(
    (1.4^((x/3)^3)*2.4*(x-10)/3)/2.6^(x/3)
)
f <- function(x) eval(f_exp)
phi_values <- c(1, 1)

phi <- function(n) {
  if (length(phi_values) >= n) {
    return(phi_values[n])
  } else {
    for (i in seq(length(phi_values), n + 1)) {
      phi_values <<- c(phi_values,
                       phi_values[i - 2]
                       + phi_values[i - 1])
    }
    return(phi_values[n])
  }
}
fib_k <- function(lower, upper, tol) {
  i <- 2
  threshold <- (upper - lower)/tol
  while (phi(i) < threshold) {
    i <- i + 1
  }
  return(i + 1)
}
fibonacci <- function(f, lower, upper, tol,
                      max = FALSE, logging = FALSE, plotting = FALSE,
                      ylim = c(-10, 10), it_lim = -1) {
  if (max) f <- function(x) -f(x)
  k <- fib_k(lower, upper, tol)
  range_len <- abs(upper - lower)
  plot_bot <- lower - 0.05 * range_len
  plot_top <- upper + 0.05 * range_len
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
    if (logging) cat("[", i, "] c=", c, ";\td=", d, "\n", sep = "")
    if (plotting) {
      plot(f, plot_bot, plot_top, ylim = ylim, col = "red")
      points(c, f(c), col = "green")
      points(d, f(d), col = "blue")
    }
    if (it_lim > 0 && i > it_lim) return(c(plot_bot, plot_top, lower, c, d, upper))}
  return((lower + upper) / 2)
}
fibonacci(f, 2, 10, 1e-10)
