f_exp <- expression(
    (1.4^((x/3)^3)*2.4*(x-10)/3)/2.6^(x/3)
)
f <- function(x) eval(f_exp)
newton <- function(f_expr, x, tol, logging = FALSE, plotting = FALSE,
                   xlim = c(0, 10), ylim = c(-10, 10), it_lim = -1) {
  f <- function(x) eval(f_expr)
  df <- function(x) eval(D(f_expr, 'x'))
  d2f <- function(x) eval(D(D(f_expr, 'x'), 'x'))
  i <- 1
  repeat {
    h <- function(x) -df(x)/d2f(x)
    new.x <- x + h(x)
    if (logging) cat("[", i, "] x=", x, "\n", sep = "")
    if (plotting) {
      plot(f, xlim[[1]], xlim[[2]], ylim = ylim, col = "red")
      plot(parabole, xlim[[1]], xlim[[2]], ylim = ylim, col = "blue", add = TRUE)
    }
    i <- i + 1
    if (it_lim > 0 && i > it_lim)
      return(function(xh) f(x)+df(x)*(xh-x)+0.5*d2f(x)*(xh-x)^2)
    if (abs(new.x - x) < tol) return(new.x)
    x <- new.x
  }
}
newton(f_exp, 10, 1e-10)
