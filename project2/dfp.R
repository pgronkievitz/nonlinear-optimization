f <- function(x) (x[1]^2+1)*(x[2]^4+1)-5-x[1]-x[2]+0.5*x[2]^2
library("numDeriv")
new_B <- function(B, dx, y) {
  I <- diag(dim(B)[[1]])
  yx <- (t(y)%*%dx)[[1]]
  a_ <- y%*%t(dx)
  b_ <- dx%*%t(y)
  c_ <- y%*%t(y)
  return(
   (I-a_/yx) %*% B %*% (I-b_/yx) + c_/yx
  )
}
golden <- function(f, lower, upper, tol) {
    ratio <- 2 / (3 + sqrt(5))
    x1 <- (1 - ratio) * lower + ratio * upper
    f.x1 <- f(x1)
    while (abs(upper - lower) > 2 * tol) {
        x2 <- (1 - ratio) * x1 + ratio * upper
        f.x2 <- f(x2)
        if (f.x1 < f.x2) {
            upper <- lower
            lower <- x2
        } else {
            lower <- x1
            x1 <- x2
            f.x1 <- f.x2
        }
    }
    return((upper + lower) / 2)
}

dfp <- function(f, x, eps,
                logging = F, plotting = F, xlim=c(-1,1), ylim=c(-1,1), res = 60) {
  x <- matrix(x)
  B <- diag(dim(x)[1])
  dx <- matrix(c(Inf,Inf))
  i<- 0
  if (plotting) {
    fu <- function(x, y) f(c(x, y))
    xs <- seq(xlim[1], xlim[2], length.out = res)
    ys <- seq(ylim[1], ylim[2], length.out = res)
    zs <- outer(xs, ys, fu)
  }
  repeat {
    i <- i+1
    H <- solve(B)
    gradfx <- grad(f, x)
    stepf <- function(step) f(x-step*gradfx)
    step <- golden(stepf, 0, 1/sqrt(sum(gradfx^2)), 1e-4)
    dx <- -H%*%gradfx*step
    B <- new_B(B, dx, grad(f,x+dx)-gradfx)
    x <- x + dx
    if (logging) {
      cat("[", i, "]", " x=[", x, "]", " Dx=[", dx, "]", " B=", B, "\n")
    }
    if (plotting) {
      filled.contour(xs, ys, zs, nlevels = 20)
      points(x)
    }
    if (sqrt(sum(dx^2)) < eps) return(x)
  }
}

dfp(f, c(1,1), 1e-6)
beale <- function(x) {
  el1 <- (1.5-x[1]+x[1]*x[2])^2
  el2 <- (2.25-x[1]+x[1]*x[2]^2)^2
  el3 <- (2.625-x[1]+x[1]*x[2]^3)^2
  return(el1+el2+el3)
}
dfp(beale, c(1,1), 1e-6)
