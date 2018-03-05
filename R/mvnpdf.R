#' Title: Densite d'une loi normale multivariees
#' Description: Cette fonction permet de calculer la densite d'une loi normale multivariee
#' Details: ceci precise plus de details sur la fonction
#' @param x matrice a n colonnes (observations) et p lignes
#' @param mean vecteur des moyennes
#' @param varcovM matrice des variances covariances
#' @param Log parametre logique parend par defaut la valeur NULL
#'
#' @return Cette fonction retourne en sortie la matrice x et la matrice y des images de x par la fonction
#' @export
#'
#' @examples
#' mvnpdf(x=matrix(1.96),Log=FALSE)
#' dnorm(1.96)
mvnpdf <- function(x, mean =  rep(0, nrow(x)),
                   varcovM = diag(nrow(x)), Log = TRUE) {
  n <- ncol(x)
  p <- nrow(x)
  x0 <- x - mean
  Rinv <- solve(varcovM)
  LogDetvarcovM <- log(det(varcovM))

  y <- NULL
  for (j in 1:n) {
    yj <- - p/2 * log(2*pi) - 0.5 * LogDetvarcovM -
      0.5 * t(x0[, j]) %*% Rinv %*% x0[, j]
    y <- c(y, yj)
  }

  if (!Log) {
    y <- exp(y)
  }

  return(list(x=x,y=y))
}

#' Plot of the mvnpdf function
#'
#' @param x an object of class \code{mvnpdf} resulting from a call of
#' \code{mnvpdf()} function.
#' @param ... graphical parameters passed to \code{plot()} function.
#'
#' @return Nothing is returned, only a plot is given.
#' @export
#' @importFrom graphics plot
#' @examples
#' pdfvalues <- mvnpdf(x=matrix(seq(-3, 3, by = 0.1), nrow = 1), Log=FALSE)
#' plot(pdfvalues)
plot.mvnpdf <- function(x, ...) {
  graphics::plot(x$x, x$y, type = "l", ...)
}

