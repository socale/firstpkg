#' @rdname mvnpdf
#' @export
mvnpdf_Paral <- function(x, mean =  rep(0, nrow(x)),
                   varcovM = diag(nrow(x)), Log = TRUE) {

  nbre=parallel::detectCores()
  ncores=nbre-1
  n <- ncol(x)
  p <- nrow(x)


  x0 <- x - mean
  Rinv <- solve(varcovM)
  LogDetvarcovM <- log(det(varcovM))


  cl=parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  vect_iter=itertools::isplitIndices(n=n,chunks=ncores)

  browser()

  y <- foreach(j=vect_iter, .combine='c') %dopar% {
    print(x0[, j, drop=FALSE])
    - p/2 * log(2*pi) - 0.5 * LogDetvarcovM -
      0.5 * t(x0[, j, drop=FALSE]) %*% Rinv %*% x0[, j, drop=FALSE]
  }

  if (!Log) {
    y <- exp(y)
  }

  parallel::stopCluster(cl)

  res <- list(x = x, y = y)
  class(res) <- "mvnpdf"
  return(res)
}
