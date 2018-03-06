// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// etant donner que l'utilisateur n'a pas besoin de manipuler directement la fnction invC, je prefere ne pas l'exporter
// et du coup je mets en commentaire @export
////' @export
// [[Rcpp::export]]
arma::mat invC(arma::mat x) {
  arma::mat m = inv(x);
  return m;
}
