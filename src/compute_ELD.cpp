#include "utils.h"
#include "EL.h"
#include <RcppEigen.h>
#ifdef _OPENMP
#include <omp.h>
#endif
#include <string>
#include <vector>

// [[Rcpp::export]]
Rcpp::NumericVector compute_ELD(const std::string method,
                                const Eigen::Map<Eigen::VectorXd> &par0,
                                const Eigen::Map<Eigen::MatrixXd> &x,
                                const int maxit_l,
                                const double tol_l,
                                const Rcpp::Nullable<double> th,
                                const int nthreads,
                                const Eigen::Map<Eigen::ArrayXd> &wt)
{
  const int n = x.rows();
  const int p = par0.size();
  const double test_th = set_threshold(p, th);
  const EL el(method, par0, x, maxit_l, tol_l, test_th, wt);
  // maximum empirical likelihood value
  const double mel = el.loglik();
  const Eigen::ArrayXd w = el.w;
  const bool weighted = w.size() != 0;

  std::vector<double> eld(n);
  // #pragma omp parallel for
  for (int i = 0; i < n; ++i)
  {
    // leave-one-out matrix
    Eigen::MatrixXd loo_x(n - 1, x.cols());
    loo_x << x.topRows(i), x.bottomRows(n - 1 - i);
    // leave-one-out weight array
    Eigen::ArrayXd loo_w{};
    if (weighted)
    {
      loo_w.resize(n - 1);
      loo_w << w.topRows(i), w.bottomRows(n - 1 - i);
      loo_w = ((n - 1) / loo_w.sum()) * loo_w;
    }
    const EL loo_el(method, el.mele_fn(loo_x, loo_w), x, maxit_l, tol_l,
                    test_th, wt);
    eld[i] = 2 * n * (mel - loo_el.loglik());
  }
  const Rcpp::NumericVector out = Rcpp::wrap(eld);
  return out;
}
