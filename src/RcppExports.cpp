// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ELtest
Rcpp::List ELtest(const Eigen::MatrixXd& x, const Eigen::MatrixXd& c, const Eigen::MatrixXd& lhs, const Eigen::VectorXd& rhs, const double threshold, const int maxit, const double abstol);
RcppExport SEXP _melt_ELtest(SEXP xSEXP, SEXP cSEXP, SEXP lhsSEXP, SEXP rhsSEXP, SEXP thresholdSEXP, SEXP maxitSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type c(cSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type lhs(lhsSEXP);
    Rcpp::traits::input_parameter< const Eigen::VectorXd& >::type rhs(rhsSEXP);
    Rcpp::traits::input_parameter< const double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< const int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< const double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(ELtest(x, c, lhs, rhs, threshold, maxit, abstol));
    return rcpp_result_gen;
END_RCPP
}
// el_mean
Rcpp::List el_mean(const Eigen::Map<Eigen::VectorXd>& theta, const Eigen::Map<Eigen::MatrixXd>& x, const int maxit, const double abstol);
RcppExport SEXP _melt_el_mean(SEXP thetaSEXP, SEXP xSEXP, SEXP maxitSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::VectorXd>& >::type theta(thetaSEXP);
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< const double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(el_mean(theta, x, maxit, abstol));
    return rcpp_result_gen;
END_RCPP
}
// pairwise
Rcpp::List pairwise(const Eigen::MatrixXd& x, const Eigen::MatrixXd& c, const int control, const int k, const double level, const bool interval, const std::string method, const int B, const int nthread, const bool progress, const double threshold, const int maxit, const double abstol);
RcppExport SEXP _melt_pairwise(SEXP xSEXP, SEXP cSEXP, SEXP controlSEXP, SEXP kSEXP, SEXP levelSEXP, SEXP intervalSEXP, SEXP methodSEXP, SEXP BSEXP, SEXP nthreadSEXP, SEXP progressSEXP, SEXP thresholdSEXP, SEXP maxitSEXP, SEXP abstolSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type c(cSEXP);
    Rcpp::traits::input_parameter< const int >::type control(controlSEXP);
    Rcpp::traits::input_parameter< const int >::type k(kSEXP);
    Rcpp::traits::input_parameter< const double >::type level(levelSEXP);
    Rcpp::traits::input_parameter< const bool >::type interval(intervalSEXP);
    Rcpp::traits::input_parameter< const std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< const int >::type B(BSEXP);
    Rcpp::traits::input_parameter< const int >::type nthread(nthreadSEXP);
    Rcpp::traits::input_parameter< const bool >::type progress(progressSEXP);
    Rcpp::traits::input_parameter< const double >::type threshold(thresholdSEXP);
    Rcpp::traits::input_parameter< const int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< const double >::type abstol(abstolSEXP);
    rcpp_result_gen = Rcpp::wrap(pairwise(x, c, control, k, level, interval, method, B, nthread, progress, threshold, maxit, abstol));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_melt_ELtest", (DL_FUNC) &_melt_ELtest, 7},
    {"_melt_el_mean", (DL_FUNC) &_melt_el_mean, 4},
    {"_melt_pairwise", (DL_FUNC) &_melt_pairwise, 13},
    {NULL, NULL, 0}
};

RcppExport void R_init_melt(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
