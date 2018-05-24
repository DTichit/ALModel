// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// calcul_duration_obligation
NumericVector calcul_duration_obligation(NumericVector coupon, NumericVector mat_res, NumericVector valeur_remboursement, NumericVector yield);
RcppExport SEXP _SiALM_calcul_duration_obligation(SEXP couponSEXP, SEXP mat_resSEXP, SEXP valeur_remboursementSEXP, SEXP yieldSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type coupon(couponSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mat_res(mat_resSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type valeur_remboursement(valeur_remboursementSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yield(yieldSEXP);
    rcpp_result_gen = Rcpp::wrap(calcul_duration_obligation(coupon, mat_res, valeur_remboursement, yield));
    return rcpp_result_gen;
END_RCPP
}
// calcul_vm_obligation
NumericVector calcul_vm_obligation(NumericVector coupon, NumericVector mat_res, NumericVector valeur_remboursement, NumericVector spread, NumericVector yield);
RcppExport SEXP _SiALM_calcul_vm_obligation(SEXP couponSEXP, SEXP mat_resSEXP, SEXP valeur_remboursementSEXP, SEXP spreadSEXP, SEXP yieldSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type coupon(couponSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mat_res(mat_resSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type valeur_remboursement(valeur_remboursementSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type spread(spreadSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type yield(yieldSEXP);
    rcpp_result_gen = Rcpp::wrap(calcul_vm_obligation(coupon, mat_res, valeur_remboursement, spread, yield));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_SiALM_calcul_duration_obligation", (DL_FUNC) &_SiALM_calcul_duration_obligation, 4},
    {"_SiALM_calcul_vm_obligation", (DL_FUNC) &_SiALM_calcul_vm_obligation, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_SiALM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
