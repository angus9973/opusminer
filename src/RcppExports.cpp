// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "opusminer_types.h"
#include <Rcpp.h>

using namespace Rcpp;

// get_tids
Rcpp::XPtr< std::vector<tidset> > get_tids();
RcppExport SEXP opusminer_get_tids() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(get_tids());
    return rcpp_result_gen;
END_RCPP
}
// opusHelper
Rcpp::GenericVector opusHelper(Rcpp::GenericVector input, Rcpp::NumericVector k_, Rcpp::LogicalVector args);
RcppExport SEXP opusminer_opusHelper(SEXP inputSEXP, SEXP k_SEXP, SEXP argsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::GenericVector >::type input(inputSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type k_(k_SEXP);
    Rcpp::traits::input_parameter< Rcpp::LogicalVector >::type args(argsSEXP);
    rcpp_result_gen = Rcpp::wrap(opusHelper(input, k_, args));
    return rcpp_result_gen;
END_RCPP
}
