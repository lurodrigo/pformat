
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]

//' @export
// [[Rcpp::export]]
List markup() {
  List l = List::create(Named("result") = 1,
                        Named("field_name_it") = "",
                        Named("conversion") = "",
                        Named("format_spec_it") = "",
                        Named("format_spec_needs_expanding") = false);
  return l;
}

//' @export
// [[Rcpp::export]]
List pformat_parse2(StringVector str) {
  List a;
  
  a = markup();

  auto it = str[0].begin();
  for (; it < str[0].end(); it++) {
    Rcout << *it << "\n";
  }

  return a;
}