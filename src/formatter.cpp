
#include <Rcpp.h>
#include <string>
#include <typeinfo>
using namespace Rcpp;
using namespace std;

//' @export
// [[Rcpp::export]]
void teste(String& v) {
  char t = 127;
  t++;
  Rcout << (int) t;
}