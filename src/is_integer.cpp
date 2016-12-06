
#include <Rcpp.h>
using namespace Rcpp;

//' Tells if a string represents an integer
//' 
//' Checks if the characters in a string are in the interval '0' .. '9'
//' 
//' @param v a string vector
//' 
//' @return a logical vector 
//' @export
// [[Rcpp::export]]
LogicalVector is_integer(CharacterVector v) {
  int n, m = v.size();
  LogicalVector res(m);

  for (int i = 0; i < m; i++) {
    res[i] = true;
    
    n = v[i].size();
    for (int j = 0; j < n; j++) {
      if (j == n-1 && v[i][j] == 'L')
        break;
      
      if (v[i][j] < '0' || v[i][j] > '9') {
        res[i] = false;
        break;
      }
    }
  }
  
  return(res);
}