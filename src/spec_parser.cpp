
#include <Rcpp.h>
#include <string>
using namespace Rcpp;
using namespace std;

namespace {
  const char* format_spec;
  int pos, end;
  
  int get_integer(int& result) {
    int num_digits = 0, digitval;
    
    for (result = 0; pos < end; pos++, num_digits++) {
      digitval = format_spec[pos] - '0';
      if (digitval < 0 || digitval > 10) 
        break;
      result = result*10 + digitval;
    }
    
    return num_digits;
  }
  
  char count_utf8(const char *str) {
    unsigned char mask[] = {192, 224, 240};
    char i = 1;

    if ((*str & mask[0]) == mask[0]) i++;
    if ((*str & mask[1]) == mask[1]) i++;
    if ((*str & mask[2]) == mask[2]) i++;
    
    return i;
  }
  
  inline bool is_alignment_token(char c) {
      switch (c) {
      case '<': case '>': case '=': case '^':
          return 1;
      default:
          return 0;
      }
  }
  
  inline bool is_sign_element(char c) {
      switch (c) {
      case ' ': case '+': case '-':
          return 1;
      default:
          return 0;
      }
  }
}

//' @export
// [[Rcpp::export]]
List pformat_parse_spec(String& format) {
  List spec = List::create(
    Named("fill") = " ",
    Named("align") = "",
    Named("alternate") = false,
    Named("sign") = "",
    Named("width") = -1,
    Named("thousands_separator") = false,
    Named("precision") = -1,
    Named("type") = ""
  );
  
  spec.attr("class") = "pformat_spec";
  
  cetype_t encoding = format.get_encoding();
  format_spec = format.get_cstring();
  pos = 0; 
  end = strlen(format_spec);

  bool align_specified = false, fill_specified = false;
  char count;
    
  // treats the case where the fill char is utf8
  if (encoding == CE_UTF8 && end-pos >= 3 && 
      (count = count_utf8(format_spec + pos)) > 1 && 
      is_alignment_token(format_spec[pos+count])) {
    spec["align"] = format_spec[pos+count];
    spec["fill"] = String(string(format_spec + pos, format_spec + pos + count));
    align_specified = true;
    fill_specified = true;
    pos += count + 1;
  } // if the second char is an alignment token, parse the fill char
  else if (end-pos >= 2 && is_alignment_token(format_spec[pos+1])) {
    spec["align"] = format_spec[pos+1];
    spec["fill"] = format_spec[pos];
    align_specified = true;
    fill_specified = true;
    pos += 2;
  } else if (end-pos >= 1 && is_alignment_token(format_spec[pos])) {
    spec["align"] = format_spec[pos++];
    align_specified = true;
  }
  
  // parse the sign options
  if (end-pos >= 1 && is_sign_element(format_spec[pos])) 
    spec["sign"] = format_spec[pos++];
  
  // if the next character is #, we're in alternate mode (only integers)
  if (end-pos >= 1 && format_spec[pos] == '#') {
    spec["alternate"] = true;
    pos++;
  }
  
  // backwards compatibility
  if (!fill_specified && end-pos >= 1 && format_spec[pos] == '0') {
    spec["fill"] = '0';
    if (!align_specified) 
      spec["align"] = '=';
    pos++;
  }
  
  // parse width
  int result;
  if (get_integer(result) > 0)
    spec["width"] = result;

  // parse the thousands separator
  if (end-pos >= 1 && format_spec[pos] == ',') {
    spec["thousands_separator"] = true;
    pos++;
  }
  
  // parse field precision
  if (end-pos >= 1 && format_spec[pos] == '.') {
    pos++;
    
    if (get_integer(result) == 0)
      stop("Format specifier missing precision");
    else
      spec["precision"] = result;
  }
  
  // more than one char remaining, invalid format specifier
  if (end-pos >= 2)
    stop("Invalid format specifier");
  
  spec["type"] = format_spec[pos];
  
  if (spec["thousands_separator"] && !(
    format_spec[pos] == '\0' || format_spec[pos] == 'd' ||
    format_spec[pos] == 'e' || format_spec[pos] == 'f' ||
    format_spec[pos] == 'g' || format_spec[pos] == 'E' ||
    format_spec[pos] == 'G' || format_spec[pos] == '%' ||
    format_spec[pos] == 'F'
  ))
    stop(string("Cannot specify ',' with '") + format_spec[pos] + "'.");
  
  return spec;
}
