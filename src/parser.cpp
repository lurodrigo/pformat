
#include <Rcpp.h>
#include <string>
#include "utils.h"
using namespace Rcpp;
using namespace std;

namespace {
  cetype_t encoding;
  const char *format;
  int pos, end;

  struct SubString {
    int begin, end;
    string get() {
      return string(format + begin, format + end);
    }
  };
  
  struct Markup {
    Markup(bool isnull) {
      format_spec_needs_expanding = false;
      literal_text.begin = literal_text.end = end;
      field_name.begin = field_name.end = end;
      conversion = '\0';
      format_spec.begin = format_spec.end = end;
      null = isnull;
      has_markup = false;
    }
    
    List toList() {
      if (has_markup)
        return List::create(
          Named("literal_text") = String(literal_text.get(), encoding),
          Named("field_name") = String(field_name.get(), encoding),
          Named("conversion") = conversion,
          Named("format_spec") = format_spec.get(),
          Named("format_spec_needs_expanding") = format_spec_needs_expanding
        );
      else
        return List::create(
          Named("literal_text") = String(literal_text.get(), encoding), 
          Named("field_name") = R_NilValue,
          Named("conversion") = R_NilValue,
          Named("format_spec") = R_NilValue,
          Named("format_spec_needs_expanding") = format_spec_needs_expanding
        );
    }
    
    SubString literal_text;
    SubString field_name;
    bool has_markup;
    char conversion;
    SubString format_spec;
    bool format_spec_needs_expanding;
    bool null;
  };
  
  const Markup NULL_MARKUP = Markup(true);
  
  // this function is a rewriting of cpython's parse_field()
  // located on /Objects/stringlib/unicode_format.h
  void parse_field(Markup& field) {
    int bracket_count = 0;
    char ch;
    
    // matches the field name
    field.field_name.begin = pos;
    while (pos != end) {
      ch = format[pos];
      pos++;

      // if (ch == '{')
      //   stop("Unexpected '{' in field name");
      
      if (ch == '[' || ch == '(') bracket_count++;
      if (ch == ']' || ch == ')') bracket_count--;
      
      if (bracket_count == 0 && (ch == '}' || ch == ':' || ch == '!'))
        break;
    }
    field.field_name.end = pos-1;
    
    if (ch == '!' || ch == ':') {
      if (ch == '!') {
        if (pos == end) 
          stop("End of string while looking for conversion specifier");
        
        field.conversion = format[pos];
        pos++;
        
        if (pos != end) {
          ch = format[pos];
          pos++;
          
          if (ch == '}')
            return;
          if (ch != ':')
            stop("Expected ':' after conversion specifier");
        }
      }
      
      field.format_spec.begin = pos;
      int braces_count = 1;
      
      while (pos != end) {
        ch = format[pos];
        pos++;
        
        if (ch == '{') {
          field.format_spec_needs_expanding = true;
          braces_count++;
        } else if (ch == '}') {
          braces_count--;
          
          if (braces_count == 0) {
            field.format_spec.end = pos-1;
            return;
          }
        }
      }
      
      stop("Unmatched '{' in format spec");
    } else if (ch != '}') {
      stop("Expected '}' before end of string");
    }
  }
  
  // this function is a rewriting of cpython's MarkupIterator_next()
  // located on /Objects/stringlib/unicode_format.h
  Markup next() {
    Markup markup(false);
    char ch;
    
    if (pos == end)
      return NULL_MARKUP;
    
    markup.literal_text.begin = pos;
    
    while (pos != end) {
      ch = format[pos];
      pos++;
      
      if (ch == '{' || ch == '}') {
        markup.has_markup = true;
        break;
      }
    }
    
    markup.literal_text.end = pos;
    
    if ((ch == '}') && (pos == end || ch != format[pos]))
      stop("Single '}' encountered in format string");
    
    if (pos == end && ch == '{')
      stop("Single '{' encountered in format string");
    
    if (pos != end) {
      if (ch == format[pos]) {
        pos++;
        markup.has_markup = false;
      } else
        markup.literal_text.end--;
    } 
    
    if (markup.has_markup)
      parse_field(markup);
    
    return markup;
  }
}

//' Preparses a format string
//'
//' @description  Parses a format string into an internal representation
//' compatible with \code{pformat()}.
//'
//' @param format_string the format string
//'
//' @return an object of class \code{pformat_compiled} containing an internal
//' representation of the format string.
//' @export
//'
//' @details This function actually only parses a "layer" of the format string
//' markup. Recursive format strings depend on data, and thus can't be
//' completely parsed yet. In these cases, \code{pformat_parse()} will still be
//' called by \code{pformat()} with format specifications needing expanding.
//'
//' @examples
//' pformat_parse("{} {}")
//' pformat_parse("{1} {2}")
//' @export
// [[Rcpp::export]]
List pformat_parse(String& format_string) {
  encoding = format_string.get_encoding();
  format = format_string.get_cstring();
  pos = 0;
  end = strlen(format);
  
  List result;

  Markup m = Markup(false);
  while (!(m = next()).null) {
    result.push_back(m.toList());
  }
  
  result.attr("class") = "pformat_compiled";
  return result;
}