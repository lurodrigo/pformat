
#include <Rcpp.h>
#include <string>
#include <list>
#include <cstdio>
using namespace Rcpp;
using namespace std;

struct SubString {
  string::iterator begin, end;
  string get() {
    return string(begin, end);
  }
};

struct Markup {
  Markup() {
    null = true;
  }
  
  Markup(string& str) {
    format_spec_needs_expanding = false;
    literal_text.begin = literal_text.end = str.end();
    field_name.begin = field_name.end = str.end();
    conversion = '\0';
    format_spec.begin = format_spec.end = str.end();
    null = false;
    has_markup = false;
  }
  
  List toList() {
    if (has_markup)
      return List::create(
        Named("literal_text") = literal_text.get(),
        Named("field_name") = field_name.get(),
        Named("conversion") = conversion,
        Named("format_spec") = format_spec.get(),
        Named("format_spec_needs_expanding") = format_spec_needs_expanding
      );
    else
      return List::create(
        Named("literal_text") = literal_text.get(),
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

const Markup NULL_MARKUP = Markup();

class Parser {
  string format_string;
  string::iterator it, str_end;
  
  // this function is a rewriting of cpython's parse_field()
  // located on /Objects/stringlib/unicode_format.h
  void parse_field(Markup& field) {
    int bracket_count = 0;
    char ch;
    
    // matches the field name
    field.field_name.begin = it;
    while (it != str_end) {
      ch = *it;
      it++;
      
      if (ch == '{')
        stop("Unexpected '{' in field name");
      
      if (ch == '[' || ch == '(') bracket_count++;
      if (ch == ']' || ch == ')') bracket_count--;
      
      if (bracket_count == 0 && (ch == '}' || ch == ':' || ch == '!'))
        break;
    }
    field.field_name.end = it-1;
    
    if (ch == '!' || ch == ':') {
      if (ch == '!') {
        if (it == str_end) 
          stop("End of string while looking for conversion specifier");
        
        field.conversion = *it;
        it++;
        
        if (it != str_end) {
          ch = *it;
          it++;
          
          if (ch == '}')
            return;
          if (ch != ':')
            stop("Expected ':' after conversion specifier");
        }
      }
      
      field.format_spec.begin = it;
      int braces_count = 1;
      
      while (it != str_end) {
        ch = *it;
        it++;
        
        if (ch == '{') {
          field.format_spec_needs_expanding = true;
          braces_count++;
        } else if (ch == '}') {
          braces_count--;
          
          if (braces_count == 0) {
            field.format_spec.end = it-1;
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
    Markup markup(format_string);
    char ch;
    
    if (it == str_end)
      return NULL_MARKUP;
    
    markup.literal_text.begin = it;
    
    while (it != str_end) {
      ch = *it;
      it++;
      
      if (ch == '{' || ch == '}') {
        markup.has_markup = true;
        break;
      }
    }
    
    markup.literal_text.end = it;
    
    if ((ch == '}') && (it == str_end || ch != *it))
      stop("Single '}' encountered in format string");
    
    if (it == str_end && ch == '{')
      stop("Single '{' encountered in format string");
    
    if (it != str_end) {
      if (ch == *it) {
        it++;
        markup.has_markup = false;
      } else
        markup.literal_text.end--;
    } 
    
    if (markup.has_markup)
      parse_field(markup);
    
    return markup;
  }
  
public:
  List parse(StringVector& v) {
    format_string = as<string>(v[0]);
    it = format_string.begin();
    str_end = format_string.end();
    
    List result;
    Markup m;
    while (!(m = next()).null) {
      result.push_back(m.toList());
    }
    
    return result;
  }
 
};

//' Preparses a format string
//'
//' @description  Parses a format string into an internal representation
//' compatible with \code{pformat()}.
//'
//' @param format_string the format string
//'
//' @return an object of class \code{pformat.compiled} containing an internal
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
List pformat_parse(StringVector format_string) {
  Parser p;
  return p.parse(format_string);
}

class SpecParser {
  string format_spec;
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
  
public:
  
  List parse(StringVector& v) {
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
    
    format_spec = as<string>(v[0]);
    pos = 0; 
    end = format_spec.size();
  
    bool align_specified = false, fill_specified = false;
    
    // if the second char is an alignment token, parse the fill char
    if (end-pos >= 2 && (
        format_spec[pos+1] == '<' || format_spec[pos+1] == '>' ||
        format_spec[pos+1] == '^' || format_spec[pos+1] == '=' )) {
      spec["align"] = format_spec[pos+1];
      spec["fill"] = format_spec[pos];
      align_specified = true;
      fill_specified = true;
      pos += 2;
    } if (end-pos >= 1 && (
        format_spec[pos] == '<' || format_spec[pos] == '>' ||
        format_spec[pos] == '^' || format_spec[pos] == '=' )) {
      spec["align"] = format_spec[pos];
      align_specified = true;
      pos++;
    }
    
    // parse the sign options
    if (end-pos >= 1 && (format_spec[pos] == '+' || format_spec[pos] == '-' ||
        format_spec[pos] == ' ')) 
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
};

//' @export
// [[Rcpp::export]]
List pformat_parse_spec(StringVector& v) {
  SpecParser p;
  return p.parse(v);
}
