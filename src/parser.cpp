
#include <Rcpp.h>
#include <string>
#include <list>
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
  }
  
  List toList() {
    return List::create(
      Named("literal_text") = literal_text.get(),
      Named("field_name") = field_name.get(),
      Named("conversion") = conversion,
      Named("format_spec") = format_spec.get(),
      Named("format_spec_needs_expanding") = format_spec_needs_expanding
    );
  }
  
  SubString literal_text;
  SubString field_name;
  char conversion;
  SubString format_spec;
  bool format_spec_needs_expanding;
  bool null;
};

const Markup NULL_MARKUP = Markup();

class Parser {
  string format_string;
  string::iterator it, str_end;
  
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
  
  Markup next() {
    Markup markup(format_string);
    bool markup_follows = false;
    char ch;
    
    if (it == str_end)
      return NULL_MARKUP;
    
    markup.literal_text.begin = it;
    
    while (it != str_end) {
      ch = *it;
      it++;
      
      if (ch == '{' || ch == '}') {
        markup_follows = true;
        break;
      }
    }
    
    markup.literal_text.end = it-1;
    
    if ((ch == '}') && (it == str_end || ch != *it))
      stop("Single '}' encountered in format string");
    
    if (it == str_end && ch == '{')
      stop("Single '{' encountered in format string");
    
    if (it != str_end && ch == *it) {
        it++;
        markup.literal_text.end++;
        markup_follows = false;
    }
    
    if (markup_follows)
      parse_field(markup);
    
    return markup;
  }
};

//' @export
// [[Rcpp::export]]
List pformat_parse2(StringVector v) {
  Parser p;
  return p.parse(v);
}

