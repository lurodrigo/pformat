
#include <Rcpp.h>
#include <string>
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
    conversion.begin = conversion.end = str.end();
    format_spec.begin = format_spec.end = str.end();
    null = false;
  }
  
  SubString literal_text;
  SubString field_name;
  SubString conversion;
  SubString format_spec;
  bool format_spec_needs_expanding;
  bool null;
};

const Markup NULL_MARKUP = Markup();

class Parser {
  string format_string;
  string::iterator it, str_end;
  
public:
  void parse(StringVector& v) {
    format_string = as<string>(v[0]);
    it = format_string.begin();
    str_end = format_string.end();
  }
  
  void parse_field(Markup& field) {
    int bracket_count = 0;
    char ch;
    
    while (it != str_end) {
      ch = *it;
      it++;
      
      if (ch == '{') 
        stop("Unexpected '{' in field name");
        
      if (ch == '[') bracket_count++;
      if (ch == ']') bracket_count--;
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
    
    markup.literal_text.end = it;
    
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
void teste(CharacterVector v) {
  Rcout << "oi som\n";
  string str = as<std::string>(v)[0];
  string::reverse_iterator a, b;
  a = str.rend();
  b = str.rbegin();
  Rcout << string(a, b);
}

