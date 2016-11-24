
#' Preparses a format string
#' 
#' @description  Parses a format string into an internal representation 
#' compatible with \code{pformat()}.
#'
#' @param format_string the format string
#'
#' @return an object of class \code{pformat.compiled} containing an internal
#' representation of the format string.
#' @export
#'
#' @details This function actually only parses a "layer" of the format string
#' markup. Recursive format strings depend on data, and thus can't be
#' completely parsed yet. In these cases, \code{pformat_parse()} will still be 
#' called by \code{pformat()} with format specifications needing expanding.
#'
#' @examples
pformat_parse = function(format_string) {
  
  it = list(start = 1, end = nchar(format_string))
  
  move = function(n = 1) {
    it$start <<- it$start + n
  }
  
  new_range = function(start = 1, end = 1) {
    list(start = start, end = end)
  }
  
  current_char = function() {
    substr(format_string, it$start, it$start)
  }
  
  piece = function(r) {
    if (is.null(r))
      return(NULL)
    else
      return(substr(format_string, r$start, r$end))
  }
  
  # this function is a rewriting of cpython's parse_field()
  # located on /Objects/stringlib/unicode_format.h
  parse_field = function() {
    obj = list(
      result = 1,
      field_name_it = NULL,
      conversion = NULL,
      format_spec_it = NULL,
      format_spec_needs_expanding = FALSE
    )
    
    bracket_count = 0
    start = it$start
    
    while (it$start <= it$end) {
      c = current_char()
      move()
      
      if ( c == "{" ) {
        stop("unexpected '{' in field name")
        return(list(result = 0))
      }
      
      if ( c == "[")
        bracket_count = bracket_count + 1
      
      if ( c == "]")
        bracket_count = bracket_count - 1
      
      if (bracket_count == 0 && c %in% c("}", ":", "!"))
        break;
    }
    
    obj$field_name_it = new_range(start, it$start - 2)
    
    if (c == "!" | c == ":") {
      if (c == "!") {
        
        if (it$start > it$end) {
          stop("end of string while looking for conversion specifier")
          return(list(result = 0))
        }
        
        obj$conversion = current_char()
        move()
        
        if (it$start <= it$end) {
          c = current_char()
          move()
          
          if (c == "}")
            return(obj)
          if (c != ":") {
            stop("expected ':' after conversion specifier")
            return(list(result = 0))
          }
        }
      }
      
      start = it$start
      braces_count = 1;
      
      while (it$start <= it$end) {
        c = current_char()
        move()
        
        if (c == "{") {
          obj$format_spec_needs_expanding = TRUE
          braces_count = braces_count + 1
        } else if ( c == "}") {
          braces_count = braces_count - 1
          
          if (braces_count == 0) {
            obj$format_spec_it = new_range(start, it$start - 2)
            return(obj)
          }
        }
      }
      
      stop("unmatched '{' in format spec")
      return(list(result = 0))
    } else if ( c != "}") {
      stop("expected '}' before end of string")
      return(list(result = 0))
    }
    
    return (obj)
  }
  
  # this function is a rewriting of cpython's MarkupIterator_next()
  # located on /Objects/stringlib/unicode_format.h
  MarkupIterator_next = function() {
    obj = list(
      literal_text = NULL
    )
    
    markup_follows = FALSE
    
    if (it$start > it$end)
      return(NULL);
    
    start = it$start
    
    while (it$start <= it$end) {
      c = current_char()
      move()
      
      if (c == "{" || c == "}") {
        markup_follows = TRUE
        break
      }
    }
    
    at_end = it$start > it$end
    len = it$start - start
    
    if ((c == "}") && (at_end | c != current_char())) {
      stop("Single '}' encountered in format string")
      return(NULL)
    }
    
    if (at_end && c == '{') {
      stop("Single '{' encountered in format string")
      return(NULL)
    } 
    
    if (!at_end) {
      if (c == current_char()) {
        move()
        markup_follows = FALSE
      } else 
        len = len - 1
    }
    
    obj$literal_text = substr(format_string, start, start + len - 1)
    
    if (!markup_follows)
      return(obj)
    
    obj2 = parse_field()
    
    if (obj2$result == 0)
      return(obj2)
    
    obj$field_name = piece(obj2$field_name_it)
    obj$format_spec = piece(obj2$format_spec_it)
    if ( is.null(obj$format_spec) )
      obj$format_spec = ""
    obj$format_spec_needs_expanding = obj2$format_spec_needs_expanding
    obj$conversion = obj2$conversion
    return(obj)
  }
  
  l = list()
  
  while (!is.null(obj <- MarkupIterator_next())) {
    l = c(l, list(obj))
  }
  
  class(l) = c("pformat.compiled", "list")
  
  return(l)
}

# This function is a rewriting of cpython's parse_internal_render_format_spec(),
# located on /Python/formatter_unicode.c
.parse_format_spec = function(format_spec) {
  
  start = 1
  end = nchar(format_spec)
  pos = start
  
  spec = list(
    fill = ' ',
    align = '',
    alternate = FALSE,
    sign = '',
    width = -1,
    thousands_separators = FALSE,
    precision = -1,
    type = ''
  )
  
  class(spec) = c("pformat.spec", "list")
  
  char_at = function(i) substr(format_spec, i, i)
  
  get_integer = function() {
    accumulator = 0L
    num_digits = 0
    
    while (pos <= end) {
      
      digitval = switch(char_at(pos),
                        "0" = 0L, "1" = 1L, "2" = 2L, "3" = 3L, "4" = 4L,
                        "5" = 5L, "6" = 6L, "7" = 7L, "8" = 8L, "9" = 9L)
      if (is.null(digitval))
        break
      
      # detect possible overflow before ir happens:
      # accumulator * 10 + digitval > .Machine$integer.max if and only if
      # accumulator > (.Machine$integer.max - digitval)/10
      
      if (accumulator > (.Machine$integer.max - digitval)/10) {
        stop("Too many decimal digits in format string")
        return(NULL)
      }
      
      accumulator = accumulator*10 + digitval
      
      pos <<- pos + 1
      num_digits = num_digits + 1
    }
    
    return(list(result = accumulator, num_digits = num_digits))
  }
  
  align_specified = FALSE
  fill_specified = FALSE
  
  # if the second char is an alignment token, parse the fill char
  if (end-pos >= 1 && char_at(pos+1) %in% c("<", ">", "=", "^")) {
    spec$align = char_at(pos+1)
    spec$fill = char_at(pos)
    align_specified = TRUE
    fill_specified = TRUE
    pos = pos + 2
  } else if (end-pos >= 0 && char_at(pos) %in% c("<", ">", "=", "^")) {
    spec$align = char_at(pos)
    align_specified = TRUE
    pos = pos + 1
  }
  
  # parse the sign options
  if (end-pos >= 0 && char_at(pos) %in% c("+", "-", " ")) {
    spec$sign = char_at(pos)
    pos = pos + 1
  }
  
  # if the next character is #, we're in alternate mode (only integers)
  if (end-pos >= 0 && char_at(pos) == "#") {
    spec$alternate = TRUE
    pos = pos + 1
  }
  
  # backwards compatibility
  if (!fill_specified && end-pos >= 0 && char_at(pos) == "0") {
    spec$fill = "0"
    if (!align_specified)
      spec$align = "="
    pos = pos + 1
  }
  
  # parse width
  int = get_integer()
  if (is.null(int))
    return(NULL)
  
  if (int$num_digit == 0)
    spec$width = -1
  else
    spec$width = int$result
  
  # parse the thousands separator
  if (end-pos >= 0 && char_at(pos) == ",") {
    spec$thousands_separators = TRUE
    pos = pos + 1
  }
  
  # parse field precision
  if (end-pos >= 0 && char_at(pos) == ".") {
    pos = pos + 1
    int = get_integer()
    
    if (is.null(int))
      return(NULL)
    
    # not having a precision after a dot is an error
    if (int$num_digits == 0) {
      stop("Format specifier missing precision")
      return(NULL)
    } else {
      spec$precision = int$result
    }
  }
  
  if (end-pos >= 1) {
    # more than one char remain, invalid format specifier
    stop("Invalid format specifier")
    return(NULL)
  }
  
  spec$type = char_at(pos)
  
  if (spec$thousands_separators && 
      !(spec$type %in% c("", "d", "e", "f", "g", "E", "G", "%", "F")) ) {
    stop(sprintf("Cannot specify ',' with '%s'.", spec$type))
    return(NULL)
  }
  
  return(spec)
}