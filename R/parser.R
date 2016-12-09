
# This function is a rewriting of cpython's parse_internal_render_format_spec(),
# located on /Python/formatter_unicode.c
parse_format_spec = function(format_spec) {
  
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