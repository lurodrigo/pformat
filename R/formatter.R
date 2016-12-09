
#' Formats an object for pretty printing
#' 
#' @description Format an object according to a format specification
#'
#' @param value any R object (conceptually)
#' @param format_spec a character vector containing a specification
#'
#' @return the formatted string representing the value
#' @export
#' 
#' @details \code{pformatter} is a generic function
#'
#' @examples
#' pformatter(10L, "<0d")
pformatter = function(value, format_spec) {
  # return (as.character(value))
  UseMethod("pformatter", value)
}

#' @describeIn pformatter 
#' @export
pformatter.integer = function(value, format_spec) {
  if (inherits(format_spec, "pformat_spec"))
    spec = format_spec
  else
    spec = pformat_parse_spec(format_spec)
  
  if (spec$type %in% c("e", "E", "f", "F", "g", "G", "n", "%")) 
    return (pformatter.double(as.double(value), spec))
  
  value
}

#' @describeIn pformatter 
#' @export
pformatter.double = function(value, format_spec) {
  if (inherits(format_spec, "pformat_spec"))
    spec = format_spec
  else
    spec = pformat_parse_spec(format_spec)
  
  if (spec$type %in% c("b", "c", "d", "o", "x", "X", "n")) {
    return (pformatter.integer(as.integer(value), spec))
  }
  
  value
}

#' @describeIn pformatter 
#' @export
pformatter.complex = function(value, format_spec) {
  value
}

#' @describeIn pformatter 
#' @export
pformatter.character = function(value, format_spec) {
  spec = pformat_parse_spec(format_spec)
  
  if (!(spec$type %in% c("s", "")) ) {
    stop(sprintf("Invalid presentation type '%s' for character objects"),
         spec_type)
  } 
  
  spec$align = switch(spec$align, 
                      '<' = "left", 
                      '>' = "right", 
                      '^' = "centre")
  
  if (is.null(spec$align)) spec$align = "left"
  if (spec$width == -1) spec$width = NULL
  # return (format(value, width = spec$width, justify = spec$align))
  
  value
}

#' @describeIn pformatter 
#' @export
pformatter.Date = function(value, format_spec) {
  strftime(value, format_spec)
}

#' @describeIn pformatter 
#' @export
pformatter.IDate = function(value, format_spec) {
  strftime(value, format_spec)
}

#' @describeIn pformatter 
#' @export
pformatter.ITime = function(value, format_spec) {
  strftime(value, format_spec)
}

#' @describeIn pformatter 
#' @export
pformatter.POSIXt = function(value, format_spec) {
  strftime(value, format_spec)
}

#' @describeIn pformatter 
#' @export
pformatter.default = function(value, format_spec) {
  as.character(value)
}