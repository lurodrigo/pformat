
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
pformatter = function(value, format_spec) {
    # return (as.character(value))
    UseMethod("pformatter", value)
}

pformatter.integer = function(value, format_spec) {
    if ( inherits(format_spec, "pformat.spec"))
        spec = format_spec
    else
        spec = .parse_format_spec(format_spec)
    
    if (spec$type %in% c("e", "E", "f", "F", "g", "G", "n", "%")) 
        return (pformatter.double(as.double(value), spec))
}

pformatter.double = function(value, format_spec) {
    if ( inherits(format_spec, "pformat.spec"))
        spec = format_spec
    else
        spec = .parse_format_spec(format_spec)
    
    if (spec$type %in% c("b", "c", "d", "o", "x", "X", "n")) {
        return (pformatter.integer(as.integer(value), spec))
    }
    
}

pformatter.complex = function(value, format_spec) {
    "complex"
}

pformatter.character = function(value, format_spec) {
    spec = .parse_format_spec(format_spec)
    
    if ( !(spec$type %in% c("s", "")) ) {
        stop(sprintf("Invalid presentation type '%s' for character objects"),
             spec_type)
    } 
    
    spec$align = list('<' = "left", '>' = "right", '^' = "centre")[[spec$align]]
    
    if ( is.null(spec$align) ) spec$align = "left"
    if ( spec$width == -1 ) spec$width = NULL
    return (format(value, width = spec$width, justify = spec$align))
}

pformatter.Date = function(value, format_spec) {
    strftime(value, format_spec)
}

pformatter.POSIXt = function(value, format_spec) {
    strftime(value, format_spec)
}

pformatter.default = function(value, format_spec) {
    as.character(value)
}


