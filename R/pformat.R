
#' pformat: brings Python-inspired string interpolation and formatting
#' 
#' blablabla
#' 
#' @docType package
#' @name pformat
#' @useDynLib pformat
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

.repr = function (x) paste0(capture.output(dput(x)), collapse = "\n")

#' String formatting and interpolation
#' 
#' @description Perfoms string formatting and interpolation through
#' a format specification
#'
#' @param format_string a format string (See details for specification) or a
#' \code{pformat.compiled} object returned from \code{pformat_parse()}
#' 
#' @return returns a `character` vector with the output
#' @export
#' 
#' @details Numbered fields start from 1 (not from 0, as in Python.)
#' 
#' Non-named fields (manually or automatically numbered) are evaluated only over
#' the list of arguments passed. You can't write expressions involving non-named
#' fields. In Python, you can write \code{"{0.real}".format(1+2j)}
#' and it will return a string with the real part of the number. You can't do
#' this here, it's a design choice. Allowing it would imply limiting the 
#' kind of expressions one could use as a field, and expressions surely provide
#' more power than numbered fields.
#' 
#' On placeholders, pformat looks for names first on keyword arguments, then 
#' as a member of the with argument object, and only then at the caller's
#' environment.
#'
#' @examples
#' 
#' pformat("{} {}", "one", "two")
#' 
#' pformat("{2} {1}", "one", "two")
#' 
#' pformat("Name: {}; Age: {}", c("Abby", "Bob", "Carl"), 22:24)
pformat <- function(format_string, ...) {
  pargs = list(...)
  
  if (is.character(format_string)) {
    with = NULL
  } else {
    with = format_string
    format_string = pargs[[1]]
    pargs = pargs[2:length(pargs)]
  }
  
  res = .pformat(format_string, pargs, with, parent.frame(), 2)
  return(res$result)
}

#' Substitutes the values on the parsed format string
#'
#' @param pargs the args passed to pformat() (except `with`)
#' @param envir the caller's environment
#' @param recursion_depth the current recursion depth. Starts with 2.
#' @param auto_arg_index the index for non-named fields
#' @inheritParams pformat
#'
#' @return a list containing two fields
#' \describe{
#'     \item{result}{the output string}
#'     \item{index}{the value of auto_arg_index}
#' }
.pformat <- function(format_string, pargs, with, envir, recursion_depth, 
                     auto_arg_index = 1) {
  if (recursion_depth < 0) 
    stop("Max string recursion")
  
  # allows splitting format string over multiple lines
  if (is.character(format_string) && length(format_string) > 1) 
    format_string = paste0(format_string, collapse = "");
  
  with = as.list(with)
  for (name in names(pargs)) {
    with[[name]] = pargs[[name]]
  }
  
  get_field = function(field_name) {
    if (is_integer(field_name))
      return (pargs[[as.integer(field_name)]])
    else {
      # first, try to evaluate the expression using the arguments
      result = try({eval(parse(text = field_name), envir = with, 
                         enclos = envir)}, 
                   silent = TRUE)
      if ( !("try-error" %in% class(result)) ) {
        return (result)
      } 
      
      stop(sprintf("Could not evaluate field '%s'", field_name))
    }
  }
  
  if (inherits(format_string, "pformat_compiled"))
    parsed = format_string
  else
    parsed = pformat_parse(format_string)
  
  result = list()
  
  for (i in 1:length(parsed)) {
    # output the literal text
    if (parsed[[i]]$literal_text != "")
      result = c(result, list(parsed[[i]]$literal_text))
    
    # if there's a field, output it
    if (!is.null(parsed[[i]]$field_name) ) {
      # this is some markup, find the object and do the formatting
      
      # handle arg indexing when empty field_names are given
      if (parsed[[i]]$field_name == "") {
        if (auto_arg_index == 0)
          stop("Cannot switch from manual field specification to automatic field numbering")
        
        parsed[[i]]$field_name = as.character(auto_arg_index)
        auto_arg_index = auto_arg_index + 1
      } else if (is_integer(parsed[[i]]$field_name)) { 
        # is a string representing an integer
        if (auto_arg_index > 1)
          stop("Cannot switch from manual field specification to automatic field numbering")
        
        # disable auto arg incrementing, if it gets
        # used later on, then an exception will be raised
        auto_arg_index = 0
      }
      
      # given the field_name, find the object it references
      # and the argument it came from
      obj = get_field(parsed[[i]]$field_name)
      
      # do any conversion on the resulting object
      obj = convert_field(obj, parsed[[i]]$conversion)
      
      
      # expand the format spec, if needed
      if (parsed[[i]]$format_spec_needs_expanding) {
        rec = .pformat(parsed[[i]]$format_spec, pargs, with, envir, 
                       recursion_depth - 1, auto_arg_index)
        parsed[[i]]$format_spec = rec$result
        auto_arg_index = rec$index
      }
      
      # format the object and append to the result
      result = c(result, list(pformatter(obj, parsed[[i]]$format_spec)))
    }
  }
  
  return(list(result = do.call(paste0, result), index = auto_arg_index))
}

#' Prints formatted output
#' 
#' @description Just like pformat(), but prints to the standard output
#' @inheritParams pformat
#' @export
ppformat = function(format_string, ..., with = NULL) {
  cat(pformat(format_string, ..., with = with))
}

# do any conversion on the resulting object
convert_field = function(value, conversion) {
  if (conversion == "" || is.null(conversion))
    return(value)
  if (conversion == "s")
    return(as.character(value))
  if (conversion == "r")
    return(.repr(value))
  if (conversion == "a")
    return(.repr(value))
  stop(sprintf("Unknown conversion specifier '%s'", conversion))
}

.onUnload <- function (libpath) {
  library.dynam.unload("pformat", libpath)
}