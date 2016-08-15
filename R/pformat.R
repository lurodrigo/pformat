
#' Split the arguments in positional and keyword categories
#'
#' @param ... a list of arguments
#'
#' @return a list with two fields: 
#' \itemize{
#'     \item pos: positional arguments
#'     \item kw: keyword arguments
#' }
#' @export
#'
.pargs = function(...) {
    l = list(...)
    fields = names(l)
    nargs = length(l)
    npos = length(fields[fields == ""])
    
    args = list()
    if (npos > 0) 
        args$pos = l[1:npos]
    else
        args$pos = list()

    if (npos != nargs)
        args$kw = l[(npos+1):nargs]
    else
        args$kw = list()

    return(args)
}

pformat <- function(format_string, ...) {
    temp = .pargs(...)
    args = temp$pos
    kwargs = temp$kw
    
    return (.pformat(format_string, args, kwargs, 2))
}

.pformat <- function(format_string, args, kwargs, recursion_depth, 
                     auto_arg_index = 0) {
    if (recursion_depth < 0) 
        error("Max string recursion ")
    
    result = list()
    
    parsed = .pformat_parse(format_string)
    
    for (i in length(parsed)) {
        # output the literal text
        if ( parsed[[i]]$literal_text != "" )
            result = c(result, list(parsed[[i]]$literal_text))
            
        # if there's a field, output it
        if ( !is.null(parsed[[i]]$field_name) ) {
            # this is some markup, find the object and do the formatting
            
            # handle arg indexing when empty field_names are given
            if ( parsed[[i]]$field_name == "" ) {
                if ( auto_arg_index == 0 )
                    error("pformat: cannot switch from manual field specification to automatic field numbering")
                
                auto_arg_index = auto_arg_index + 1
                field_name = as.character(auto_arg_index)
            } else if ( !is.na(as.integer(parsed[[i]]$field_name)) ) { 
                # is a string representing an integer
                if ( auto_arg_index > 0 )
                    error("pformat: cannot switch from manual field specification to automatic field numbering")
                
            }
        }
    }
}

.pformat_parse <- function(formatString) {
    return ()
}