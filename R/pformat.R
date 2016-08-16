
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

.repr = function (x) paste0(capture.output(dput(x)), collapse = "\n")

pformat <- function(format_string, ...) {
    temp = .pargs(...)
    args = temp$pos
    kwargs = temp$kw
    
    res = .pformat(format_string, args, kwargs, 2)
    return(res$result)
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
                    stop("pformat: cannot switch from manual field specification to automatic field numbering")
                
                auto_arg_index = auto_arg_index + 1
                field_name = as.character(auto_arg_index)
            } else if ( .is_integer(parsed[[i]]$field_name) ) { 
                # is a string representing an integer
                if ( auto_arg_index > 0 )
                    stop("pformat: cannot switch from manual field specification to automatic field numbering")
                
                # disable auto arg incrementing, if it gets
                # used later on, then an exception will be raised
                auto_arg_index = 0
            }
            
            # given the field_name, find the object it references
            # and the argument it came from
            obj = .pformat_get_field(parsed[[i]]$field_name, args, kwargs)
            
            # do any conversion on the resulting object
            obj = .pformat_convert_field(obj, parsed[[i]]$conversion)
            
            # expand the format spec, if needed
            rec = .pformat(parsed[[i]]$format_spec, args, kwargs, 
                           recursion_depth - 1, auto_arg_index)
            format_spec = rec$result
            auto_arg_index = rec$index
            
            # format the object and append to the result
            result = c(result, list(.pformat_format_field(obj, format_spec)))
        }
    }
    
    return (list(result = do.call(paste0, result), index = auto_arg_index))
}

.is_integer = function(s) !is.na(as.integer(s))

.pformat_get_value = function(key, args, kwargs) {
    if (.is_integer(key))
        return(args[[key]])
    else
        return(kwargs[[key]])
}

# given a field_name, find the object it references.
#  field_name:   the field being looked up, e.g. "0.name"
#                 or "lookup[3]"
#  used_args:    a set of which args have been used
#  args, kwargs: as passed in to vformat
.pformat_get_field = function(field_name, args, kwargs) {
    splitted = .pformat_field_name_split(field_name)
    
    obj = .pformat_get_value(splitted$first, args, kwargs)
    
    # loop through the rest of the field_name, doing
    # getattr or getitem as needed
    for (i in 1:length(splitted$rest)) {
        if ( splitted$rest[[i]]$is_attr )
            obj = obj[[ splitted$rest[[i]]$id ]]
        else
            obj = obj[[i]]
    }
    
    return(obj)
}

.pformat_format_field = function(value, format_spec) {
    return(value)
}

.pformat_convert_field = function(value, conversion) {
    # do any conversion on the resulting object
    if (is.null(conversion))
        return(value)
    if (conversion == "s")
        return(as.character(value))
    if (conversion == "r")
        return(repr(r))
    if (conversion == "a")
        return(repr(r))
    stop(paste0("pformat: Unknown conversion specificier ", conversion))
}
