
.repr = function (x) paste0(capture.output(dput(x)), collapse = "\n")

pformat <- function(format_string, ...) {
    res = .pformat(format_string, list(...), 2)
    return(res$result)
}

.pformat <- function(format_string, pargs, recursion_depth, 
                     auto_arg_index = 1) {
    if (recursion_depth < 0) 
        error("Max string recursion")
    
    result = list()
    
    parsed = pformat_parse(format_string)
    
    for (i in 1:length(parsed)) {
        # output the literal text
        if ( parsed[[i]]$literal_text != "" )
            result = c(result, list(parsed[[i]]$literal_text))
            
        # if there's a field, output it
        if ( !is.null(parsed[[i]]$field_name) ) {
            # this is some markup, find the object and do the formatting
            
            # handle arg indexing when empty field_names are given
            if ( parsed[[i]]$field_name == "" ) {
                if ( auto_arg_index == 0 )
                    stop("cannot switch from manual field specification to automatic field numbering")
                
                parsed[[i]]$field_name = as.character(auto_arg_index)
                auto_arg_index = auto_arg_index + 1
            } else if ( .is_integer(parsed[[i]]$field_name) ) { 
                # is a string representing an integer
                if ( auto_arg_index > 1 )
                    stop("cannot switch from manual field specification to automatic field numbering")
                
                # disable auto arg incrementing, if it gets
                # used later on, then an exception will be raised
                auto_arg_index = 0
            }
            
            # given the field_name, find the object it references
            # and the argument it came from
            obj = .pformat_get_field(parsed[[i]]$field_name, pargs)
            
            # do any conversion on the resulting object
            obj = .pformat_convert_field(obj, parsed[[i]]$conversion)
            
            
            # expand the format spec, if needed
            if ( parsed[[i]]$format_spec_needs_expanding ) {
                rec = .pformat(parsed[[i]]$format_spec, pargs, 
                               recursion_depth - 1, auto_arg_index)
                parsed[[i]]$format_spec = rec$result
                auto_arg_index = rec$index
            }
            
            # format the object and append to the result
            result = c(result, list(.pformat_format_field(obj, format_spec)))
        }
    }
    
    return (list(result = do.call(paste0, result), index = auto_arg_index))
}

.is_integer = function(s) {
    v = getOption("warn")
    options(warn = -1)
    x = !is.na(as.integer(s))
    options(warn = v) 
    return(x)
}

.pformat_get_field = function(field_name, pargs) {
    if (.is_integer(field_name))
        return (pargs[[as.integer(field_name)]])
    else
        return (eval(parse(text = field_name), envir = pargs))
}

.pformat_format_field = function(value, format_spec) {
    return(as.character(value))
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
    stop(sprintf("Unknown conversion specifier '%s'", conversion))
}