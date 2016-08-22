
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
            return (NULL)
        else
            return (substr(format_string, r$start, r$end))
    }
    
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
            
            if (bracket_count == 0 & c %in% c("}", ":", "!"))
                break;
        }
        
        obj$field_name_it = new_range(start, it$start - 2)
        
        if (c == "!" | c == ":") {
            if (c == "!") {
                
                if (it$start > it$end) {
                    stop("end of string while looking for conversion specifier")
                    return (list(result = 0))
                }
                
                obj$conversion = current_char()
                move()
                
                if (it$start <= it$end) {
                    c = current_char()
                    move()
                    
                    if (c == "}")
                        return (obj)
                    if (c != ":") {
                        stop("expected ':' after conversion specifier")
                        return (list(result = 0))
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
    
    MarkupIterator_next = function() {
        obj = list(
            literal_text = NULL
        )
        
        markup_follows = FALSE
        
        if (it$start >= it$end)
            return (NULL);
        
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
        
        if ((c == "}") & (at_end | c != current_char())) {
            stop("Single '}' encountered in format string")
            return (NULL)
        }
        
        if (at_end & c == '{') {
            stop("Single '{' encountered in format string")
            return (NULL)
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
            return (obj)
        
        obj2 = parse_field()
        
        if (obj2$result == 0)
            return(obj2)
        
        obj$field_name = piece(obj2$field_name_it)
        obj$format_spec = piece(obj2$format_spec_it)
        obj$format_spec_needs_expanding = obj2$format_spec_needs_expanding
        obj$conversion = obj2$conversion
        return (obj)
    }
    
    l = list()
    
    while (!is.null(obj <- MarkupIterator_next())) {
        l = c(l, list(obj))
    }
    
    return (l)
}