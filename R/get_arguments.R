get_arguments = function(function.name, user.code=DM.user.code){
  # Function takes a function.name and the DM.user.code as input, 
  # then checks how many times the user called that function 
  # and for each time the function was called it returns the arguments supplied to the function. 
  # The ouput is a list. Each list component contains a vector with the supplied arguments.
  # The names of the latter vector are the respective argument names.
  
  my.regex <- paste( function.name, "\\(.*?\\)",sep="") # Make reg ex pattern
  where.is.regex <- gregexpr(pattern=my.regex,text=user.code)    
  if( any(where.is.regex[[1]] == (-1)) ){ return(FALSE) } # No match for the reg ex 
  official.arguments <- names(formals(function.name));
  n <- length(where.is.regex[[1]]); # n is number of matches found
  argument.list <- list();
  
  # Start loop over different times the function was called
  for(i in 1:n){ 
    # Get all.arguments vector
    regex.start <- where.is.regex[[1]][i] + nchar(function.name)+1 #Start after functionname and bracket
    regex.end <- regex.start + attr(where.is.regex[[1]],"match.length")[i]-nchar(function.name) -3
    all.arguments <- substr( user.code, regex.start, regex.end ) # Extract the part with the arguments

    # Get overview of the separate arguments: in matrix, rows are arguments, 
    # Vector with supplied arguments and the names of the vector is the name of the argument
    arguments <- strsplit(all.arguments,",")[[1]]
    arguments.vector <- names.vector <- c();
    
    for(a in 1:length(arguments)){
      split.argument = strsplit(arguments[a],"=")[[1]]
      if(length(split.argument)==1){ # Only the variable supplied, not attached to name
        arguments.vector[a] <- split.argument
        names.vector[a] <- official.arguments[a]
      } else if (length(split.argument)==2){ # Argument name used by student
        arguments.vector[a] <- split.argument[2]
        names.vector[a] <- split.argument[1]            
      }
    }
    names(arguments.vector) <- names.vector    
    argument.list[[i]] <- arguments.vector
  }#end loop
  return(argument.list)
}