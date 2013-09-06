closed_test <- function( names=NULL, values=NULL, check.existence=NULL){
  if(!is.null(names) & !is.null(values)){   # Check wheter arguments were supplied
    
  if(is.null(check.existence)){ check.existence = rep(TRUE,length(names)) }  
  # Do the variables exist in user workspace?
  check <- check_existence(names[check.existence])  
  if(!is.null(check)){ return(check) }
  
  if( length(names) != length(values) ){     stop("There seems to be an error in the submission correctness test. Please contact the course creator.") } 
    
    to.print   <- equal <- NULL; 
    the.names  <- lapply( names,  function(zzzzA){ eval(parse(text=zzzzA))} )
    the.values <- lapply( values, function(zzzzA){ eval(parse(text=zzzzA))} )
    n          <- length(names)

    for(i in 1:n){ # Loop over arguments to check whether the student provided correct solution
      the.value <- the.values[[i]];
      the.name  <- the.names[[i]];
      we.have.lists <- (is.list(the.value) );
      
      if(we.have.lists){ 
        LC <- .check_list(names[[i]],values[[i]])
        equal[i] <- LC[[1]]
        if(!equal[i]){ to.print <- LC[[2]] }
      }
      if(!we.have.lists){
        equal[i]  <- suppressWarnings(.check_equal(the.name,the.value))
        if( !equal[i] ){  to.print <- capture.output( cat("Did you set the value of",names[[i]],"equal to:", values[[i]],"? ") ) }
      }
      }
    if(any(!equal)){ return( list(FALSE,to.print) ) } 
  }
  return(TRUE)
}