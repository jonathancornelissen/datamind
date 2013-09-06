.check_list = function(name,value){
  
  # Check existence of variables in workspace:
  check <- check_existence(name)
  if(!is.null(check)){ return(check) }
  
  # Get the values:
  the.name  <- eval(parse(text=name));
  the.value <- eval(parse(text=value));
  
  # Check whether list has correct length: 
  if( length(the.name) != length(the.value) ){ 
    to.print <- paste("The list",name,"does not seem to have the correct length:",length(the.value))
    return(list(FALSE,to.print))
  }
  
  # Check correct naming: 
  if(!is.null(names(the.value))){ 
    condition <- is.null(names(the.name)) | any(names(the.value) != names(the.name))
    if(condition){
      to.print <- paste("The list",name,"does not seem to have the correct names!")
      return(list(FALSE,to.print))
    }
  }
  
  n <- length(the.value); 
  equal <- NULL;
  for(i in 1:n){
    equal[i] <- .check_equal(the.name[[i]],the.value[[i]])
    if( !equal[i] ){  to.print <- capture.output( cat("Please check the value of list item:",i,"of the list:",name,"!") ) }
  }
  
  if(any(!equal)){ return( list(FALSE,to.print) ) } else { return( TRUE ) }  
}