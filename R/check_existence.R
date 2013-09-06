check_existence <- function( variables = NULL ){
  if ( !is.null(variables) ){ 
    it.exists <-  sapply(variables,exists)
    if ( any(!it.exists) ){
      to.print <- paste("It looks like you didn't define the variable:", variables[!it.exists] )
      return( list(FALSE,to.print) )
    } 
  } 
}