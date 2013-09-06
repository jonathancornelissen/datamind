code_test = function( string, times=NULL, user.code=DM.user.code){
  # string is a character (vector) containing strings that the user should have typed
  # times is the minimum number of times the user should have typed that piece of code. (By default 1..)
  # user.code is the code of the user, by default DM.user.code  
  n <- length(string); to.print <- NULL;
  if(is.null(times)){ times <- rep(1,n) }
  
  for(i in 1:n){
    user.typed.it = student_typed(string[i])
    if (!user.typed.it){
      to.print <- c( to.print, paste("It looks like your code doesn't contain:",string[i]) )
    } else if(user.typed.it < times[i]){
      to.print <- c( to.print, paste("It looks like your code doesn't contain:",string[i],"only",user.typed.it,"times.") )
    }
  }
  
  if (is.null(to.print)){ 
    return(TRUE) 
  } else {
    return( list(FALSE,to.print) )
  }
}