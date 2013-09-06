student_typed <- function(string, user.code=DM.user.code){
  # Remove white spaces, ";" and "\n" from string & user.code
  user.code <- gsub( "[[:space:]]|;|\n","",user.code );
  string <- gsub( "[[:space:]]|;|\n","",string );
  
  # Student can choose to assign by "<-" or "=", this shouldn't matter
  user.code <- gsub( "<-","=",user.code );
  string <- gsub( "<-","=",string );
  
  # Student can choose to TRUE/T and FALSE/F
  user.code <- gsub( "FALSE","F", user.code );
  string    <- gsub( "FALSE","F", string );
  user.code <- gsub( "TRUE", "T", user.code );
  string    <- gsub( "TRUE", "T", string );
  
  # Find reg ex and return result:  
  where.is.regex <- gregexpr(pattern=string, text=user.code,fixed=TRUE);
  if( any(where.is.regex[[1]] == (-1)) ){ return(FALSE) } # No match for the reg ex 
  n <- length(where.is.regex[[1]]);
  return(n)
} 