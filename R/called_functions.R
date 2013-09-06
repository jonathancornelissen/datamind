called_functions <- function( user.code=DM.user.code){
  # Break code up into sections preceding left brackets:
  left.brackets <- c(unlist( strsplit(user.code, split="[[:space:]]*\\(")) )
  
  # Loop over these elements to check for each one whether it contains a function call
  # and extract the function name if that is the case!
  called.functions <- as.character( c(unlist(sapply(left.brackets, 
                                             function (x) {
                                               # Split up according to anything that can't be in a function name.
                                               # split = not alphanumeric, not '_', and not '.'
                                               words <- c(unlist(strsplit(x, split="[^[:alnum:]_.]")))
                                               last.word <- tail(words, 1)
                                               last.word.is.function <- tryCatch(is.function(get(last.word)),
                                                                                 error=function(e) return(FALSE))
                                               return(last.word[last.word.is.function])
                                             })))  )
  
  return(called.functions)  
}