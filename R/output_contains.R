output_contains = function(expr, console.output=DM.console.output){
  correct.output <- try(capture.output(try(eval(parse(text=expr)))))
  correct.output <- paste(correct.output,collapse = '')
  
  # Remove new-lines ??? TODO: do we need extra stuff here???
  console.output = gsub("\n|[[:space:]]","", console.output)
  correct.output = gsub("\n|[[:space:]]","", correct.output)
  
  # 
  where.is.regex <- gregexpr(pattern=correct.output, text=console.output, fixed=TRUE)
  if( any(where.is.regex[[1]] == (-1)) ){ return(FALSE) 
  } else { return(TRUE) }
}
