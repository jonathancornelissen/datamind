.check_equal = function(the.name,the.value){
  NAS       <-  any(is.na(the.value))   |  any(is.na(the.name))
  NULLS     <-  any(is.null(the.value)) |  any(is.null(the.name))
  factors   <-  is.factor(the.value) & is.factor(the.name)
    
  if(NAS){ #TODO: make NA handling better
    equal <- any(is.na(the.value)) &  any(is.na(the.name))  
  } else if(NULLS){  #TODO make NULL handling better   
    equal <- any(is.null(the.value)) &  any(is.null(the.name))
  }else{
    if(factors){ # Check factor levels are the same, otherwise we get error on comparison
      if( ! all(levels(the.value)==levels(the.name))){ return(FALSE) } 
    }
    equal <- try( all(the.name == the.value),silent=TRUE)
    if(!is.logical(equal)){ equal <- FALSE }
  }
  return(equal)
}