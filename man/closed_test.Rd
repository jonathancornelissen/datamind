\name{closed_test}
\alias{closed_test}
\title{Test whether variables are equal to pre-specified values and return feedback}
\description{
Function takes a number of (variable) names as input for which it checks whether they are equal to the supplied values. It then return feedback that can be used as the output of the SCT (Submission Correctness Test).
}
\usage{
closed_test( names=NULL, values=NULL, check.existence=NULL)
}

\arguments{
  \item{names}{ Character vector or list with the names of the variables for which you would like their values.} 
  \item{values}{ Character vector or list with the values the corresponding variables should have. It should have the same length as the names vector obviously.} 
  \item{check.existence}{ Vector having same length as names and values containing boolean values, which indicate whether the existence in the users workspace of "names" element i should be checked. Typically (and by default) TRUE in case names element "i" is just a variable, but can be FALSE in case names element "i" is actually an expression, not a variable name, for example "names(some.vector)" } 
}
\value{
The output of this function indicates the correctness of the Test. It can either boolean or a list. In the former case, TRUE/FALSE indicates whether variables have the expected values. In the latter case, the first list-component is TRUE/FALSE indicating the result of the test, whereas the second element contains the text with feedback that should be provided to the student.
}

\examples{
# At first, the student typs his code: He needs to assign 4 variables:
a <- "Closed Test Example"
b <- c(1,2,3)
c <- matrix(1:9)
d <- list(name1=3, name2=4)

# Closed Submission Correctness Test as stipulated by the course creator!
names <- c("a","b","c","d")
values <- c("c('Closed Test Example')","c(1,2,3)","matrix(1:9)","list(name1=3,name2=4)")
DM.result <- closed_test(names,values)
DM.result #Everyting the student typed is correct, so it returns TRUE

#Suppose the student forgets to name the list correctly
d <- list(3,4) 
DM.result <- closed_test(names,values)
DM.result #This is picked up correctly by the Submission Correctness Test ;-)

d <- list(name1=3, name2=4) # Correct again
c <- "mistake" # Student makes another mistake...
DM.result <- closed_test(names,values)
DM.result #This is picked up correctly by the Submission Correctness Test ;-), 
#closed_test returns FALSE and then a message to help the student improve his answer
}