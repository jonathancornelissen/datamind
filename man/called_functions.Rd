\name{called_functions}
\alias{called_functions}
\title{Get the functions a student called}
\description{
Function takes DM.user.code as input, then checks sequentially what functions were called by the student and returns a vector with the character names of those functions. 
}
\usage{
called_functions(user.code=DM.user.code)
}
\arguments{
  \item{user.code}{ Character text you want to investigate.} 
}
\value{A character vector with the function names called by the student, in the order they appeared in the user.code}

\examples{
DM.user.code <- ' #Some random user code
x <- 1:10
mean(x,y=blablaY,z=blablaZ);sum(x)
sapply(x,FUN=sum)
mean(x2=2,y2=3)
'
# Try it out:
called_functions()
}