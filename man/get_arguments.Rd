\name{get_arguments}
\alias{get_arguments}
\title{Get the arguments of one or multiple function calls done by a student}
\description{
Function takes a function_name and the DM.user.code as input, then checks how many times the user called that function and for each time the function was called it returns the arguments supplied to the function. The ouput is a list. Each list component contains a vector with the supplied arguments. The names of the latter vector are the respective argument names.
}

\usage{
get_arguments(function.name, user.code=DM.user.code)
}

\arguments{
  \item{function.name}{ String with the name of the function you want to find the arguments supplied by the student. }
  \item{user.code}{ Name of the character text you want to investigate.  } 
}

\value{
A list, in which each list component report the arguments supplied by the student as a vector. The names of this vector are the respective argument names.
}

\examples{
DM.user.code <- '
x <- 1:10
mean(x,y=blablaY,z=blablaZ);sum(x)
sapply(x,FUN=sum)
mean(x2=2,y2=3)
'
# Try it out:
get_arguments("mean")
get_arguments("sapply")
}