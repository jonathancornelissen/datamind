\name{student_typed}
\alias{student_typed}
\title{Test whether user code contains a certain string}
\description{
Returns either FALSE in case the student didn't type the string, or the number of occurrences in the code. Thus, it is a simple wrapper function for regexes. White spaces are ignored, as well as ";", as well as new lines. Furthermore, the function ignores whether a student uses "=" or "<-" for assignment.
}
\usage{
student_typed(string, user.code=DM.user.code)
}

\arguments{
  \item{string}{ String to look for in the students code.} 
  \item{user.code}{ The students code as character. By default set to DM.user.code which will contain the students code when the SCT starts.}
}
\value{
Either FALSE, either the number of occurences of the string (or something very similar to that string) in the students code.
}

\examples{
DM.user.code <- '
x <- 1:10
mean(x,y=blablaY,z=blablaZ);sum(x)
sapply(x,FUN=sum)
mean(x2=2,y2=3)
'

# Check the student code:
student_typed("x <- 1:10") #1: Exactly as typed by the student.
student_typed("x<-1:10;") #1: Not exactly as typed by the student.
student_typed("x<-5:10;") #FALSE: Mistake by student.
student_typed("mean") #2: Multiple occurrences.
}