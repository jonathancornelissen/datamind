\name{output_contains}
\alias{output_contains}
\title{Test whether variables are equal to pre-specified values and return feedback}
\description{
Function checks whether the ouput of the argument expr can be found in the output of the student's console.
}
\usage{
output_contains( expr, console.output=DM.console.output )
}

\arguments{
  \item{expr}{ The expression (as string) for which the output should be in the student's console.} 
  \item{console.output}{ The string containing the output printed to the student's console. By default this will be recorded in DM.console.output before the Submission Correctness Test starts, so the console.output argument here is by default also set to DM.console.output.} 
}

\value{
TRUE when the student's console indeed contains the ouput you expect based on the given expression, FALSE otherwise.
}

\examples{
# Suppose the student has to type a loop that print the numbers 1 upto 10
# Smart student does exactly that and types: 
DM.user.code = '
n<-10;
for(i in 1:n){print(i)};
'
# What student's console contains:
DM.console.output = paste(capture.output(eval(parse(text=DM.user.code))), collapse="")
# What the test tells us:
output_contains("for(i in 1:10){print(i)}")
# Suppose the student was supposed to loop upto 20:
output_contains("for(i in 1:20){print(i)}")
}