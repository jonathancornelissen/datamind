\name{DM.codeTest}
\alias{DM.codeTest}
\title{Test whether the student's code contains certain strings}
\description{
Function takes (a number of) character string(s) as input and the number of times the student should have typed them and the student's code. It returns TRUE in case everything was as expected, and a list with first item FALSE and second item the (vector of) character string(s) to show to the student. 
}
\usage{
DM.codeTest( string, times=NULL, user.code=DM.user.code)
}

\arguments{
  \item{string}{ A character (vector) containing strings that the user should have typed.} 
  \item{times}{  The minimum number of times the user should have typed that piece of code. (By default 1 for every element in string)} 
  \item{user.code}{ Name of the character text you want to investigate. By default already set to DM.user.code. } 
}

\value{
The output of this function indicates the correctness of the Test. It can either boolean or a list. In the former case, TRUE/FALSE indicates whether variables have the expected values. In the latter case, the first list-component is TRUE/FALSE indicating the result of the test, whereas the second element contains the text with feedback that should be provided to the student.
}

\examples{
# This is what the student typed:
DM.user.code <- '# An addition\n5+5 \n\n# A substraction\n5-5 \n\n# A multiplication\n3*5\n\n # A division\n(5+5)/2 \n\n# Exponentiation\n2^5\n\n# Modulo\n\n'
# Let us check whether the student included 5+5 and 2^5 as requested in assignment
DM.codeTest( c("5+5","2^5") )
# However, he did not include:
DM.codeTest( c("5+5","2^5","very.bad.code") )
DM.codeTest( c("5+5","2^5","very.bad.code","very.very.bad.code") )
}