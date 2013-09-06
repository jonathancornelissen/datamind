\name{datamind_login} 
\alias{datamind_login} 
\title{Login in to www.datamind.org} 
\description{ 
Logging in to www.datamind.org will allow you to upload your chapters written in R markdown to datamind.org, where they are converted into  interactive exercises. The function will store a token in the \code{.DATAMIND_ENV} environment.
} 

\usage{ 
datamind_login(email, password) 
} 

\arguments{ 
  \item{email}{ Email used to log into datamind.org } 
  \item{password}{ Password used to log into datamind.org}
} 

\value{The \code{datamind_login} function does not return a value but it is called for its side effect as described above.}

\examples{
datamind_login(email="test@test.com",password="testtest")
}