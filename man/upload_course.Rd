\name{upload_course} 
\alias{upload_course} 
\title{Upload a course to www.datamind.org} 
\description{ 
Before calling this function you should have logged into datamind.org with \code{\link{datamind_login}} function. The function parses the entire course using the \code{slidify} function, then upload the result to datamind.org and shows you 
} 

\usage{ 
upload_course(open=TRUE)
} 

\arguments{ 
  \item{open}{ Boolean, TRUE by default, determines whether a browser window should open, showing the course creation web interface } 
} 

\value{The \code{upload_course} function does not return a value but it is called for its side effect as described above.}

\examples{
# TODO add downloading intro to R folder here
upload_course()
}