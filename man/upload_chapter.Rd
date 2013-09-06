\name{upload_chapter} 
\alias{upload_chapter} 
\title{Upload a chapter to www.datamind.org} 
\description{ 
Before calling this function you should have logged into datamind.org with \code{\link{datamind_login}} function. The function parses the chapter using the \code{slidify} function, then upload the result to datamind.org and shows you 
} 

\usage{ 
upload_chapter(inputFile,...)
} 

\arguments{ 
  \item{inputFile}{ Path to the ".Rmd" file to be uploaded } 
  \item{...}{ Extra arguments to be passed to the \code{slidify} function under the hood }
} 

\value{The \code{upload_chapter} function does not return a value but it is called for its side effect as described above.}

\examples{
upload_chapter(inputFile="myChapterName.Rmd")
}