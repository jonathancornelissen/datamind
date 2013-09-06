\name{preview_chapter}
\alias{preview_chapter}
\title{Preview a chapter locally}
\description{ 
The function parses the chapter (in R markdown) using the \code{slidify} function from the \code{slidify} package. It then opens an html page that gives you an overview of the chapter in a nicer interface, so you can check it for typo's. Later we will add checks on the quality and correctness of your exercises in this overview. 
}

\usage{
preview_chapter(inputFile,outputFile,...)
} 

\arguments{ 
  \item{inputFile}{ Path to chapter ".Rmd"" file to be previewed.} 
  \item{outputFile}{ Path to name of ".html" output file that will be created. By default the name of the input file is taken.}
  \item{...}{ Extra arguments you'd like to pass to the function. Under the hood, the \code{slidify} function from the \code{slidify} package is called.} 
} 

\value{The \code{preview_chapter} function does not return a value but it is called for its side effect as described above.}

\examples{
preview_chapter("index.Rmd")
}