\name{author_course}
\alias{author_course}
\title{Author an interactive course}
\description{
The author_course function will: 
(i) create a folder in you current working directory with the name "courseName", 
(ii) initialize a Git repo for version control, 
(iii) copy a demo file "index.Rmd" inside that folder so you can get started, and 
(iv) open "index.Rmd", a template for creating a chapter, such that you can start writing exercises in the template. Find more information on the (necessary) blocks of an interactive exercise on DataMind below or just read the instructions in the template.
}

\usage{
author_course(chapdir,...)
}

\arguments{
  \item{chapdir}{ String indicating the course name (and thus the name of the folder that will be created)} 
  \item{...}{ Extra arguments you'd like to pass to the function. Under the hood, the \code{author} function from the \code{slidify} package is called.}
}

\value{The \code{author_course} function does not return a value but it is called for its side effect as described above.}

\examples{
author_course("myNewTutorialName")
}