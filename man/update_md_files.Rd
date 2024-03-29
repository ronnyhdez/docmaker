% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_md_files.R
\name{update_md_files}
\alias{update_md_files}
\title{Update your md files}
\usage{
update_md_files(path = "here")
}
\arguments{
\item{path}{The path where your directory with your notes are located. If
empty, it will take the path of the repository you are located.}
}
\description{
This function will take your notes written in markdown directly
to your docs folder. If a note is updated, use this function to validate it
and update the docs/ directory.
}
\examples{
\dontrun{
update_md_files("~/Desktop/test_docmaker")
}

}
\author{
Ronny Alexander Hernández Mora
}
