% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_all_docs.R
\name{make_all_docs}
\alias{make_all_docs}
\title{Build and deploy all your Rmd documents to your gh-pages site}
\usage{
make_all_docs(deploy = FALSE)
}
\arguments{
\item{deploy}{If TRUE all the Rmd documents will be deploy to your site}
}
\description{
Take all your Rmarkdown documents and render to markdown. This
function also can deploy all your content at once to your GitHub site.
}
\details{
The function assumes that all your Rmd files are located in the
root of your directory. Every file that is \strong{Rmd} will be rendered to md
and then build with mkdocs followed by the deployment action if param
\code{deploy} is TRUE.

If you are using your README file as .Rmd, this function will ignore this
file.
}
\examples{
\dontrun{
make_all_docs(deploy = TRUE)
}

}
\author{
Ronny Alexander Hernández Mora
}
