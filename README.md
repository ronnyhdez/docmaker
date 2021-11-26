
<!-- README.md is generated from README.Rmd. Please edit that file -->

# docmaker <a href="url"><img src="man/figures/bibliotekarien.png" align="right" width="30%"></a>

<!-- badges: start -->
<!-- badges: end -->

Create your notes with Rmd and deploy them with mkdocs and github pages.

## Installation

You can install the development version of docmaker from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ronnyhdez/docmaker")
```

## Example

Once you have a repository, you can take your Rmd notes and deploy it to
GitHup pages

``` r
library(docmaker)
publish_notes(file = "check.Rmd", mkdocs_build = TRUE, mkdocs_deploy = TRUE)
```
