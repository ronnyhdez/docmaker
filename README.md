
<!-- README.md is generated from README.Rmd. Please edit that file -->

# docmaker <a href="url"><img src="man/figures/bibliotekarien.png" align="right" width="30%"></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/docmaker)](https://cran.r-project.org/package=docmaker)
<!-- badges: end -->

## Overview

`docmaker` is a package that allows you to automate your steps for using
GitHub pages, MkDocs and Rmarkdown.

If you have a project in which there are files built with Rmarkdown that
you would like to take to MkDocs, `docmaker` will help you with the all
the steps in the middle to achieve this.

## Installation

You can install the development version of `docmaker` from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("ronnyhdez/docmaker")
```

When published on CRAN, you will be able to install the package with:

``` r
install.packages("devtools")
```

## Usage

You can create your repository on GitHub, clone your repo and from
there, you will need to implement a structure to be able to use GitHub
pages and MkDocs. For this you can use the function:

``` r
library(docmaker)
build_repo(github_page_url = "https://ronnyhdez.github.io/drawer/",
           site_author = "Ronny A. Hernández Mora")
```

Once you have a repository, you can take your Rmd notes and deploy it to
GitHup pages

``` r
make_doc(file = "check.Rmd", mkdocs_build = TRUE, mkdocs_deploy = TRUE)
```

If you have several files in the root of your project directory, you can
instead use the following function to build and deploy all your Rmd
files:

``` r
make_all_docs(deploy = TRUE)
```

## Getting help

If you find a bug please fill an issue with a reproducible example on
[GitHub](https://github.com/ronnyhdez/docmaker/issues/)
