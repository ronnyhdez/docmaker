
<!-- README.md is generated from README.Rmd. Please edit that file -->

# docmaker <a href="url"><img src="man/figures/logo.png" align="right" width="30%"></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/docmaker)](https://cran.r-project.org/package=docmaker)
[![R-CMD-check](https://github.com/ronnyhdez/docmaker/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ronnyhdez/docmaker/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Overview

`docmaker` is a package that allows you to automate your workflow for
taking notes or creating documentation and publish it as a webpage using
GitHub pages, MkDocs, and Rmarkdown or Quarto or Markdown.

If you have a repository with files built with Rmarkdown/Quarto/Markdown
that you would like to take to MkDocs; `docmaker` will help you with the
all the steps in the middle to achieve this.

## Installation

You can install the development version of `docmaker` from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("ronnyhdez/docmaker")
```

If you don´t have the `devtools` package installed, run the following:

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

Once you have a repository, you can take your Rmarkdown/Quarto/Markdown
notes and deploy them with GitHub pages

``` r
make_doc(file = "check.Rmd", mkdocs_build = TRUE, mkdocs_deploy = TRUE)
```

If you have several files in the root of your project directory, you can
instead use the following function to build and deploy all your Rmd
files:

``` r
make_all_docs(deploy = TRUE)
```

In the case of having `md` files, you can use the following function to
update them in the folder that is deployed to GitHub pages:

``` r
update_md_files("~/Desktop/test_docmaker")
```

If a file is no longer needed, and you erase that file from your root
directory, it will needed to update the `docs/` folder. In that case we
can use the function:

``` r
clean_files()
```

## Getting help

If you find a bug please fill an issue with a reproducible example on
[GitHub](https://github.com/ronnyhdez/docmaker/issues/)

## Ref

Image is the bibliotekarien
