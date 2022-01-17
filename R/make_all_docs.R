#' @importFrom magrittr %>%

#' @title Build and deploy all your Rmd documents to your gh-pages site
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description Take all your Rmarkdown documents and render to markdown. This
#' function also can deploy all your content at once to your GitHub site.
#' 
#' @details The function assumes that all your Rmd files are located in the
#' root of your directory. Every file that is **Rmd** will be rendered to md
#' and then build with mkdocs followed by the deployment action if param 
#' `deploy` is TRUE. 
#' 
#' If you are using your README file as .Rmd, this function will ignore this
#' file.
#' 
#' @param deploy If TRUE all the Rmd documents will be deploy to your site
#' 
#' @example 
#' \dontrun{
#' make_all_docs(deploy = TRUE)
#'}
#'
#' @export
make_all_docs <- function(deploy = FALSE) {
  
  # List all files except the readme
  files <- fs::dir_ls(glob = "*.Rmd") 
  
  if (fs::file_exists("README.Rmd")) {
    rm_readme <- !files %in% c("README.Rmd")
    files <- files[rm_readme]
  }
  
  # Use own package function to render everything to md into docs folder
  files %>% 
    purrr::map(make_doc)
  
  # Deploy when all md files are ready (do it once)
  if (deploy == TRUE) {
    system("mkdocs build --config-file=mkdocs.yml")
    system("mkdocs gh-deploy --strict --force")
  }
}






