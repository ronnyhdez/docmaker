#' @title Render and deploy all your Rmd documents
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description Take all your Rmarkdown documents and render to markdown. This
#' function also can deploy all your content at once to your GitHub site.
#' 
#' @param deploy If TRUE all the Rmd documents will be deploy to your site
#' 
#' @example 
#' \dontrun{
#' render_docs(deploy = TRUE)
#'}
#'
#' @export
render_docs <- function(deploy = FALSE) {
  
  # List all files except the readme
  files <- fs::dir_ls(glob = "*.Rmd") 
  
  if (fs::file_exists("README.Rmd")) {
    rm_readme <- !files %in% c("README.Rmd")
    files <- files[rm_readme]
  }
  
  # Use own package function to render everything to md into docs folder
  files %>% 
    purrr::map(publish_notes)
  
  # Deploy when all md files are ready (do it once)
  if (deploy == TRUE) {
    system("mkdocs build --config-file=mkdocs.yml")
    system("mkdocs gh-deploy --strict --force")
  }
}






