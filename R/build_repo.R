#' @title Build repo template
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description A function to create the minimum files and folders require to
#' build a repository to work with mkdocs and GitHub pages.
#' 
#' @details This will create an empty `docs` folder where all your md files
#' will be located coupled with output files as images. Also it will create a
#' `mkdocs.yml` file that uses material theme, contains the site url and the
#' author name. 
#' 
#' @param github_page_url Your GitHub page name. You can check this when you
#' configure the GitHub pages in your repository. This is going to be written
#' in the mkdocs.yml file.
#' @param site_author The name of the author. This is going to be written in
#' the mkdocs.yml file.
#' 
#' @examples 
#' \dontrun{
#' build_repo(github_page_url = "https://ronnyhdez.github.io/drawer/",
#'            site_author = "Ronny A. Hernández Mora")
#'}
#'
#' @export
build_repo <- function(github_page_url, site_author) {
  
  docs_folder <- paste0(here::here(), "/docs")
  
  if (!fs::dir_exists(docs_folder)) {
    fs::dir_create(paste0(here::here(), "/docs"))
  }
  
  if (!fs::file_exists("mkdocs.yml")) {
    
    # Create elements for the basic info needed in the yml
    link <- paste0("site_url: ", github_page_url)
    author <- paste0("site_author: ", site_author)
    project_name <- rstudioapi::getActiveProject()
    project_name <- stringr::str_remove(project_name, ".*/")
    project_name <- paste0("site_name: ", project_name)
    
    
    yml_file <- file("mkdocs.yml")
    
    writeLines(c(project_name,
                 author,
                 link,
                 "theme:",
                 "   name: material",
                 "markdown_extensions:",
                 "   - toc:",
                 "      permalink: []"),
               yml_file)
    
    close(yml_file)
    
    # Include in .gitignore the site folder
    gitignore_file <- file(".gitignore")
    
    existing_elements <- readLines(gitignore_file)
    
    writeLines(c(existing_elements,
                 "site/"),
               gitignore_file)
    
    close(gitignore_file)
    
  }
}



