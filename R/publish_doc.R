#' @title Convert from Rmarkdown to markdown
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This function will take the Rmarkdown file and transform it
#' to a markdown file that can be taken by mkdocs to render a HTML
#' 
#' @param file The Rmarkdown file that we want to convert to markdown
#' 
#' @param mkdocs_build TRUE if you want to build the mkdocs documentation. This
#'  will run the `mkdocs build --config-file=mkdocs.yml` command
#' 
#' @example 
#' \dontrun{
#' publish_notes(file = "check.Rmd")
#'}
#'
#' @export
publish_notes <- function(file, overwrite = TRUE, mkdocs_build = FALSE, mkdocs_deploy = FALSE) {
  
  # Workflow with files out of a folder ----
  
  # Remove path and keep just the file name and extension
  original_file_name <- stringr::str_remove(file, ".*/")
  
  # Change Rmd file extension to md.
  file_name <- paste0(fs::path_ext_remove(original_file_name), ".md")
  
  # Render the file in here
  rmarkdown::render(input = file, 
                    output_file = file_name,
                    output_format = "md_document")
  
  # Copy new md file to docs/ folder (where site in rendered)
  # TODO: use fs::file_move()
  
  if (overwrite == TRUE) {
    fs::file_copy(file_name, new_path = "docs/", overwrite = TRUE)
  } else {
    fs::file_copy(file_name, new_path = "docs/")
    fs::file_delete(file_name)
  }
  
  # md file in here is no longer needed
  fs::file_delete(file_name)
  
  # Create path string to copy from.
  file_name <- fs::path_ext_remove(file_name)
  
  images_from_file <- paste0(file_name, "_files/")
  
  if (fs::dir_exists(images_from_file)) {
    
    file_path_name <- paste0("docs/", images_from_file)
    
    images_output <- paste0(here::here(), "/", file_path_name)
    
    if (!fs::dir_exists(images_output)) {
      
      file_outputs <- paste0(here::here(), "/", images_from_file)
      
      fs::dir_copy(path = file_outputs,
                   # Path by default on resulting md file involves a
                   # second docs/ folder. That's why I have to create
                   # a path that takes this in account.
                   new_path = paste0(here::here(), "/", file_path_name),
                   overwrite = TRUE)
      
      # Remove unused folder
      fs::dir_delete(file_outputs)
      # fs::dir_delete(paste0(here::here(), "/", file_path_name))
      
    }
    
  }
  
  # Build mkdocs documentation
  if (mkdocs_build == TRUE) {
    system("mkdocs build --config-file=mkdocs.yml")
  }
  
  # Deploy static site
  if (mkdocs_deploy == TRUE) {
    system("mkdocs gh-deploy --strict --force")
  }
  
}



