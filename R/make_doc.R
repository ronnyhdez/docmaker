#' @title Take your Rmarkdown or quarto files to mkdocs
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This function will take your Rmarkdown or quarto files and 
#' transform them into a markdown file that can be taken by mkdocs to render
#' a HTML
#' 
#' @param file The Rmd or qmd file that we want to convert to markdown
#' 
#' @param mkdocs_build TRUE if you want to build the mkdocs documentation. This
#'  will run the `mkdocs build --config-file=mkdocs.yml` command
#'  
#' @param mkdocs_deploy TRUE if you want to deploy the mkdocs documentation. This
#'  will run the `mkdocs gh-deploy --strict --force` command
#' 
#' @examples 
#' \dontrun{
#' make_doc(file = "check.Rmd")
#'}
#'
#' @export
make_doc <- function(file, mkdocs_build = FALSE, mkdocs_deploy = FALSE) {
  
  # Workflow with files out of a folder ----
  
  # Read the file extension
  file_extension  <- stringr::str_extract(file, "\\.[^.]*$")
  # Remove path and keep just the file name and extension
  
  original_file_name <- stringr::str_remove(file, ".*/")
  
  # Change qmd or Rmd file extension to md.
  file_name <- paste0(fs::path_ext_remove(original_file_name), ".md")
  
  # Render the file in here
  if (file_extension == ".Rmd") {
    rmarkdown::render(input = file, 
                      output_file = file_name,
                      output_format = "md_document")
  } else if (file_extension == ".qmd") {
    quarto::quarto_render(input = file,
                          output_file = file_name,
                          output_format = "md")
  } else {
    stop("File format not supported")
  }
  
  # Copy new md file to docs/ folder (where site in rendered)
  fs::file_move(file_name, new_path = "docs/")
  
  # Create path string to copy from.
  images_generated_from_file <- paste0(fs::path_ext_remove(file_name),
                                       "_files/")
  
  if (fs::dir_exists(images_generated_from_file)) {
    file_path_name <- paste0("docs/", images_generated_from_file)
    images_output <- paste0(here::here(), "/", file_path_name)
    
    if (!fs::dir_exists(images_output)) {
      file_outputs <- paste0(here::here(), "/", images_generated_from_file)
      fs::dir_copy(path = file_outputs,
                   # Path by default on resulting md file involves a
                   # second docs/ folder. That's why I have to create
                   # a path that takes this in account.
                   new_path = paste0(here::here(), "/", file_path_name),
                   overwrite = TRUE)
      
      # Remove unused folder
      fs::dir_delete(file_outputs)
    }
  }
  
  copy_rmd_images(file_to_deploy = file)
  
  # Build mkdocs documentation
  if (mkdocs_build == TRUE) {
    system("mkdocs build --config-file=mkdocs.yml")
  }
  
  # Deploy static site
  if (mkdocs_deploy == TRUE) {
    system("mkdocs gh-deploy --strict --force")
  }
}


