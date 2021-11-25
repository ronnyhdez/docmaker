#' @title Publish from folder
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This function will take the Rmarkdown file contained in a folder
#' and transform it to a markdown file that can be taken by mkdocs to render a
#' HTML file
#' 
#' @param file The Rmarkdown file that we want to convert to markdown
#' @param mkdocs_build TRUE if you want to build the mkdocs documentation. This
#'  will run the `mkdocs build --config-file=mkdocs.yml` command
#' 
#' @example 
#' \dontrun{
#' convert_rmd_md(file = "check.Rmd")
#'}
#'
#' @export
publish_folder_notes <- function(file, mkdocs_build = FALSE, mkdocs_deploy = FALSE) {
  
  # Wrokflow con archivo en notes. Falla si archivo necesita datos
  # # Copia archivo a carpeta docs
  # fs::file_copy(file, paste0(here::here(), "/docs/"))
  # 
  # # Nos dejamos unicamente el nombre del archivo sin el path
  # original_file_name <- stringr::str_remove(file, ".*/")
  # 
  # # Creamos el nombre del archivo .md
  # file_name <- paste0(fs::path_ext_remove(original_file_name), ".md")
  
  # # Renderizamos el archivo
  # rmarkdown::render(input = paste0("docs/", original_file_name), 
  #                   output_file = file_name,
  #                   output_format = "md_document")
  
  # # Borramos archivo Rmd copiado en  carpeta docs:
  # fs::file_delete(paste0(here::here(), "/docs/", original_file_name))
  
  # Workflow con archivos fuera (bajo here())
  original_file_name <- stringr::str_remove(file, ".*/")
  file_name <- paste0(fs::path_ext_remove(original_file_name), ".md")
  rmarkdown::render(input = file, 
                    output_file = file_name,
                    output_format = "md_document")
  
  fs::file_copy(file_name, new_path = "docs/")
  fs::file_delete(file_name)
  
  # Create path string to copy from.
  file_name <- fs::path_ext_remove(file_name)
  
  file_path_name <- paste0("docs/", file_name, "_files/")
  
  images_output <- paste0(here::here(), "/", file_path_name)
  
  if (!fs::dir_exists(images_output)) {
    
    file_outputs <- paste0(here::here(), "/", file_name, "_files")
    
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
  
  # Build mkdocs documentation
  if (mkdocs_build == TRUE) {
    system("mkdocs build --config-file=mkdocs.yml")
  }
  
  # Deploy static site
  if (mkdocs_deploy == TRUE) {
    system("mkdocs gh-deploy --strict --force")
  }
  
}



