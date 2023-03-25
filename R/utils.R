#' @importFrom magrittr %>%

#' @title Copy the images in external folder to docs folder
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description If your .Rmd file import images that are located in a external
#' folder, this function will detect these images and copy them to the `docs`
#' folder so the file can be succesfully deployed with MkDocs
#' 
#' @param file_to_deploy Is the name of the file to validate that have imported
#' external images.
#' 
#' @examples 
#' \dontrun{
#' copy_rmd_images("03-nicoya.Rmd")
#'}
#'
#' @noRd
copy_rmd_images <- function(file_to_deploy) {
  
  existing_elements <- readLines(file_to_deploy, warn = FALSE)
  
  rmd_img_paths <- existing_elements[stringr::str_detect(existing_elements, 
                                                         "!\\[\\]")]
  # Check if there exists calls to images inside the file
  if (!rlang::is_empty(rmd_img_paths == FALSE)) {
    # Retrieve just the path
    clean_image_paths <- stringr::str_extract_all(rmd_img_paths,
                                                  "[^\\!\\[\\]\\(\\)]") %>% 
      purrr::map(.f = function(x) stringr::str_c(x, collapse = ""))
    
    clean_image_paths %>% 
      purrr::map(.f = function(x) {
        # Take the path up to the first "/"
        path_folder <- paste0("docs/", stringr::str_extract(x, "[^/]+"))
        if (!fs::dir_exists(path_folder)) {
          fs::dir_create(paste0(here::here(), "/", path_folder))
        }
        
        fs::file_copy(path = x,
                      new_path = path_folder,
                      overwrite = TRUE)
      })
  }
  
  # close(file_to_deploy)
}