#' @import dplyr

#' @title Update your md files
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description This function will take your notes written in markdown directly
#' to your docs folder. If a note is updated, use this function to validate it
#' and update the docs/ directory.
#' 
#' @param path The path where your directory with your notes are located. If
#' empty, it will take the path of the repository you are located.
#' 
#' @examples 
#' \dontrun{
#' update_md_files("~/Desktop/test_docmaker")
#'}
#'
#' @export
update_md_files <- function(path = "here") {
  
  # Create objects with paths
  if (path == "here") {
    path <- here::here()
    docs_path <- paste0(path, "/docs")
    
  } else {
    path <- path
    docs_path <- paste0(path, "/docs")
  }
  
  # Validate if paths exists
  stopifnot("Path does not exists" = fs::dir_exists(path))
  stopifnot("There is no docs directory in path" = fs::dir_exists(docs_path))
  
  # Identify md files in the root directory
  # path <- "~/Documents/repos/github/drawer" 
  # path <- "~/Desktop/test_docmaker"
  files_root <- fs::dir_info(path) %>% 
    filter(type == "file",
           grepl("\\.md$", path)) %>% 
    select(path, modification_time, change_time, birth_time) %>% 
    mutate(path = as.character(path)) %>% 
    tidyr::separate(col = path, into = c("path", "file"), sep = ".*/") %>% 
    select(-path) %>% 
    filter(file != "README.md") %>% 
    rename("mod_time_root" = "modification_time",
           "change_time_root" = "change_time",
           "birth_time_root" = "birth_time")
  
  # Identify md files in the `docs` directory
  docs_path <- paste0(path, "/docs")
  files_docs <- fs::dir_info(docs_path) %>% 
    filter(type == "file") %>% 
    select(path, modification_time, change_time, birth_time) %>% 
    tidyr::separate(col = path, into = c("path", "file"), sep = ".*/") %>% 
    select(-path) 
  
  # If none of the files in root are in docs, copy them
  if (nrow(files_docs) == 0) {
    files_root %>% 
      select(file) %>% 
      pull() %>% 
      purrr::map(~ file_copy(file.path(path, .x), 
                             file.path(docs_path, .x),
                             overwrite = TRUE))
    
    print("Files are updated!")
    
  } else {
    # Join dataframes with the `files_root` as reference
    both <- files_root %>% 
      left_join(files_docs, by = join_by(file)) %>% 
      # If a file exists in root but not in docs, treat the NA
      mutate(change_time = tidyr::replace_na(change_time, 
                                             lubridate::make_datetime())) %>%
      # Check which files in root are more updated than the ones in `docs`
      filter(change_time_root > change_time) %>% 
      select(file) %>% 
      pull()
    
    if (length(both) == 0) {
      print("No files in docs dicrectory are outdated")
    } else {
      both %>% 
        purrr::map(~ file_copy(file.path(path, .x), 
                               file.path(docs_path, .x),
                               overwrite = TRUE))
      
      print("Files are updated!")
    }
  }
}



  



