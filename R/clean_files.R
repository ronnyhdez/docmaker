#' @import dplyr

#' @title Clean files in the docs folder that no longer exists in the root path.
#' 
#' @author Ronny Alexander Hernández Mora
#' 
#' @description If you have erased no longer needed files in the root directory,
#' the render `md` files will continue to exists in the docs folder and hence, 
#' they will continue to appear in your webpage. This function checks if there
#' are erased files in the root and delete them from the docs folder.
#' 
#' @param path The path where your directory with your notes are located. If
#' empty, it will take the path of the repository you are located.
#' 
#' @examples 
#' \dontrun{
#' clean_files("~/Desktop/test_docmaker")
#'}
#'
#' @export
clean_files <- function(path = "here") {
  # Create objects with paths
  if (path == "here") {
    root_path <- here::here()
    docs_path <- paste0(root_path, "/docs")
    
  } else {
    root_path <- path
    docs_path <- paste0(path, "/docs")
  }
  
  # Validate if paths exists
  stopifnot("Path does not exists" = fs::dir_exists(root_path))
  stopifnot("There is no docs directory in path" = fs::dir_exists(docs_path))
  
  # docs_path <- "/home/ronny/Documents/repos/github/drawer/docs"
  # root_path <- "/home/ronny/Documents/repos/github/drawer"
  
  # Identify md files in the root directory
  files_root <- fs::dir_info(root_path) %>% 
    # filter(type == "file",
    #        grepl("\\.md$", path)) %>% 
    filter(type == "file",
           grepl(".*\\.(Rmd|qmd|md$)", path)) %>% 
    select(path, modification_time, change_time, birth_time) %>% 
    mutate(path = as.character(path)) %>% 
    tidyr::separate(col = path, into = c("path", "file"), sep = ".*/") %>% 
    select(-path) %>% 
    filter(file != "README.md") %>% 
    rename("mod_time_root" = "modification_time",
           "change_time_root" = "change_time",
           "birth_time_root" = "birth_time") %>% 
    tidyr::separate(col = file, into = c("name_file", "extension"), sep = "\\.")
  
  # Identify md files in the `docs` directory
  files_docs <- fs::dir_info(docs_path) %>% 
    filter(type == "file",
           grepl("\\.md$", path)) %>%
    select(path, modification_time, change_time, birth_time) %>% 
    tidyr::separate(col = path, into = c("path", "file"), sep = ".*/") %>% 
    select(-path) %>% 
    tidyr::separate(col = file, into = c("name_file", "extension"), sep = "\\.")
  
  both <- files_docs %>% 
    # Check wich files exists in docs but not anymore in root
    anti_join(files_root, by = join_by(name_file)) %>% 
    tidyr::unite(file_name, name_file,extension, sep = ".") %>% 
    select(file_name) %>%
    pull()
  
  if (length(both) == 0) {
    print("No outdated files existed in the docs folder")
  } else {
    files_to_erase <- purrr::map_vec(both, function(path) {
      paste0(docs_path, "/", path)
    })
    
    fs::file_delete(files_to_erase)
    
    message <- paste("These files have been erased from `docs/`:",
                     paste(both, collapse = ", "))
    print(message)
  }
}

