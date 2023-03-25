# Update md files

library(dplyr)
library(tidyr)
library(fs)

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
  path <- "~/Documents/repos/github/drawer" 
  path <- "~/Desktop/test_docmaker"
  files_root <- dir_info(path) %>% 
    filter(type == "file",
           grepl("\\.md$", path)) %>% 
    select(path, modification_time, change_time, birth_time) %>% 
    mutate(path = as.character(path)) %>% 
    separate(col = path, into = c("path", "file"), sep = ".*/") %>% 
    select(-path) %>% 
    filter(file != "README.md") %>% 
    rename("mod_time_root" = "modification_time",
           "change_time_root" = "change_time",
           "birth_time_root" = "birth_time")
  
  # Identify md files in the `docs` directory
  docs_path <- paste0(path, "/docs")
  files_docs <- dir_info(docs_path) %>% 
    filter(type == "file") %>% 
    select(path, modification_time, change_time, birth_time) %>% 
    separate(col = path, into = c("path", "file"), sep = ".*/") %>% 
    select(-path) 
  
  # If none of the files in root are in docs, copy them
  if (nrow(files_docs) == 0) {
    
  }
  
  
  # Join dataframes with the `files_root` as reference
  both <- files_root_md_only %>% 
    left_join(files_docs, by = join_by(file)) %>% 
    # Check which files in root are more updated than the ones in `docs`
    filter(change_time_root > change_time) %>% 
    select(file) %>% 
    pull()
}



  



