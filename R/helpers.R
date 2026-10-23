# mergeing list of same kind of data files from a folder
merge_files_from_folder <-  function(folder) {

  file_list <- list.files(folder, pattern = "\\.(csv|txt)$",
    ignore.case = TRUE, full.names = TRUE, recursive = TRUE) 

  read_each <- function(f){
    if (str_detect(f, regex("\\.csv$", ignore_case = TRUE))) {
      read_csv(f, show_col_types = FALSE)
    } else {
      read_table(f, show_col_types = FALSE)
    }
  }
  pop_data_raw <- file_list |>
    set_names(basename(file_list)) |>
    map(read_each) |>
    list_rbind(names_to = "id") 
}



