#' @author J. Beem-Miller
#' @description Reads Jena AMS reports (.xlsx) files
#' @details Function first checks that the target file matches the template, then strips the headers out of the file and returns a data frame with the data values
#' @param template_file Filename, character
#' @return list of data frames
#' @import openxlsx

library(openxlsx)

read_jena_ams_results <- function(jena_ams_dir, template_file) {
  if(missing(template_file)) {
    template_file <- "../data/raw/ams_jena_template_2020-04-22/ams_jena_template.xlsx"
  }
  
  # start input at column header row (27)
  template <- read.xlsx(template_file, startRow = 27)

  # get pathnames for .xlsx files in jena_ams_dir
  data_files <- list.files(jena_ams_dir, pattern = "\\.xlsx", full.names = TRUE)
  # remove open .xlsx files
  data_files <- grep(data_files, pattern='\\~\\$', inv=T, value=T)
  
  # check that row 27 column names match the template
  for(i in seq_along(data_files)) {
    for(j in seq_along(names(read.xlsx(data_files[i], startRow = 27)))) {
      if(names(read.xlsx(data_files[i], startRow = 27))[j] != names(template)[j])
        cat("Row 27 of ", data_files[i], " does not contain proper column names")
    }
  }
  
  # read in files
  data_ls <- lapply(seq_along(data_files), function(i) {
    read.xlsx(data_files[i], startRow = 27)
  })
  
  names(data_ls) <- grep(list.files(jena_ams_dir, pattern = "\\.xlsx"),pattern='\\~\\$', inv=T, value=T)

  return(data_ls)
}