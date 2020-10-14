#' @author J. Beem-Miller
#' @description Reads Jena Iso lab reports (.xlsx) files
#' @details Function first checks that the target file matches the template, then strips the headers out of the file and returns a data frame with the data values
#' @param template_file Filename, character
#' @return list of data frames
#' @import readxl

library(readxl)

read_jena_iso_results <- function(jena_iso_dir, template_file) {
  if(missing(template_file)) {
    template_file <- "../data/raw/iso_jena_template_2020-07-27/iso_jena_template.xlsx"
  }

  # start input at column header row (27)
  template <- read_xlsx(template_file, sheet = "d13C value", skip = 2)

  # get pathnames for .xlsx files in jena_ams_dir
  data_files <- list.files(jena_iso_dir, pattern = "\\.xls", full.names = TRUE)
  # remove open .xlsx files
  data_files <- grep(data_files, pattern='\\~\\$', inv = TRUE, value = TRUE)

  # check that header row matches the template
  for(i in seq_along(data_files)) {
    for(j in seq_along(names(template))) {
      if(names(read_xls(data_files[i], sheet = "d13C value"))[j] != names(template)[j])
        cat("Column ", j, "malformed in ", basename(data_files[i]))
    }
  }

  # read in files
  data_ls <- lapply(seq_along(data_files), function(i) {
    cat(basename(data_files[i]), "\n")
    df <- read_xls(data_files[i], sheet = "d13C value")
  })

  names(data_ls) <- grep(list.files(jena_iso_dir, pattern = "\\.xls"),pattern='\\~\\$', inv = TRUE, value = TRUE)

  return(data_ls)
}
