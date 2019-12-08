#' @title find_filename finds a file in the Grattan data warehouse
#' @description `find_filename()` finds files that correspond to the provided
#' input in the Grattan data warehouse.
#'
#' @param filename A filename or fragment, with or without filepath,
#' such as "SIH15bh.dta", "SIH15bh", "ABS/SIH/2015-16/Stata/SIH15bh.dta", "SIH".
#' If you specify a file extension, such as ".dta", ".csv", or ".sas7bdat",
#' the extension must match exactly.
#'
#' @return The full local path to the file that matches 'filename' in the
#' Grattan data warehouse. If more than one match is found, an error will
#' be shown, including details of the multiple matches.
#'
#' @details Note that certain file extensions, including ".zip", ".txt" and
#' ".fst" are excluded from find_filename's search. If you want to load a ".fst"
#' file, use `read_microdata(fast = TRUE)`
#'
#' @importFrom tools file_ext
#'
#' @export

# Function finds filename corresponding to supplied 'filename',
# either fails with appropriate errors or returns filename
find_filename_non_grattan <- function(filename,data_warehouse_path) {


  # Look in all subdirs of the datawarehouse, if a single match is found,
  # return it
  # If 0 matches, stop; if > 1 matches, stop and tell the user what they are
  # First, exclude files with given extensions ('unused_extensions')
  # unused_extensions is an internal data object; see data-raw

  all_files <- list.files(data_warehouse_path, recursive = TRUE)
  #all_files <- all_files[!tolower(file_ext(all_files)) %in% unused_extensions]

  # Exclude folders that match these names
  unused_folders <- paste0(c("/doc/", "/documentation/"),
                           collapse = "|"
  )

  all_files <- all_files[!grepl(unused_folders, all_files)]

  matched_files <- all_files[grepl(filename, all_files,
                                   ignore.case = TRUE
  )]

  # If the user supplies an extension (.csv, .dta, whatever) then the returned
  # file *must* match that extension
  supplied_ext <- file_ext(filename)

  if (supplied_ext != "") {
    matched_files <- matched_files[file_ext(matched_files) == supplied_ext]
  }

  # If there are >1 matched, tell the user what
  # they are so they can be more specific
  if (length(matched_files) > 1) {
    stop(paste0(
      "Multiple files were found with ", filename,
      " in the filename. ",
      ".\n The matches are:\n", paste0(matched_files, collapse = "\n")
    ))
  }

  path <- file.path(data_warehouse_path, matched_files)


  return(path)
}
