#' @name split_path
#' @description Split a path string into directory and path.
#' @param path_string [string] Path with directory
#' @return [list: dir, file] List containing \n[dir] directory string and \n[file] file string. 
#' If no directory found, dir is NULL.
#'
#' @export

split_path <- function(path_string) {
  
  stopifnot(is.character(path_string))
  dir <- NULL
  file <- NULL
  
  if(grepl("[/\\\\]", path_string)){
    
    find_files <- "^(?<dir>.*[/\\\\])(?<file>[^/\\\\]+)$"
    dir_specified <- regexpr(find_files, path_string, perl=TRUE)
    reg_mat <- rbind(
      attr(dir_specified, "capture.start"), 
      attr(dir_specified, "capture.length"))
    dir <- substr(path_string, reg_mat[1,'dir'], sum(reg_mat[,'dir'])-1)
    file <- substr(path_string, reg_mat[1,'file'], sum(reg_mat[,'file'])-1)
  
  } else if(grepl(".", path_string)) {
    file <- path_string
  }
  
  return(list(
    dir = dir,
    file = file
  ))
}
