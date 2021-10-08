##' @export
##' @rdname orderly_develop_start
orderly_develop_clean <- function(name = NULL, root = NULL, locate = TRUE) {
  loc <- orderly:::orderly_develop_location(name, root, locate)
  status <- orderly:::orderly_status(loc$path)
  drop <- status$filename[status$derived & status$present]
  if (length(drop) > 0L) {
    orderly:::orderly_log("remove", drop)
    file.remove(file.path(loc$path, drop))
  }
  invisible(NULL)
}
