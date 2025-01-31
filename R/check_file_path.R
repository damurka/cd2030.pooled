#' @keywords internal
check_file_path <- function(path, call = caller_env()) {

  check_required(.data, call = call)
  # Validate if the file exists
  if (!file.exists(path)) {
    cd_abort(
      c("x" = "The specified file {.val {path}} does not exist. Please provide a valid file path."),
      call = call
    )
  }

  invisible(TRUE)
}
