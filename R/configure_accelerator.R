#' claude-recommended approach to agnostic config of GPU for mac and linux
#' @param verbose logical
#' @note Proposition is that if tensorflow-metal is available, MPS will be found and used,
#' certainly by optimizer_adam() if used.
#' @export
configure_accelerator <- function(verbose = TRUE) {
  gpus <- tensorflow::tf$config$list_physical_devices('GPU')
  if (length(gpus) > 0) {
    if (verbose) message("Using GPU: ", gpus[[1]]$name)
    tensorflow::tf$config$experimental$set_memory_growth(gpus[[1]], TRUE)
  } else {
    if (verbose) message("No GPU found, using CPU")
  }
  invisible(gpus)
}
