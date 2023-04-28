#' ldkeras will provide reference to insulated keras, tensorflow, cuda, etc.
#' @note ldkeras is expected to provide reference to insulated keras, tensorflow, cuda, etc.
#' @export
ldkeras = function() {
  proc <- basilisk::basiliskStart(bsklenv)
  on.exit(basilisk::basiliskStop(proc))
  basilisk::basiliskRun(proc, function() {
    reticulate::import("keras")
  })
}

