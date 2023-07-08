#' list jpgs in the package
#' @export
available_jpgs = function() {
 dir(system.file("jpegs", package="littleDeep"), full.names=TRUE)
}
