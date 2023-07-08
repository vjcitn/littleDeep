#' list jpgs in the package
#' @export
available_jpgs = function() {
 fi = dir(system.file("jpegs", package="littleDeep"), full.names=TRUE)
 simp = basename(fi)
 names(fi) = simp
 fi
}
