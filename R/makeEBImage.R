#' convert a length-1 ImageArray to an Image object in EBImage, via jpeg export
#' @param iarr1 ImageArray instance
#' @examples
#' data(ciftrain1k)
#' ore = makeEBImage(ciftrain1k[4])
#' EBImage::display(ore, method="raster") # small!
#' @export
makeEBImage = function(iarr1) {
 stopifnot(is(iarr1, "ImageArray"))
 stopifnot(length(getTypes(iarr1))==1)
 dat = getArray(iarr1)[1,,,,drop=TRUE]
 tf = tempfile()
 jpeg::writeJPEG(dat/255, tf)
 tf = file.rename(tf, nf <- paste0(tf, ".jpg"))
 EBImage:::readImage(nf)
}

#' perform edge detection via Sobel algorithm
#' @param iarr1 ImageArray instance
#' @examples
#' data(ciftrain1k)
#' ore = runSobel(ciftrain1k[4])
#' opar = par(no.readonly=TRUE)
#' par(mfrow=c(2,2))
#' plotOne(ciftrain1k[4])
#' plotOne(ore, transpose=TRUE)
#' plotOne(ore, transpose=TRUE, interpolate=FALSE)
#' plotOne(ciftrain1k[4], interpolate=FALSE)
#' par(opar)
#' @export
runSobel = function(iarr1) {
 if (!requireNamespace("EBImageExtra")) stop("install ornelles/EBImageExtra from github to use this function")
 ei = makeEBImage(iarr1)
 sei = EBImageExtra::sobel(ei)
 ImageArray( array(sei, dim=c(1, dim(sei))), types=getTypes(iarr1),
   typelevels = littleDeep:::typelevels(iarr1))
 }

#' plot a length-1 ImageArray using as.raster
#' @param iarr1 ImageArray instance
#' @param transpose logical(1) use aperm(..., c(2,1,3)) before rendering
#' @examples
#' data(ciftrain1k)
#' ore = runSobel(ciftrain1k[4])
#' par(mfrow=c(2,2))
#' plotOne(ciftrain1k[4])
#' plotOne(ore, transpose=TRUE)
#' plotOne(ore, transpose=TRUE, interpolate=FALSE)
#' plotOne(ciftrain1k[4], interpolate=FALSE)
#' @export
plotOne = function(iarr1, transpose=FALSE, ...) {
 stopifnot(is(iarr1, "ImageArray"))
 stopifnot(length(getTypes(iarr1))==1)
 a = getArray(iarr1)
 a = a[1,,,,drop=TRUE]
 if (transpose) a=aperm(a, c(2,1,3))
 plot(as.raster(a/max(a)), ...)
}
