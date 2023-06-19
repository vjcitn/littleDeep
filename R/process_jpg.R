
#' import, resize and rotate a JPEG that should have been produced in square mode
#' @importFrom EBImage readImage rotate resize
#' @param fn path to JPEG file
#' @param w output width in pixels
#' @param h output width in pixels
#' @param angle in degrees, magnitude of rotation for EBImage::rotate
#' @examples
#' appl = system.file("jpegs", "oneBigApple.jpg", package="littleDeep")
#' im = process_jpg(appl, angle=90)
#' plot(grDevices::as.raster(im))
#' @export
process_jpg = function(fn, w=32, h=32, angle=0) {
  ein = EBImage::readImage(fn)
  EBImage::rotate(EBImage::resize(ein, w=w, h=h), angle=angle)
}

#pp = process_jpg("oneBigApple.jpg", angle=90)
#plot(as.raster(pp))
