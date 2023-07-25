#' make an ImageArray with a single jpeg
#' @param fn path to jpeg
#' @param type character(1)
#' @param type typelevels character()
#' @examples
#' purp = system.file("jpegs", "newpurple.jpg", package="littleDeep")
#' c3 = restore_islr_cnn(system.file("extdata", "cif3", package="littleDeep")) 
#' ia = jpeg2iarr(purp, "rose", c3$typelevels) 
#' scores_string(c3$model, ia)
#' @export
jpeg2iarr = function(fn, type, typelevels) {
  j = process_jpg(fn)
  j = array(j@.Data, dim=c(1,32,32,3))
  ImageArray(arr=j, types=type, typelevels=typelevels)
}

