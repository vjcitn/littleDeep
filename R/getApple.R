#' get a high-resolution jpeg of an apple in ImageArray
#' @examples
#' ap1 = getApple()
#' ap1
#' plotOne(ap1)
#' @export
getApple = function() {
   apl = system.file("jpegs", "oneBigApple.jpg", package="littleDeep")
   j = readJPEG(apl)
   j = array(j, dim=c(1,dim(j)))
   ImageArray(j, types="apple", typelevels="apple")
}
