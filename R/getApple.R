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
#' get path to apple image
#' @export
applePath = function() {
   system.file("jpegs", "newapple.jpg", package="littleDeep")
}

#' get path to flower image
#' @export
flowerPath = function() {
   system.file("jpegs", "newpurple.jpg", package="littleDeep")
}

#' get path to activated mast cell image
#' @note downloaded from http://cellimagelibrary.org/images/222
#' @export
mastPath = function() {
   system.file("jpegs", "mast.jpg", package="littleDeep")
}

#' produce an ImageArray for any single jpeg
#' @param jpgpath character(1) path
#' @param type character(1) type label
#' @examples
#' ia = jpg2ImageArray(applePath())
#' plotOne(ia) # slow
#' @export
jpg2ImageArray = function(jpgpath, type="type not set") {
   jdat = jpeg::readJPEG(jpgpath)
   num = array(jdat, dim=c(1,dim(jdat)))
   ImageArray(num, types=type, typelevels=type)
}
