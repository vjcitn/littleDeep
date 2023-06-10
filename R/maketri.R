#' produce sequence of points on a line between two points
#' @param x1 numeric
#' @param y1 numeric
#' @param x2 numeric
#' @param y2 numeric
#' @param npts number of points to return
#' @return list with elements x, y
#' @export
getline = function(x1, y1, x2, y2, npts=20) {
   m = (y2 - y1)/(x2-x1)
   xx = seq(x1, x2, length.out=npts)
   lin = function(x) y1 + m*(x-x1)
   list(x=xx, y=lin(xx))
}
 
maketri = function(x1=4, y1=4, x2=10, y2=10, x3=6, y3=14, npts=20) {
   l1 = getline(x1, y1, x2, y2, npts=npts)
   l2 = getline(x1, y1, x3, y3, npts=npts)
   l3 = getline(x2, y2, x3, y3, npts=npts)
   list(x=c(l1$x, l2$x, l3$x), y=c(l1$y, l2$y, l3$y))
}

scale_to_k = function(x, k=64) {
 n = (x-min(x))/(max(x)-min(x))
 (k-1)*n + 1
}
 
load_jpeg = function(x, y, siz=64, dim=c(64,64,3)) {
  stopifnot(length(x)==length(y))
  ans = array(1,dim=dim)
  sx = round(scale_to_k(x, k=siz))
  sy = round(scale_to_k(y, k=siz))
  for (i in 1:3) {
    for (j in 1:length(x)) { 
      ans[sx[j], sy[j], i] = 0
      }
    }
  ans
}

#tr = maketri()
#
#library(raster)
#rasterImage(www<- load_jpeg(tr$x, tr$y, siz=32), 0, 0, 1, 1, interpolate=FALSE)
#writeJPEG(www, "lk1.jpg")


#' produce a raster image of a random "triangle" in 64 x 64 plane
#' @import raster
#' @export
rantri = function() {
  co = sample(1:64, size=6)
  t = maketri( x1=co[1], y1=co[2], x2=co[3], y2=co[4], x3=co[5], y3=co[6])
  raster::rasterImage(load_jpeg(t$x, t$y, siz=sample(c(20,40,60), size=1)), 0, 0, 1, 1, interpolate=FALSE)
}
#
#   m1 = (y2 - y1)/(x2-x1)
#   xx = seq(x1, x2, length.out=20)
#   l1 = function(x) y1 + m1*(x-x1)
#   yy = y1 + m1*xx
#   m2 = (y3 - y2) / (x3 - x2)
#   xxx = seq(x2, x3, length.out=20)
#   yyy = y1 + m2*xxx
#   l2 = function(x) y2 + m2*(x-x2)
#   m3 = (y3-y1)/(x3-x1)
#   l3 = function(x) y3 + m3*(x-x3)
#   xxxx = seq(x1, x3, length.out=20)
#   plot(c(xx, xxx, xxxx), c(l1(xx), l2(xxx), l3(xxxx)), pch=" ")
#   lines(xx, l1(xx))
#   lines(xxx, l2(xxx))
#   lines(xxxx, l3(xxxx))
#   points(c(x1,x2,x3), c(y1,y2,y3), pch=19, col="gold")
##   X = seq(-50,50)
##   plot(c(X, X, X), c(l1(X), l2(X), l3(X)), pch=" ", xlim=c(0,20), ylim=c(0,20))
##   lines(X, l1(X), col="green", lty=1,lwd=3)
##   lines(X, l2(X), col="red", lty=1,lwd=3)
##   lines(X, l3(X), col="blue", lty=1,lwd=3)
##   points(c(x1,x2,x3), c(y1,y2,y3), pch=19, col="gold")
#}
