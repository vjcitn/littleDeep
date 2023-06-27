

 
#' produce a list of x and y coordinates of "discrete lines" with
#' specified numbers of points, forming a quadrilateral with vertices given
#' by input coordinates
#' @param x1 numeric(1)
#' @param y1 numeric(1)
#' @param x2 numeric(1)
#' @param y2 numeric(1)
#' @param x3 numeric(1)
#' @param y3 numeric(1)
#' @param x4 numeric(1)
#' @param y4 numeric(1)
#' @param npts numeric(1) tells number of points to fill for each side
#' @note chull is used to ensure sides do not cross
#' @examples
#' str(makequad())
#' @export
makequad = function(x1=4, y1=4, x2=10, y2=10, x3=11, y3=14, x4=15, y4=0, npts=30, make_open=TRUE) {
   pts = list(x=c(x1,x2,x3,x4), y=c(y1,y2,y3,y4))
   ch = chull(pts)
   last = setdiff(seq_len(4), ch)
   ch = c(ch,last)
   xs = c(x1,x2,x3,x4)[ch]
   ys = c(y1,y2,y3,y4)[ch]
   l1 = getline(xs[1],ys[1],xs[2],ys[2],npts=npts)
   l2 = getline(xs[2],ys[2],xs[3],ys[3],npts=npts)
   l3 = getline(xs[3],ys[3],xs[4],ys[4],npts=npts)
   l4 = getline(xs[4],ys[4],xs[1],ys[1],npts=npts)
   tmp = list(l1,l2,l3,l4)
   list(x=c(tmp[[1]]$x, tmp[[2]]$x, tmp[[3]]$x, tmp[[4]]$x ),
      y=c(tmp[[1]]$y, tmp[[2]]$y, tmp[[3]]$y, tmp[[4]]$y ))
}

scale_to_k = function(x, k=64) {
 n = (x-min(x))/(max(x)-min(x))
 (k-1)*n + 1
}
 
#' produce coordinates of a random "quadrilateral" in 64 x 64 plane
#' @param npts numeric(1) passed to `maketri`
#' @param side_plane numeric(1) length of square side for rendering surface
#' @examples
#' set.seed(2345)
#' plot(0,0,type="n", xlim=c(1,64), ylim=c(1,64))
#' for (i in seq_len(30)) { lines(ranquad()) }
#' @export
ranquad = function(npts=50, side_plane=64) {
  co = sample(seq_len(side_plane), size=8)
  makequad( x1=co[1], y1=co[2], x2=co[3], y2=co[4], x3=co[5], y3=co[6], x4=co[7], y4=co[8], npts=npts)
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



#' @param max_radius numeric(1) defaults to NULL, must be less than side_plane
#' @examples
#' set.seed(1234)
#' myjpc = array(1, dim=c(30,64,64,3)) # store images with 0 for dark
#' for (i in seq_len(30)) { 
#'    z = rancirc() 
#'    p = load_jpeg(z$x, z$y, siz=sample(c(20,40,60),size=1)) 
#'    myjpc[i,,,] = p 
#'    }
#' plot(0,0,type="n",xlim=c(1,64), ylim=c(1,64))
#' set.seed(1234)
#' for (i in seq_len(30)) { lines(ranquad()); sample(c(20,40,60), size=1) }
#' #z = load_jpeg(rc$x, rc$y, siz=333, dim=c(400,400,3))
#' #graphics::rasterImage(z, 0, 0, 1, 1, interpolate=FALSE)
