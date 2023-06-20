
#library(littleDeep)
#myjpc = array(1, dim=c(100,64,64,3))
#for (i in 1:100) { z = rancirc(); p = load_jpeg(z$x, z$y, siz=sample(c(20,40,60),size=1)); myjpc[i,,,] = p }
#plot(0,0,type="n", xlim=c(0,1), ylim=c(0,1))
#for (i in 1:50) {Sys.sleep(.4); r = writeJPEG(myjpc[i,,,]); z = readJPEG(r); 
#    rasterImage(z, 0,0, 1, 1, interpolate=FALSE) }

#' make a discrete image of a circle
#' @param center_x numeric(1) defaults to 30
#' @param center_y numeric(1) defaults to 30
#' @param radius numeric(1) defaults to 15
#' @param npts numeric(1) number of points to generate using `seq(0, 2*pi, length.out=npts)`
#' @examples
#' plot(makecirc())
#' @export
makecirc = function(center_x=30, center_y=30, radius=15, npts=100) {
   ai = as.integer
   angs = seq(0,2*pi,length.out=npts)
   list(x=ai(radius*cos(angs)+center_x), y=ai(radius*sin(angs)+center_y))
}

#' produce coordinates for a randomly positioned circle in a square plane
#' @param side_plane numeric(1) length of side
#' @param npts numeric(1) number of points to plot in discrete plane
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
#' for (i in seq_len(30)) { lines(rancirc()); sample(c(20,40,60), size=1) }
#' #z = load_jpeg(rc$x, rc$y, siz=333, dim=c(400,400,3))
#' #graphics::rasterImage(z, 0, 0, 1, 1, interpolate=FALSE)
#' @export
rancirc = function(side_plane = 64, npts=100, max_radius=NULL) {
  radius = sample(seq(1, floor(side_plane/2)), size=1)
  if (!is.null(max_radius)) while(radius > max_radius) {  # redo
        radius = sample(seq(1, floor(side_plane/2)), size=1)
        }
  hix = side_plane - radius - 1
  lox = 0 + radius + 1
  hiy = side_plane - radius - 1
  loy = 0 + radius + 1
# length 1 inputs to sample are trouble
  if (lox!=hix) x = sample(seq(lox, hix), size=1) else x = lox
  if (loy!=hiy) y = sample(seq(loy, hiy), size=1) else y = loy
  makecirc(x, y, radius, npts)
}
