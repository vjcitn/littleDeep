
#' container for 4D image array (index,x,y,z), typically (,32,32,3)
#' @export
setClass("ImageArray", representation(arr="array", types="character"))

#' display information about ImageArray instance
#' @export
setMethod("show", "ImageArray", function(object) {
  dims = dim(object@arr)
  cat(sprintf("ImageArray instance with %d images, each %d x %d x %d\n", dims[1],
      dims[2], dims[3], dims[4]))
  cat(" Image types: ")
  cat(Biobase::selectSome(unique(object@types)), "\n")
  rng = range(object@arr)
  r3 = function(x) round(x,3)
  cat(sprintf(" Array elements range from %f to %f.\n", r3(rng[1]), 
     r3(rng[2])))
})

#' extract numerical array
#' @export
setGeneric("getArray", function(iarr) standardGeneric("getArray"))
#' extract numerical array
#' @export
setMethod("getArray", "ImageArray", function(iarr) slot(iarr, "arr"))
#' extract vector of labels
#' @export
setGeneric("getTypes", function(iarr) standardGeneric("getTypes"))
#' extract vector of labels
#' @export
setMethod("getTypes", "ImageArray", function(iarr) slot(iarr, "types"))

#' constructor for ImageArray
#' @param arr 4D array
#' @param types character()
#' @export
ImageArray = function(arr, types) {
  stopifnot(length(types) == dim(arr)[1])
  new("ImageArray", arr=arr, types=types)
}

#nnnn = abind(myjpc, myjpt, along=1)

#shapes = ImageArray(nnnn, types=rep(c("circle", "triangle"), each=N_PER_TYPE))

#shapes

#cifmeta = fromJSON(system.file("extdata", "cif.json", package="littleDeep"))
#labs = cifmeta[[1]]$features$fine_label$names
#if (!exists("cifdata")) cifdata = dataset_cifar100()
#ciftrain = cifdata[[1]]$x
#ciflabels = labs[cifdata[[1]]$y+1]
#
#train = ImageArray(ciftrain, ciflabels)
#train

#' produce labeled 3x3 array of images via plot.raster
#' @param iarr ImageArray instance
#' @param \dots passed to plot.raster and thence to rasterImage; `interpolate=TRUE` can be useful
#' @export
preview = function(iarr, ...) {
  par(mfrow=c(3,3), mar=c(2,2,2,4))
  mx = max(getArray(iarr))
  tys = getTypes(iarr)
  for (i in 1:9) {
    plot(as.raster(iarr@arr[i,,,], max=mx), ...)
    title(tys[i])
  }
  NULL
}

#' return an ImageArray with images restricted to specific types
#' @param iarr ImageArray instance
#' @param tvec character vector of types to retain
#' @export
filterByType = function(iarr, tvec) {
 tys = getTypes(iarr)
 stopifnot(all(tvec %in% tys))
 inds = which(tys %in% tvec)
 new("ImageArray", arr=getArray(iarr)[inds,,,], types=tys[inds])
}

#' linearize data in an image to a single vector for each image in an 
#' ImageArray instance
#' @param iarr ImageArray instance
#' @export
flattenToMatrix = function (iarr) 
{
    thearr = getArray(iarr)
    dims = dim(thearr)
    mydatt = matrix(1, nrow = dims[1], ncol = prod(dims[-1]))
    for (i in seq_len(dims[1])) mydatt[i, ] = as.numeric(thearr[i, 
        , , ])
    mydatt
}

#' simple extraction with bracket
#' @export
setMethod("[", c("ImageArray", i="numeric",
    j="missing", drop="missing"), function(x,i,j,drop) {
  arr = getArray(x)
  stopifnot(i <= dim(arr)[1])
  ty = getTypes(x)
  new("ImageArray", arr=arr[i,,,], types=ty[i])
})

