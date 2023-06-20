
setClass("ImageArray", representation(arr="array", types="character"))
setMethod("show", "ImageArray", function(object) {
  dims = dim(object@arr)
  cat(sprintf("ImageArray instance with %d images, each %d x %d x %d\n", dims[1],
      dims[2], dims[3], dims[4]))
  cat(" Image types: ")
  cat(Biobase::selectSome(unique(object@types)), "\n")
  rng = range(object@arr)
  cat(sprintf(" Array elements range from %f to %f.\n", rng[1], rng[2]))
})
setGeneric("getArray", function(iarr) standardGeneric("getArray"))
setMethod("getArray", "ImageArray", function(iarr) slot(iarr, "arr"))
setGeneric("getTypes", function(iarr) standardGeneric("getTypes"))
setMethod("getTypes", "ImageArray", function(iarr) slot(iarr, "types"))

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

preview = function(iarr, ...) {
  par(mfrow=c(3,3), mar=c(2,2,2,4))
  mx = max(getArray(iarr))
  tys = getTypes(iarr)
  for (i in 1:9) {
    plot(as.raster(iarr@arr[i,,,], max=mx))
    title(tys[i])
  }
  NULL
}

filterByType = function(iarr, tvec) {
 tys = getTypes(iarr)
 inds = which(tys %in% tvec)
 new("ImageArray", arr=getArray(iarr)[inds,,,], types=tys[inds])
}

flattenToMatrix = function(iarr) {
  dims = dim(getArray(iarr))
  mydatt = matrix(1, nrow=dims[1], ncol=prod(dims[-1]))
  for (i in seq_len(dims[1])) mydatt[i,] = as.numeric(getArray(iarr[i,,,]))
  mydatt
}

