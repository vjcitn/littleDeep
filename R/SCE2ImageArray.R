#' convert assay data from SCE to ImageArray
#' @param sce SingleCellExperiment instance
#' @param classvec vector of labels
#' @param dims dimensions of a single image
#' @param vfun a function to be applied to rows of assay(sce) to measure feature-wise 
#' variation in expression across cells
#' @param \dots not used
#' @export
SCE2ImageArray = function(sce, classvec, dims=c(32,32,3), vfun=rowSds, ...) {
  am = as.matrix(assay(sce))
  vmeas = vfun(am)
  nfeat = prod(dims)
  kp = head(order(vmeas, decreasing=TRUE), nfeat)
  sce = sce[kp,]
  am = am[kp,]
  nim = ncol(sce)
  outarr = array(1, dim=c(nim, dims))
  for (i in seq_len(nim)) outarr[i,,,] = as.numeric(am[,i])
  new("ImageArray", arr=outarr, types=classvec)
}
 
