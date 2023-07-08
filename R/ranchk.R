#' display a random cifar100 test image and its classification
#' @export
rancif = function() {
     data("ciftest2k", package="littleDeep")
     ind = sample(1:2000, size=1)
     im = ciftest2k[ind]
     modstuff = littleDeep::restore_islr_cnn(system.file("extdata", "cifrex", package="littleDeep"))
     mp = model_probs(modstuff$model, im)
     top10 = order(as.numeric(data.matrix(mp[1,])), decreasing=TRUE)[1:10]
     plot(as.raster(im@arr[1,,,]/255))
     mp[,top10]
}

