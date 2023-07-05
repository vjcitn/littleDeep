
#' build islr_cnn instance trained on cifar100
#' @param saveto character(1) passed to save_islr_cnn, folder where hdf5 and additional metadata are stored
#' @param nEpochs numeric(1) used in `islr_cnn`
#' @param \dots passed to `islr_cnn`
#' @export
train_cifar100 = function(saveto=tempfile(), nEpochs=25, ...) {
   cifmeta = jsonlite::fromJSON(system.file("extdata", "cif.json", package="littleDeep"))
   labs = cifmeta[[1]]$features$fine_label$names
   cifdata = dataset_cifar100()
   ciftrain = cifdata[[1]]$x
   ciflabels = labs[cifdata[[1]]$y+1]
   train = ImageArray(ciftrain, ciflabels)
   basic = islr_cnn(train, nEpochs=nEpochs, ...)
   save_islr_cnn(basic, saveto)
}
   
