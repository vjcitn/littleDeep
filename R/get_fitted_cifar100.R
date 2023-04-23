#' restore a fitted model for CIFAR100
#' @return a keras model reference
#' @export
get_fitted_cfar100 = function() {
 load_model_hdf5(system.file("extdata", "cifar100.keras.h5", package="littleDeep"))
}
