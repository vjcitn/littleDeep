#' restore a fitted model for CIFAR100
#' @return a keras model reference
#' @note Metadata about the input data are available in extdata/cif.json.
#' @examples
#' mod = get_fitted_cifar100()
#' mod
#' @export
get_fitted_cifar100 = function() {
 load_model_hdf5(system.file("extdata", "cifar100.keras.h5", package="littleDeep"))
}

.get_fitted_cifar100 = function() {
# set up process
#    cl <- basiliskStart(ldeepenv,
#                        testload = "keras")
    ans = basilisk::basiliskRun(proc = cl,
                                  fun = .get_fitted_cifar100)
#    basiliskStop(cl)
    ans
}

