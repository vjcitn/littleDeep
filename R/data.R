#' a history object for keras fit for cifar100 data, using the ISLR tuning settings
#' @docType data
#' @format list
#' @note just saved the history() output for a keras fit
#' @examples
#' data(cifar100_history)
#' plot(cifar100_history) # will use keras
"cifar100_history"

#' random collection of 2000 images from the test data of the cifar100 (100 categories) dataset
#' @docType data
#' @format ImageArray
#' @examples
#' data(ciftest2k)
#' preview(ciftest2k) # will use keras
"ciftest2k"

#' random collection of 1000 images from the training data of the cifar100 (100 categories) dataset
#' @docType data
#' @format ImageArray
#' @examples
#' data(ciftrain1k)
#' preview(ciftrain1k) # will use keras
"ciftrain1k"
