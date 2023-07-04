#
#
#library(littleDeep)
#library(abind)
##set.seed(1234)
#
#NIMAGES=2500
#
#build_array = function( shapefun = rancirc, nimages=NIMAGES, sidelength=64, depth=3, npts=90, ... ) {
# tmp = array(1, dim=c(nimages,sidelength,sidelength,depth)) # store images with 0 for dark
# for (i in seq_len(nimages)) {
#    z = shapefun(npts=npts, ...)
#    p = load_jpeg(z$x, z$y, siz=sidelength, dim=c(sidelength,sidelength,3))
#    tmp[i,,,] = p
#    }
#  tmp
#}
# 
#
#circarr = build_array( rancirc, sidelength=32, side_plane=32, npts=120 )
#triarr = build_array( rantri, sidelength=32, side_plane=32, npts=40 )
#quadarr = build_array( ranquad, sidelength=32, side_plane=32, npts=30 )
#
#allsh = abind(circarr, triarr, along=1)
#allsh = abind(allsh, quadarr, along=1)
#
#dim(allsh)
#
#par(ask=TRUE)
#shapes = ImageArray(allsh, types=rep(c("circle", "triangle", "quad"), each=NIMAGES))
#
#test_circarr = build_array( rancirc, sidelength=32, side_plane=32, npts=120 )
#test_triarr = build_array( rantri, sidelength=32, side_plane=32, npts=40 )

#test_quadarr = build_array( ranquad, sidelength=32, side_plane=32, npts=30 )
#
#test_allsh = abind(test_circarr, test_triarr, along=1)
#test_allsh = abind(test_allsh, test_quadarr, along=1)
#
#test_shapes = ImageArray(test_allsh, types=rep(c("circle", "triangle", "quad"), each=NIMAGES))
#
#
#' use the CNN in Introduction to Statistical Learning with R on an ImageArray instance
#' @param iarr ImageArray instance, must have 32x32x3 images
#' @param nEpochs numeric(1) used in call to fit for keras_model_sequential
#' @param batchSize numeric(1) used in call to fit for keras_model_sequential
#' @param valSplit numeric(1) used in call to fit for keras_model_sequential
#' @return list with components model (compiled model) and history (of fit)
#' @export
islr_cnn = function(iarr, nEpochs=30, batchSize=128, valSplit=.2) {
    stopifnot(inherits(iarr, "ImageArray"))
    arr = getArray(iarr) # may be large
    d = dim(arr)[-1]
    stopifnot(all(d==c(32,32,3)))
    denom = max(arr)  # scale values to max value 1
# convert input instance to cifar-data like list
    yclass = getTypes(iarr)
    yclass = as.numeric(factor(yclass))
    yclass = yclass - min(yclass) # to zero
    trainxy = list(train=list(x = arr, y=yclass))
    ncat = length(unique(trainxy$train$y))
# following is exactly as in ISLR except nEpochs and batchSize and valSplit are parameters
    model <- keras_model_sequential() %>% layer_conv_2d(filters = 32, 
        kernel_size = c(3, 3), padding = "same", activation = "relu", 
        input_shape = c(32, 32, 3)) %>% layer_max_pooling_2d(pool_size = c(2, 
        2)) %>% layer_conv_2d(filters = 64, kernel_size = c(3, 
        3), padding = "same", activation = "relu") %>% layer_max_pooling_2d(pool_size = c(2, 
        2)) %>% layer_conv_2d(filters = 128, kernel_size = c(3, 
        3), padding = "same", activation = "relu") %>% layer_max_pooling_2d(pool_size = c(2, 
        2)) %>% layer_conv_2d(filters = 256, kernel_size = c(3, 
        3), padding = "same", activation = "relu") %>% layer_max_pooling_2d(pool_size = c(2, 
        2)) %>% layer_flatten() %>% layer_dropout(rate = 0.5) %>% 
        layer_dense(units = 512, activation = "relu") %>% layer_dense(units = ncat, 
        activation = "softmax")
    model %>% compile(loss = "categorical_crossentropy", optimizer = optimizer_rmsprop(), 
        metrics = c("accuracy"))
    history <- model %>% fit(trainxy$train$x/denom, to_categorical(trainxy$train$y, 
        ncat), epochs = nEpochs, batch_size = batchSize, validation_split = valSplit)
    list(model = model, history = history)
}

#' estimate accuracy of fitted model predictions for a given ImageArray
#' @param fitted islr_cnn model instance
#' @param iarr ImageArray instance
#' @return accuracy estimate
#' @export
eval_model = function(model, iarr) {
 yclass = getTypes(iarr)
 yclass = as.numeric(factor(yclass))
 yclass = yclass - min(yclass) # to zero
 accuracy <- function(pred, truth)
   mean(drop(as.numeric(pred)) == drop(truth))
 testPreds <- model %>% predict(getArray(iarr)) %>% k_argmax()
 testPreds %>% accuracy(yclass)
}

#' report estimated model properties for a given ImageArray
#' @param fitted islr_cnn model instance
#' @param iarr ImageArray instance
#' @return vector of fitted class probabilities
#' @examples
#' litsh = make_shape_iarr(nimages=3)
#' preview(litsh)
#' smod = load_shape_cnn()
#' model_probs(smod, litsh)
#' @export
model_probs = function(model, iarr, roundto=3) {
 yclass = getTypes(iarr)
 yclass = as.numeric(factor(yclass))
 yclass = yclass - min(yclass) # to zero
 accuracy <- function(pred, truth)
   mean(drop(as.numeric(pred)) == drop(truth))
 omat = model %>% predict(getArray(iarr)) 
 omat = apply(omat, 2, round, roundto)
 ty = typelevels(iarr)
 ans = data.frame(matrix(omat, nc=length(ty)))
 names(ans) = ty # factor was used in build
 ans
}


#' make an array of JPEG-like data for invocations of a (presumably random) shape-generating function
#' @param shapefun a function producing a sidelength x sidelength binary image
#' @param nimages numeric(1) number of images
#' @param sidelength numeric(1) passed to load_jpeg as siz
#' @param depth numeric(1) defaults to 3
#' @param npts numeric(1) passed to shapefun
#' @examples
#' circarr = build_array( rancirc, sidelength=32, side_plane=32, npts=120 )
#' dim(circarr)
#' @export
build_array = function( shapefun = rancirc, nimages=2500, sidelength=64, depth=3, npts=90, ... ) {
 tmp = array(1, dim=c(nimages,sidelength,sidelength,depth)) # store images with 0 for dark
 for (i in seq_len(nimages)) {
    z = shapefun(npts=npts, ...)
    p = load_jpeg(z$x, z$y, siz=sidelength, dim=c(sidelength,sidelength,3))
    tmp[i,,,] = p
    }
  tmp
}


#' produce an ImageArray of circles, triangles, quadrilaterals
#' @import abind
#' @param nimages numeric(1)
#' @export
make_shape_iarr = function(nimages=2500) {
  circarr = build_array( rancirc, nimages=nimages, sidelength=32, side_plane=32, npts=120 )
  triarr = build_array( rantri, nimages=nimages, sidelength=32, side_plane=32, npts=40 )
  quadarr = build_array( ranbox, nimages=nimages, sidelength=32, side_plane=32, npts=30 )

  allsh = abind(circarr, triarr, along=1)
  allsh = abind(allsh, quadarr, along=1)

  ImageArray(allsh, types=rep(c("circle", "triangle", "quad"), each=nimages))
}

#' load shape-trained CNN
#' @examples
#' load_shape_cnn()
#' @export
load_shape_cnn = function() {
  keras::load_model_hdf5(system.file("extdata", "shapemod.h5", package="littleDeep"))
}
