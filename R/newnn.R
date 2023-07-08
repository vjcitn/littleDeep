function (iarr, nEpochs = 1, batchSize = 128, valSplit = 0.2) 
{
    reticulate::import("keras")
    ca = match.call()
    stopifnot(inherits(iarr, "ImageArray"))
    arr = getArray(iarr)
    d = dim(arr)[-1]
    stopifnot(all(d == c(32, 32, 3)))
    denom = max(arr)
    yclass = getTypes(iarr)
    yclass = as.numeric(factor(yclass))
    yclass = yclass - min(yclass)
    trainxy = list(train = list(x = arr, y = yclass))
    ncat = length(unique(trainxy$train$y))
    model <- keras_model_sequential() %>% layer_conv_2d(filters = 32, 
        kernel_size = c(3, 3), padding = "same", activation = "relu", 
        input_shape = c(32, 32, 3))
    model %>% compile(loss = "categorical_crossentropy", optimizer = optimizer_rmsprop(), 
        metrics = c("accuracy"))
    history <- model %>% fit(trainxy$train$x/denom, to_categorical(trainxy$train$y, 
        ncat), epochs = nEpochs, batch_size = batchSize, validation_split = valSplit)
    curver = packageVersion("littleDeep")
    ans = list(model = model, history = history, typelevels = typelevels(iarr), 
        littleDeepVersion = curver, call = ca, date = Sys.Date())
    class(ans) = c("islr_cnn", "list")
    ans
}
