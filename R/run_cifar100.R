#' run the ISLR CNN for CIFAR100 data
#' @import keras
#' @param nEpochs numeric(1) defaults to 30
#' @param batchSize numeric(1) defaults to 128
#' @param valSplit numeric(1) defaults to 0.2
#' @return a list with elements history and model
#' @note Takes 15+ minutes on a CPU, with default settings.  This
#' is derived from Introduction to Statistical Learning with R by
#' G. James, D. Witten, T. Hastie, R. Tibshirani.
#' @export
run_cifar100 = function(nEpochs=30, batchSize=128, valSplit=.2) {
cifar100 = dataset_cifar100()
model <- keras_model_sequential() %>%
  layer_conv_2d(
    filters = 32, kernel_size = c(3, 3),
    padding = "same", activation = "relu",
    input_shape = c(32, 32, 3)
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(
    filters = 64, kernel_size = c(3, 3),
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(
    filters = 128, kernel_size = c(3, 3),
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(
    filters = 256, kernel_size = c(3, 3),
    padding = "same", activation = "relu"
  ) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_flatten() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dense(units = 100, activation = "softmax")

 model %>% compile ( loss = "categorical_crossentropy" ,
    optimizer = optimizer_rmsprop () , metrics = c ( "accuracy" ) )
 history <- model %>% fit ( cifar100$train$x/255 , to_categorical(cifar100$train$y,100) , 
      epochs = nEpochs ,
      batch_size = batchSize , validation_split = valSplit)
 list(model=model, history=history)
}

