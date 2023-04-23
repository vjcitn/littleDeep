#' run the ISLR CNN for CIFAR100 data
#' @import keras
#' @return a list with elements history and model
#' @note Takes 15+ minutes on a CPU
#' @export
run_cifar100 = function() {
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
 history <- model %>% fit ( cifar100$train$x/255 , to_categorical(cifar100$train$y,100) , epochs = 30 ,
      batch_size = 128 , validation_split = 0.2)
 list(model=model, history=history)
}

