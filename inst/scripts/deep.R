## ----setup,message=FALSE,echo=FALSE-------------------------------------------
library(littleDeep)


## ----getkeras, message=FALSE--------------------------------------------------
library(keras)
cifar100 <- dataset_cifar100()
names(cifar100)
names(cifar100$train)
length(cifar100$train$y)
length(cifar100$test$y)


## ----getmeta,message=FALSE----------------------------------------------------
library(jsonlite)
cifmeta = fromJSON(system.file("extdata", "cif.json", package="littleDeep"))
labs = cifmeta[[1]]$features$fine_label$names
head(labs)


## ----lkdat--------------------------------------------------------------------
mypl = function(x, use="train", ...) 
   plot(as.raster(cifar100[[use]][["x"]][x,,,], max=255), ...)
labinds = cifar100[[1]][[2]]+1
par(mfrow=c(5,5), mar=c(0,0,3,0))
for (i in 1:25) { mypl(i); title(labs[labinds[i]]) }


## ----getmod-------------------------------------------------------------------
model = load_model_hdf5(system.file("extdata", "cifar100.keras.h5", package="littleDeep"))
model


## ----lkhist-------------------------------------------------------------------
data(cifar100_history)
cifar100_history
plot(cifar100_history)


## ----lkacc--------------------------------------------------------------------
accuracy <- function(pred, truth)
  mean(drop(as.numeric(pred)) == drop(truth))
testPreds <- model %>% predict(cifar100$test$x) %>% k_argmax()
testPreds %>% accuracy(cifar100$test$y)
kp <- which(as.numeric(testPreds) == cifar100$test$y)[1:25]


## ----morepl-------------------------------------------------------------------
par(mfrow=c(5,5), mar=c(0,0,3,0))
for (i in kp) {mypl(i, use="test"); title(labs[cifar100$test$y+1][i])}

