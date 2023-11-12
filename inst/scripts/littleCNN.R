library(littleDeep)
library(jsonlite)

if (!exists("cifdata")) cifdata = dataset_cifar100()

names(cifdata)

dim(cifdata[["train"]][["x"]])

head(table(cifdata[["train"]][["y"]]))

cifmeta = fromJSON(system.file("extdata", "cif.json", package="littleDeep"))
labs = cifmeta[[1]]$features$fine_label$names
head(labs)

ciftrain = cifdata[[1]]$x
ciflabels = labs[cifdata[[1]]$y+1]

train = ImageArray(ciftrain, ciflabels)
train

preview(train)

ciftest = cifdata[["test"]]$x
ciftestlabs = labs[cifdata[["test"]]$y+1]
test = ImageArray(ciftest, ciftestlabs)
test

head(table(ciftestlabs))

littrain = filterByType(train, c("boy", "rose", "chair"))
littest = filterByType(test, c("boy", "rose", "chair"))
littrain

preview(littrain)

args(islr_cnn)

tr1 = islr_cnn(littrain, nEpochs=50)
tr1

plot(tr1$history)

model_probs(tr1$model, littest[1:9])

confmat = function(trained_model, arr) {
 stopifnot(inherits(trained_model, "islr_cnn"))
 mp = model_probs(trained_model$model, arr)
 pred = trained_model$typelevels[ apply( data.matrix(mp),
   1, which.max) ]
 given = arr@types
 table(given=given, pred=pred)
}


confmat(tr1, littest)

tm = tempfile()
mpath = save_islr_cnn(tr1, tm)
library(rhdf5)
h5ls(mpath)

dir(tm)

ll = restore_islr_cnn(tm)

ll

ll$model
