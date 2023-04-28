pipvec = c("keras==2.12.0", "tensorboard==2.12.2",
"tensorflow==2.12.0", "tensorflow-estimator==2.12.0",
"nvidia-cublas-cu12==12.1.3.1",
"nvidia-cuda-runtime-cu12==12.1.105", "nvidia-cudnn-cu12==8.9.0.131",
"tensorrt==8.6.0")


' @import basilisk
bsklenv <- basilisk::BasiliskEnvironment(
  envname = "bsklenv",
  pkgname = "littleDeep",
  packages = c("numpy==1.23.1", "pandas==1.4.4"),
  pip = pipvec
)

