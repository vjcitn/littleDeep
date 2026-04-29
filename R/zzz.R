
.onLoad <- function(libname, pkgname) {
  # Suppress XLA/C++ stderr logs (Metal device info, XLA service init)
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "3")
  Sys.setenv(GRPC_VERBOSITY        = "ERROR")
  Sys.setenv(JAX_LOG_COMPILES      = "0")
  
  reticulate::py_require("keras==3.13.0")
  reticulate::py_require("h5py==3.14.0")
  
  if (Sys.info()[["sysname"]] == "Darwin" && 
      Sys.info()[["machine"]] == "arm64") {
    # Apple Silicon — use JAX backend with Metal
    reticulate::py_require("jax==0.4.35")
    reticulate::py_require("jax-metal==0.1.1")
    Sys.setenv(KERAS_BACKEND = "jax")
  } else {
    # Linux/NVIDIA — use TF backend (CUDA handled by system)
    reticulate::py_require("tensorflow==2.21.0")
    Sys.setenv(KERAS_BACKEND = "tensorflow")
  }
}
