
# necessary for python module control
#' @import basilisk
#ldeepenv <- basilisk::BasiliskEnvironment(envname="ldeepenv",
#    pkgname="littleDeep",
#    packages=c("keras==2.12.0", "tensorflow==2.12.0", "h5py==3.6.0"))
#
#    cl <- basiliskStart(orthosenv,
#                        testload = "tensorflow")
#    LATC <- basilisk::basiliskRun(proc = cl,
#                                  fun = .predictEncoder,
#                                  organism = organism,
#                                  gene_input = C)
#    if (verbose) {
#        message("Encoding and decoding contrasts...")
#    }
#    res <- basilisk::basiliskRun(proc = cl,
#                                 fun = .predictEncoderD,
#                                 organism = organism,
#                                 delta_input = D, context = LATC)
#    basilisk::basiliskStop(cl)

#' try to persist python refs
#' @export
#start_ldpy = function() basiliskStart(ldeepenv, testload="keras")
