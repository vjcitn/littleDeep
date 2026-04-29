#' from Claude, to help reduce noise in Apr 2026 usage
#' @export
suppress_keras_warnings <- function() {
  reticulate::py_run_string("
import warnings
warnings.filterwarnings('ignore', category=FutureWarning)
warnings.filterwarnings('ignore', category=DeprecationWarning)
warnings.filterwarnings('ignore', message='Some donated buffers were not usable')
warnings.filterwarnings('ignore', message='Donation is not implemented')

import logging
logging.getLogger('jax').setLevel(logging.ERROR)
logging.getLogger('absl').setLevel(logging.ERROR)
  ")
}
