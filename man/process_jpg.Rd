% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_jpg.R
\name{process_jpg}
\alias{process_jpg}
\title{import, resize and rotate a JPEG that should have been produced in square mode}
\usage{
process_jpg(fn, w = 32, h = 32, angle = 0)
}
\arguments{
\item{fn}{path to JPEG file}

\item{w}{output width in pixels}

\item{h}{output width in pixels}

\item{angle}{in degrees, magnitude of rotation for EBImage::rotate}
}
\description{
import, resize and rotate a JPEG that should have been produced in square mode
}
\examples{
appl = system.file("jpegs", "oneBigApple.jpg", package="littleDeep")
im = process_jpg(appl, angle=90)
plot(grDevices::as.raster(im))
}
