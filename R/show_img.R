#' produce raster image of a jpg file
#' @import jpeg
#' @param fn character(1) path to jpg
#' @param main character(1) title defaults to 'jpeg'
#' @param \dots passed to graphics::rasterImage
#' @return rasterImage is called
#' @examples
#' apl = system.file("jpegs", "oneBigApple.jpg", package="littleDeep")
#' show_jpg(apl)
#' @export
show_jpg = function(fn, main="jpeg", ...) {
 img = jpeg::readJPEG(fn)
 plot(as.raster(img), main=main)
}

#' a magick-based solution to presenting jpg data, includes rotation capability
#' @importFrom magick image_read image_rotate image_ggplot
#' @param fn character(1) path to jpg
#' @param main character(1) title defaults to 'jpeg'
#' @examples
#' apl = system.file("jpegs", "oneBigApple.jpg", package="littleDeep")
#' show_jpg(apl)
#' @export
show_jpg2 = function (fn, main = "jpeg", degrees = 0, ...) 
{
    library(ggplot2)
    img = magick::image_read(fn)
    img = magick::image_rotate(img, degrees = degrees)
    magick::image_ggplot(img) + ggtitle(main)
}

 

show_channels = function(fn, ...) {
 img = jpeg::readJPEG(fn)
 opar = par(no.readonly=TRUE)
 on.exit(par(opar))
 par(mfrow=c(2,2))
 show_jpg(fn, main="full")
 for (i in 1:3) {
   plot(0,0, ylim=c(0,1), xlim=c(0,1), type="n", axes=FALSE, main=sprintf("plane %d", i))
   graphics::rasterImage(keep_plane(img, i), 0, 0, 1, 1)
 }
}

monochromize = function(fn, img=NULL, thresh, rel=">",
   kill_boundary = TRUE, darkval=.02, ...) {
 if (is.null(img)) img = jpeg::readJPEG(fn)
 kill = do.call(rel, list(img, thresh))
 d = dim(img)
 img[which(kill)] = 1
 img[-which(kill)] = darkval
 img = array(img, dim=d)
 if (kill_boundary) {
   img[1,,] = img[64,,] = 1
   img[,1,] = img[,64,] = 1
   }
 img
}
 
#' app to demonstrate filtering of a jpeg
#' @import shiny
#' @export
chkthres = function() {
if (!requireNamespace("OpenImage")) stop("install OpenImage to use this package")
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(width=2,
    helpText("pick a jpg"),
    fileInput("jpg", " "),
    helpText("set decoloring threshold"),
    sliderInput("thresh", " ", min=0, max=1, value=.3, step=.05, animate=TRUE),
    sliderInput("angle", " ", min=0, max=360, value=0, step=5, animate=TRUE),
    downloadButton("downloadData", "Save transformed jpeg")
    ),
   mainPanel(
    tabsetPanel(
     tabPanel("view", plotOutput("view", width="450px", height="450px")),
     tabPanel("init", plotOutput("view2", width="450px", height="450px"))
    )
   )
  )
 )
 server = function(input, output) {
  getimg = reactive({
   fn = input$jpg
   validate(need(!is.null(fn), "pick a file"))
   print("reading")
   img = jpeg::readJPEG(fn$name)
   img
   })
  output$view = renderPlot({
   img = getimg()
   if (input$angle != 0) img = OpenImageR::rotateImage(img, input$angle, mode="same")
   mc = monochromize(img=img, thresh=input$thresh)
   d = dim(mc)
   mc[mc< .02] = 1
   mc = array(mc,dim=d)
   graphics::rasterImage(mc, 0,0,1,1)
   })
  output$view2 = renderPlot({
   img = getimg()
   graphics::rasterImage(img, 0,0,1,1)
   })
  output$downloadData <- shiny::downloadHandler(
     filename = function() {
         paste("txjpeg-", Sys.Date(), ".jpg", sep="")
         },
     content = function(file) {
           img = getimg()
           if (input$angle != 0) img = OpenImageR::rotateImage(img, input$angle, mode="same")
           mc = monochromize(img=img, thresh=input$thresh)
   d = dim(mc)
   mc[mc< .02] = 1
   mc = array(mc,dim=d)
           jpeg::writeJPEG(mc, file)
         }
       )
  }
 runApp(list(ui=ui, server=server))
}
 



#  ui <- fluidPage(
#       downloadButton("downloadData", "Download")
#     )
#     
#     server <- function(input, output) {
#       # Our dataset
#       data <- mtcars
#     
#     }

