
#' app for uploading and transforming a square jpeg
#' @import shiny
#' @export
jpeg_shrinker = function() {
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText("jpeg_shrinker for littleDeep"),
    fileInput("jpeg", "pick a jpeg"), width=2
   ),
   mainPanel(
    tabSetPanel(
     tabPanel("figs",
      plotOutput("given", width="800px"),
      plotOutput("shrunk", width="800px"),
      verbatimTextOutput("pred")
      ),
     tabPanel("about", verbatimTextOutput("modtxt"))
    )
   )
  )
 )
 server = function(input, output) {
  options(shiny.maxRequestSize=30*1024^2) 
  modstuff = littleDeep::restore_islr_cnn(system.file("extdata", "shapemodf", package="littleDeep"))
  output$modtxt({
    renderPrint(modstuff)
    })
  output$pred = renderPrint({
    tmp = process_jpg(input$jpeg$datapath)
    arr = tmp@.Data #EBImage
    arr = array(arr, dim=c(1,32,32,3))
    iarr = ImageArray(arr, types="given", typelevels=modstuff$typelevels)
    model_probs(modstuff$model, iarr)
  })
  output$given = renderPlot({
   req(input$jpeg)
   validate(need(file.exists(input$jpeg$datapath),"please choose a jpg from your disk"))
   littleDeep::show_jpg(input$jpeg$datapath, interpolate=FALSE)
   }, width=900)
  output$shrunk = renderPlot({
   req(input$jpeg)
   validate(need(file.exists(input$jpeg$datapath),"please choose a jpg from your disk"))
   tmp = littleDeep::process_jpg(input$jpeg$datapath)
   plot(as.raster(tmp), interpolate=FALSE)
   }, width=900)
  }
 runApp(list(ui=ui, server=server))
}


  
    
  
