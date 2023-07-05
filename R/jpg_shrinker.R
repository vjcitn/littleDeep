
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
    plotOutput("given", width="800px"),
    plotOutput("shrunk", width="800px"),
    verbatimTextOutput("pred")
   )
  )
 )
 server = function(input, output) {
  mod = littleDeep::load_shape_cnn()
  output$pred = renderPrint({
    model_probs(mod, ImageArray(process_jpg(input$jpeg$datapath), types="given", typelevels="given"))
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


  
    
  
