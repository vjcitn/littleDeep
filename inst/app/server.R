library(littleDeep)
 server = function(input, output) {
  options(shiny.maxRequestSize=30*1024^2) 
  reticulate::import("h5py")
  get_model = reactive({
    if (input$model == "shapes")
       modstuff = littleDeep::restore_islr_cnn(system.file("extdata", "shapemodf", package="littleDeep"))
    else if (input$model == "cifar100")
       modstuff = littleDeep::restore_islr_cnn(system.file("extdata", "cifrex", package="littleDeep"))
    else if (input$model == "cifar3")
       modstuff = littleDeep::restore_islr_cnn(system.file("extdata", "cif3", package="littleDeep"))
    modstuff
    })
  output$modtxt = renderPrint({
    get_model()
    })
  output$pred = renderPrint({
    req(input$jpeg)
    tmp = process_jpg(input$jpeg$datapath)
    arr = tmp@.Data #EBImage
    arr = array(arr, dim=c(1,32,32,3))
    modstuff = get_model()
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
