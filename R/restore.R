restore = function(model="cifar3") {
   input = list()
   input$model = model
   if (input$model == "shapes")
       modstuff = littleDeep::restore_islr_cnn(system.file("extdata", "shapemodf", package="littleDeep"))
    else if (input$model == "cifar100")
       modstuff = littleDeep::restore_islr_cnn(system.file("extdata", "cifrex", package="littleDeep"))
    else if (input$model == "cifar3")
       modstuff = littleDeep::restore_islr_cnn(system.file("extdata", "cif3", package="littleDeep"))
    modstuff
}

