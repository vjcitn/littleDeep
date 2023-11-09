 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText("jpeg_shrinker for littleDeep"),
    radioButtons("model", "model", c("shapes", "cifar100", "cifar3")),
    fileInput("jpeg", "pick a jpeg"), width=2
   ),
   mainPanel(
    tabsetPanel(
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
