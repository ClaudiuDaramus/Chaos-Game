library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tags$h2("Chaos Game"),
      style = "margin-top: 75px;",
      tags$p(),
      # input
      selectInput(inputId = "n", label = "Form:",
                  c("Triangle" = 3, "Square" = 4, "Pentagon" = 5, "Hexagon" = 6)),
      actionButton(inputId = "go", label = "Update"),
      tags$p(),
      selectInput(inputId = "r", label = "R:",
                  c(0.9, "3/4" = 0.75, "2/3" = 2/3, 0.6, "1/2" = 1/2, 0.4, "1/3" = 1/3, "1/4" = 1/4)),
      actionButton(inputId = "do1", label = "One Step"),
      actionButton(inputId = "do2", label = "100 Steps"),
      actionButton(inputId = "do3", label = "1000 Steps"),
      actionButton(inputId = "do4", label = "5000 Steps")
    ),
    mainPanel(
      plotOutput("chaosGame")
    )
  )
)

server <- function(input, output) {
  triangle.points = data.frame(x = c(0, 1, 0.5), y = c(0, 0, 1))
  square.points = data.frame(x = c(0, 0, 1, 1), y = c(0, 1, 0, 1))
  pentagon.points = data.frame(x = c(0.2, 0.8, 0.5, 0, 1), y = c(0, 0, 1, 0.6, 0.6))
  hexagon.points = data.frame(x = c(0.5, 0.5, 0 , 0, 1, 1), y = c(0, 1, 0.3, 0.7, 0.3, 0.7)) 
  
  current.point = reactiveVal(round(runif(2),2))
  r = reactive({as.numeric(input$r)})
  n = reactive({as.numeric(input$n)})
  # initial este triunghi (3 varfuri)
  show.points = reactiveVal(triangle.points)
  nrVf = reactiveVal(3)
  
  addShowPoints <- function() {
    # triungi
    if (n() == 3)
    {
      show.points(triangle.points)
      nrVf(3)
    }
    # patrat
    else if (n() == 4)
    {
      show.points(square.points)
      nrVf(4)
    }
    # pentagon
    else if (n() == 5)
    {
      show.points(pentagon.points)
      nrVf(5)
    }
    # hexagon
    else if (n() == 6)
    {
      show.points(hexagon.points)
      nrVf(6)
    }
  }
  
  # functie pentru adaugarea unui punct nou
  addEv <- function() {
    vf = sample(nrVf(),1)
    diferentaX = abs(show.points()[vf,]$x - current.point()[1]) * r()
    diferentaY = abs(show.points()[vf,]$y - current.point()[2]) * r()
    if (show.points()[vf,]$x > current.point()[1])
    {
      newX = current.point()[1] + diferentaX
    }
    else {
      newX = current.point()[1] - diferentaX
    }
    
    if (show.points()[vf,]$y > current.point()[2])
    {
      newY = current.point()[2] + diferentaY
    }
    else {
      newY = current.point()[2] - diferentaY
    }
    
    # coordonatele noului punct
    current.point ( c(newX, newY) )
    
    # adaugam noul punct in data.frame-ul show.points
    newPoints = show.points()
    newPoints[nrow(newPoints) + 1,] = current.point()
    show.points(newPoints)
  }
  
  observeEvent(input$go, {
    addShowPoints()
  })
  observeEvent(input$do1, {
    addEv()
  })
  observeEvent(input$do2, {
    for(i in 1:100)
    { addEv() }
  })
  observeEvent(input$do3, {
    for(i in 1:1000)
    { addEv() } 
  })
  observeEvent(input$do4, {
    for(i in 1:5000)
    { addEv() } 
  })
  
  
  output$chaosGame <- renderPlot({
    plot(show.points(), xlim=c(0,1), ylim=c(0,1), axes=FALSE, yaxt = "n",
         xaxt = "n", frame.plot=TRUE, pch=20, ann=FALSE, asp=1, cex = 0.95)
  },height = 500, width = 500)
}

shinyApp(ui = ui, server = server, options = list(height = 500))