library(countcolors)
library(jpeg)
library(shiny)

ui <- fluidPage(
  titlePanel("SVD of a 219 x 210 pixel image"), 
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("k", "Number of singular values: ",
                  min = 2, max = 210, step = 1, value = 210, 
                  animate = animationOptions(interval = 300, loop = TRUE))
    ), 
    mainPanel(
      plotOutput("finalPlot")
    )
  )
)

jpg <- jpeg::readJPEG("doraemon.jpg")

r <- jpg[,,1]
g <- jpg[,,2]
b <- jpg[,,3]
svd_r <- svd(r)
svd_g <- svd(g)
svd_b <- svd(b)

server <- function(input, output){
  new_matrix <- reactive({
    k <- as.numeric(input$k)
    sapply(list(svd_r, svd_g, svd_b), function(SVD){
      A <- SVD$u[,1:k] %*% diag(SVD$d[1:k]) %*% t(SVD$v[,1:k])
      # Due to floating point error, kind of messes up the image with negative values and values over 1.
      # Try to improve orthogonality of U and V?
      A <- replace(A, A < 0, 0)
      A <- replace(A, A > 1, 1)
    }, simplify = 'array')
  })
  
  output$finalPlot <- renderPlot({
    countcolors::plotArrayAsImage(new_matrix())
  })
}

shinyApp(ui, server)