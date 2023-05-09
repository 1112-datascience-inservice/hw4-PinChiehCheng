library(shiny)
library(ca)
library(ggbiplot)
library(plotly)
library(DT)
library("FactoMineR")
library("factoextra")

data(iris)

shinyApp(
  ui = fluidPage(
    titlePanel(title = h3("PCA / CA for Iris dataset", align="center")),
    tabsetPanel(
      type = "tabs",
      tabPanel("PCA",
               tabsetPanel(
                 tabPanel("plot",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("xPC", "X-axis principal component",
                                           choices = c("PC1", "PC2", "PC3", "PC4"),
                                           selected = "PC1"),
                              radioButtons("yPC", "Y-axis principal component",
                                           choices = c("PC2", "PC3", "PC4"),
                                           selected = "PC2")
                            ),
                            mainPanel(
                              plotOutput("pcaPlot", height = "600px", width = "800px")
                            )
                          )
                 ),
                 tabPanel("result",
                          sidebarLayout(
                            sidebarPanel(
                              h4("PCA Parameters"),
                              br(),
                              p("The PCA analysis below was performed on the standardized Iris dataset using the prcomp() function in R. The principal components (PCs) are calculated from the covariance matrix of the variables."),
                              br(),
                              p("The number of principal components is determined by the number of variables in the dataset. For the Iris dataset, there are four variables, so there will be four principal components."),
                              br(),
                              p("The rotation matrix shows how the original variables (sepal length, sepal width, petal length, and petal width) are related to the principal components."),
                              br(),
                              p("The standard deviations of the principal components show how much of the total variation in the data is accounted for by each principal component.")
                            ),
                            mainPanel(
                              # Output the PCA rotation matrix as a table
                                h5("Rotation Matrix"),
                                  p(DT::dataTableOutput("results"),
                                br(),
                                  tableOutput("sdTable")
                                )
                              )
                            )
                          )
                 )
               ),
      tabPanel("CA",
               tabsetPanel(
                 tabPanel("Biplot",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("showRow", "Show row", TRUE),
                              checkboxInput("showCol", "Show column", TRUE),
                            ),
                            mainPanel(
                              plotOutput("biplot", height = "600px", width = "800px")
                            )
                          )
                 ),
                 tabPanel("row",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType", "Quality / Contributions",
                                           choices = c("cos2", "contrib"), selected = "cos2")
                            ),
                            mainPanel(
                              plotOutput("rowPlot", height = "600px", width = "800px")
                            )
                          )
                 ),
                 tabPanel("column",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType2", "Quality / Contributions",
                                           choices = c("cos2", "contrib"), selected = "cos2")
                            ),
                            mainPanel(
                              plotOutput("colPlot", height = "600px", width = "800px")
                            )
                          )
                 ),
                 tabPanel("eig.val",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxInput("add_line", "Show average eigenvalue", value = FALSE)
                            ),
                            mainPanel(
                              plotOutput("screeplot", height = "600px", width = "600px")
                            )
                          )
                 )
               )
      ),
      tabPanel("Dataset",
               tabsetPanel(
                 tabPanel("Scatter",
                          sidebarPanel(
                            selectInput(inputId = "x_axis",
                                        label = "X-axis variable",
                                        choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                                        selected = "Sepal.Length"),
                            selectInput(inputId = "y_axis",
                                        label = "Y-axis variable",
                                        choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                                        selected = "Sepal.Width"),
                            checkboxInput(inputId = "add_rug", label = "Add Rug", value = FALSE)
                          ),
                          mainPanel(
                            plotlyOutput(outputId = "iris_plot")
                          )
                 ),
                 tabPanel("Histogram",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var", "Variable", choices =c("Sepal.Length" = 1, "Sepal.Width" = 2, "Petal.Length" = 3, "Petal.Width" = 4), selected = 1),
                              br(),
                              sliderInput("bins", "Number of BINs", min = 5, max = 100, value = 50),
                              br(),
                              radioButtons("color", "Color", choices = c("rosybrown", "steelblue", "palevioletred"), selected = "rosybrown")
                            ),
                            mainPanel(
                              plotOutput("histogram")
                            )
                          )
                 ),
                 tabPanel("data", sidebarPanel(
                   h4("Iris Dataset"),
                   br(),
                   p("The Iris dataset consists of measurements of the sepal length, sepal width, petal length, and petal width for three different species of iris flowers."),
                   br(),
                   selectInput("species", "Specie", choices = unique(iris$Species), selected = "setosa")
                 ),
                 mainPanel(
                   dataTableOutput("data")
                 ))
               )
               
      ))),
  
  
  
  server <- function(input, output, session) {
    # Load data
    data(iris)
    head(iris, 3)
    
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE.-------------------------- 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE, tolerance = 0.01)
    
    # output the PCA rotation matrix
    output$results <- DT::renderDataTable(
      ir.pca$rotation,
      class = 'cell-border stripe',
      rownames = TRUE,
      options = list(pageLength = 5, scrollX = TRUE, lengthChange = FALSE, info = FALSE, searching = FALSE, paging = FALSE)
    )
    
    # update the yPC radioButtons options based on the current xPC selection
    observe({
      updateRadioButtons(session, "yPC", choices = setdiff(c("PC1", "PC2", "PC3", "PC4"), input$xPC))
    })
    
    output$pcaPlot <- renderPlot({
      
      # set x_choice based on input$xPC selection
      switch(input$xPC,
             "PC1" = x_choice <- 1,
             "PC2" = x_choice <- 2,
             "PC3" = x_choice <- 3,
             "PC4" = x_choice <- 4
      )
      
      # set y_choice based on input$yPC selection
      switch(input$yPC,
             "PC1" = y_choice <- 1,
             "PC2" = y_choice <- 2,
             "PC3" = y_choice <- 3,
             "PC4" = y_choice <- 4
      )
      
      # create ggbiplot object with chosen principal components
      g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species, ellipse = TRUE, choices = c(x_choice, y_choice)) +
        scale_color_discrete(name = '') +
        theme(legend.direction = 'horizontal', legend.position = 'top') +
        coord_cartesian(xlim = c(min(ir.pca$x[, x_choice]), max(ir.pca$x[, x_choice])),
                        ylim = c(min(ir.pca$x[, y_choice]), max(ir.pca$x[, y_choice])))
      
      print(g)
      g
    })
    
    output$sdTable <- renderTable({
      data.frame(`Standard deviation` = ir.pca$sdev)
    })
    
    # CA--------------------------
    
    res.ca <- CA(iris[, -5], graph = FALSE)
    
    # Biplot
    output$biplot <- renderPlot({
      p <- fviz_ca_biplot(res.ca, col.col = if(input$showCol) "deeppink3" else NA, 
                          shape.row = 15, col.row = if(input$showRow) "steelblue" else NA, repel = TRUE) +
        ggtitle("")
      print(p)
    })
    
    # row quality and contributions
    output$rowPlot <- renderPlot({
      if (input$plotType == "cos2") {
        p <- fviz_ca_row(res.ca, col.row = "cos2",
                         gradient.cols = c("powderblue", "steelblue", "palevioletred"),
                         shape.row = 15,
                         repel = TRUE) +
          ggtitle("")
      } else if (input$plotType == "contrib") {
        p <- fviz_ca_row(res.ca, col.row = "contrib",
                         gradient.cols = c("#00AFBB", "pink1", "deeppink4"), 
                         shape.row = 15,
                         repel = TRUE) +
          ggtitle("")
      }
      print(p)
    })
    
    # column quality and contributions
    output$colPlot <- renderPlot({
      if (input$plotType2 == "cos2") {
        p <- fviz_ca_col(res.ca, col.col = "cos2",
                         gradient.cols = c("skyblue", "steelblue", "mediumorchid4"),
                         repel = TRUE) +
          ggtitle("")
      } else if (input$plotType2 == "contrib") {
        p <- fviz_ca_col(res.ca, col.col = "contrib",
                         gradient.cols = c("#00AFBB", "pink1", "deeppink4"), 
                         repel = TRUE) +
          ggtitle("")
      }
      print(p)
    })
    
    # screeplot
    output$screeplot <- renderPlot({
      p <- fviz_screeplot(res.ca, addlabels = TRUE) 
      if (input$add_line) {
        p <- p + geom_hline(yintercept=33.33, linetype=2, color="red")
      }
      p <- p + ggtitle("")
      print(p)
    })

    # dataset-------------------------- 
    output$data <- DT::renderDataTable({
      # Filter data based on selected Species
      iris_sub <- iris[iris$Species == input$species, ]
      # Return filtered data
      iris_sub
    })
    
    # scatter-------------------------- 
    output$iris_plot <- renderPlotly({
      p <- ggplot(data = iris, aes_string(x = input$x_axis, y = input$y_axis, color = "Species")) +
        geom_point() +
        theme_classic() +
        labs(x = input$x_axis, y = input$y_axis)
      
      if (input$add_rug) {
        p <- p + geom_rug(sides = "trbl")
      }
      
      ggplotly(p)
    })
    
    # histogram
    output$histogram <- renderPlot({
      colm <- as.numeric(input$var)
      hist(iris[,colm], breaks=seq(0, max(iris[,colm]), l=input$bins+1), col=input$color, main="", xlab=names(iris[colm]), xlim=c(0,max(iris[,colm])))
    })
  }
)

