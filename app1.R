
library(shiny)
library(arules)
library(arulesViz)
library(DT)


#setwd("C:/Users/Karen/Desktop/1040R/Project1")
uitems <- readRDS("uitems.Rds")
baskets <- readRDS("baskets.Rds")

ui <- fluidPage(
  headerPanel("What would most likely be bought next?"),
  sidebarPanel( 
    selectInput("test", "What was the item purchased?", 
                c("PARTY BUNTING", "LUNCH BAG CARS BLUE", "LUNCH BAG WOODLAND", "SPOTTY BUNTING", 
                  "JUMBO BAG OWLS", "WOODEN REGATTA BUNTING", "METAL SIGN", "BIRTHDAY CARD", 
                  "VICTORIAN SEWING KIT", "SPACE BOY MINI BACKPACK", "RED HARMONICA IN BOX", "RETRO SPOT", 
                  "CARD BILLBOARD FONT", "POTTERING MUG", "BACK DOOR", "10 COLOUR SPACEBOY PEN", 
                  "RED DINER WALL CLOCK", "GARAGE KEY FOB","POLKADOT PEN", "SWALLOWS GREETING CARD", 
                  "FRONT DOOR", "COAL BLACK", "6 ROCKET BALLOONS", "GIRAFFE WOODEN RULER", 
                  "SET OF 10 LED DOLLY LIGHTS"), selected = NULL),
    p("Select the item that has been bought.
      Items tab will show the items that would most likely be bought and its number of occurences.
      Associations Rules Graph shows a graphical representation of the rules for the item.
      Visualization tab shows how the lift, support and confidence level that the next item would be bought.")
    ),
  mainPanel(
    tabsetPanel(
      tabPanel("Items",plotOutput("toprules")),
      tabPanel("Association Rules Graph",plotOutput("grp")),
      tabPanel("Visualization", plotOutput("result"))
    )
  )
    )


server <- function(input, output) {
  
  library(shiny)
  library(arules)
  library(arulesViz)
  library(ggplot2)
  
  test_item <- reactive({
    input$test
  })
  
  rules_1 <- reactive({
    sort(apriori(baskets, parameter=list(supp=0.001,conf = 0.2), 
                 appearance = list(lhs=input$test, default="rhs"), 
                 control = list(verbose=F)),decreasing = TRUE, by="confidence")
  })
  
  dtrules_1 <- reactive(data.frame(inspect(rules_1())))
  c1 <- reactive(dtrules_1()$rhs)
  c2 <- reactive(dtrules_1()$count)
  dtitems <- reactive({ggplot(data=dtrules_1(), aes(x=dtrules_1()$rhs, y=dtrules_1()$count, fill=rhs)) + 
              geom_col()+theme(axis.text.x = element_blank())})
              
  
  output$toprules <- renderPlot(
    dtitems()
  )
  
  p1 <-  reactive({plot(rules_1(), method="graph", cex=0.75)})
  
  output$grp <- renderPlot({
    p1()
  })
  
  
  g1 <- reactive({plot(rules_1(), measure = c("support", "lift"), shading = "confidence")})
  
  output$result <- renderPlot({
    g1()
  })
}


shinyApp(ui=ui, server=server)
