
library(shiny)

setwd("C:/Users/Karen/Desktop/1040R/Project1")
uitems <- readRDS("uitems.Rds")
baskets <- readRDS("baskets.Rds")

ui <- fluidPage(
        headerPanel("What would most likely be bought next?"),
        sidebarPanel( 
          selectInput("test", "What was the item purchased?", uitems, selected = NULL),
          p("Select the item that has been bought.
            Data tab will show the list of items that would most likely be bought next.
            Associations Rules Graph shows a graphical representation of the rules for the item.
            Visualization tab compares the support, confidence and lift of the items.")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Data",dataTableOutput("toprules")),
            tabPanel("Association Rules Graph",plotOutput("grp")),
            tabPanel("Visualization", plotOutput("result"))
          )
        )
      )
  

server <- function(input, output) {
            
            library(arules)
            library(arulesViz)
  
            test_item <- reactive({
              input$test
            })
              
            rules_1 <- reactive({
              sort(apriori(baskets, parameter=list(supp=0.001,conf = 0.2), 
              appearance = list(lhs=input$test, default="rhs"), 
              control = list(verbose=F)),decreasing = TRUE, by="confidence")
            })
            
            dtrules_1 <- reactive({inspect(rules_1())})
            
            output$toprules <- renderDataTable({
              dtrules_1()
            })
            
            p1 <-  reactive({plot(rules_1(), method="graph", cex=0.5)})
            
            output$grp <- renderPlot({
              p1()
            })
            
            library(ggplot2)
            library(ggpubr)
            
            g1 <- reactive({ggplot(data=dtrules_1(), aes(x=rhs, y=support, fill=rhs)) + 
                geom_bar(stat="identity") + guides(fill=FALSE)})
            g2 <- reactive({ggplot(data=dtrules_1(), aes(x=rhs, y=confidence, fill=rhs)) + 
                geom_bar(stat="identity")+ guides(fill=FALSE)})
            g3 <- reactive({ggplot(data=dtrules_1(), aes(x=rhs, y=lift, fill=rhs)) + 
                geom_bar(stat="identity")})
            
            grules <- reactive({ggarrange(g1()+ rremove("x.text"), g2()+ rremove("x.text"), 
                                g3() + rremove("x.text"), ncol=1,nrow=4)})
                  
            output$result <- renderPlot({
              grules()
            })
        }
            

shinyApp(ui=ui, server=server)
