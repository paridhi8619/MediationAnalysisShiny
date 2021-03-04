# install.packages("devtools")
library(devtools)
# install_github("easyGgplot2", "kassambara")
# install_github("kassambara/easyGgplot2")
library(shiny)
library(mediation)
library(DT)
library(ggplot2)
# library(easyGgplot2)
library(plyr)
fileData <- read.csv("C:/Users/paridhij747/Documents/Lilly/LillyShiny/data/mediation_data.csv")

ui <- fluidPage(
  titlePanel("Mediation Analysis Data Viz and Simulation"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "indVar",choices = c("itch", "BSA", "redness"), 
                  selected = "itch", label = "Choose the mediating variable"),
      # DT::dataTableOutput("table"),
      plotOutput("plot1")
    ),
    mainPanel(
      plotOutput("distPlot"),
      verbatimTextOutput(outputId = "RegSum")
      
      
    )
  )
  
)

server <- function(input, output) {
  b <- lm(formula = DLQI ~ as.factor(TRT), data = fileData)
  lm1 <- reactive({
     lm(reformulate(termlabels = c('as.factor(TRT)', input$indVar), response="DLQI"), data=fileData)
  })
  contcont1 <- reactive({
    contcont <- mediate(b, lm1(), sims=100, treat="as.factor(TRT)", mediator=input$indVar)
  })
  output$plot1 <- renderPlot({
    # browser()
   
    
    # print(class(input$indVar))
    # if(input$indVar == "itch")
    #   c <- lm(formula = DLQI ~ binTrt + itch, data=fileData)
    # 
    # else if(input$indVar == "BSA")
    #   c <- lm(formula = DLQI ~ binTrt + BSA, data=fileData)
    # 
    # else
    #   c <- lm(formula = DLQI ~ binTrt + redness, data=fileData)
    # c <- lm1()
    # contcont <- mediate(b, c, sims=100, treat="binTrt", mediator=input$indVar)
    # 
    # print(class(summary(contcont)))
    plot(contcont1())
  })
   output$RegSum <- renderPrint({
     # c <- lm1()
   # 
   # contcont <- mediate(b, c, sims=100, treat="binTrt", mediator=input$indVar)
   summary(contcont1())
   })
   output$distPlot <- renderPlot({
     mu <- ddply(fileData, "TRT", summarise, grp.mean=mean(DLQI))
     head(mu)
     ggplot(fileData, aes(x=DLQI, color=TRT, fill=TRT)) +
       geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
       geom_density(alpha=0.6)+
       geom_vline(data=mu, aes(xintercept=grp.mean, color=TRT),
                  linetype="dashed")+
       scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
       scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
       labs(title="Treatment histogram plot",x="DLQI", y = "Density")+
       theme_classic()
   })
}

shinyApp(ui = ui, server = server)
# shiny::runApp(display.mode="showcase")


