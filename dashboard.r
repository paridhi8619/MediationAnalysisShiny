packages <- c("shiny", "shinydashboard", "dplyr", "DT", "shinyBS", "ggplot2", "mice", "shinydashboardPlus", "mediation")
install.packages(setdiff(packages, rownames(installed.packages()))) 
 

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(shinyBS)
library(ggplot2)
library(mice)
library(shinydashboardPlus)
library(mediation)
library(simstudy)
# library(plyr)
options(digits=5)
fileData <- read.csv("C:/Users/paridhij747/Documents/Lilly/LillyShiny/data/mediation_data.csv")

ui <- dashboardPage(skin="red",
                    dashboardHeader(title = h4(HTML("Mediation Analysis<br/> Data Viz and Simulation"))
                                    ),
                    dashboardSidebar(
                      sidebarMenu(
                         menuItem("Data", tabName = "Data", icon = icon("server")),
                        # menuItem("Data Summary", tabName = "Summary", icon = icon("table")),
                          menuItem("Mediation Analysis", tabName = "Results", icon = icon("chart-line")),
                          menuItem("Simulation", tabName = "simulation", icon = icon("sync"))
   
                      ),
                      column(10,align = "center", tags$img(src = "PwC_Outline_Logo_White.png",width="70%", style= "margin-top: 350px; "))
                      
                    ),
                     dashboardBody(
                       
                      tabItems(
                        #First tab content
                        tabItem(tabName = "Data",
                                # fluidRow(
                                #   # infoBox(value = tags$p(style = "font-weight: bold;",35),"Good Batch Count", icon=icon("thumbs-up"), color="green", width = 3),
                                #   # infoBox(value = tags$p(style = "font-weight: bold;",15),"Bad Batch Count", icon=icon("thumbs-down"), color="red", width = 3)
                                #   infoBoxOutput("GoodBatchCount"),
                                #   infoBoxOutput("BadBatchCount")
                                #),
                                #uncomment
                                fluidRow(
                                  DT::dataTableOutput("table"), style = "overflow-x: scroll;"
                                )
                                # fluidRow( uiOutput("modals")),
                                # fluidRow( DT::dataTableOutput("table2"))

                        ),
                    #     
                    #     tabItem(tabName = "Summary",
                    #             # fluidRow(
                    #             #   verbatimTextOutput("summaryTable"), style = "overflow-x: scroll; overflow-y: scroll;" 
                    #             # )
                    #             # fluidRow(
                    #             #   verbatimTextOutput("summaryTableTitle")
                    #             #   #format("Batch wise summary of variables", justify = "centre") 
                    #             # ),
                    #             # fluidRow(
                    #             #   #  verbatimTextOutput("summaryTable"), style = "overflow-x: scroll; overflow-y: scroll;" 
                    #             #   DT::dataTableOutput("summaryTable"),options = list(pageLength = 20)
                    #             #   # style = "overflow-x: scroll;"
                    #             # )
                    #     ),
                        tabItem(tabName = "Results",
                                fluidRow(column(6,
                                                selectInput(inputId = "indVar",choices = c("itch", "BSA", "redness"), 
                                                            selected = "itch", label = "Choose the mediating variable:"),
                                                plotOutput("distPlot")
                                ),
                                  column(6,plotOutput("plot1"),
                                         verbatimTextOutput(outputId = "RegSum"))
                                # column(5,plotOutput("impFeatPlot"))
                                # column(5,plotOutput("impFeatPlot"),height = "20%")
                                )
                        ),
                    tabItem(tabName = "simulation",
                            fluidRow(column(3,
                                            textInput("seed", "Simulation seed",value = 12345)),
                                     column(3,
                                            textInput("count", "No. of observations", 50)),
                                     sliderInput("medItch", "Mediation%", 0.1, 3, 2.67)),
                            # fluidRow(column(3,
                            #                 sliderInput("itchMean", "Mean: itch", round(min(fileData$itch),2), round(max(fileData$itch), 2), value = "")),
                            #          column(3,
                            #                 sliderInput("BSAMean", "Mean: BSA", round(min(fileData$BSA), 2), round(max(fileData$BSA), 2), value = "")),
                            #          column(3,
                            #                 sliderInput("rednessMean", "Mean: redness", round(min(fileData$redness), 2), round(max(fileData$redness), 2), value = ""))),
                            # fluidRow(column(3,
                            #                 sliderInput("itchsd", "Mean: itch", round(min(fileData$itch),2), round(max(fileData$itch), 2), value = "")),
                            #          column(3,
                            #                 sliderInput("BSAsd", "Mean: BSA", round(min(fileData$BSA), 2), round(max(fileData$BSA), 2), value = "")),
                            #          column(3,
                            #                 sliderInput("rednesssd", "Mean: redness", round(min(fileData$redness), 2), round(max(fileData$redness), 2), value = ""))),
                            fluidRow(column(4, verbatimTextOutput("itchMedPerc"))),
                            fluidRow(column(4,
                                            plotOutput("itchPlot")),
                                     column(4,
                                            plotOutput("BSAPlot")),
                                     column(4,
                                            plotOutput("rednessPlot")))
                            
                            
                            )
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
  output$table <- DT::renderDataTable({
    fileData
  })
  
  
  
  # mdeItch <- reactive({
  #   input$medItch
  # })
  # seed <- reactive({
  #   input$seed
  # })
  simDataGen <- reactive({
    groupMeans <- aggregate(fileData[, c(2,4,6,8)], list(fileData$TRT), mean)
    cor <- cor(fileData[,c(2,4,6, 8)])
    sd <- aggregate(fileData[, c(2,4,6,8)], list(fileData$TRT), sd)
    num <- as.numeric(input$count)
    num <- num * 2
    print(num)
    simData <- genCorData(num, mu = c(groupMeans$itch[1], groupMeans$BSA[1], groupMeans$redness[1], groupMeans$DLQI[1]), 
                          sigma = c(sd$itch[1], sd$BSA[1], sd$redness[1], sd$DLQI[1]), corMatrix = cor)
    simData$V2[simData$V2 < min(fileData$BSA)] <-  min(fileData$BSA)
    simData$V2[simData$V2 > max(fileData$BSA)] <-  max(fileData$BSA)
    
    simData$V3[simData$V3 < min(fileData$redness)] <-  min(fileData$redness)
    simData$V3[simData$V3 > max(fileData$redness)] <-  max(fileData$redness)
    simData <- as.data.frame(simData[, c(3,4)])
    names(simData) <- c("BSA", "redness")
    simData
  })
 
  
  medDataGen <- reactive({
    muPlacebo <- 5.925
    muTrt <- 5.925 - 1.437 
    
    sePlacebo <- 0.232
    seTrt <- sqrt(0.232^2/120 + 0.328^2/120)
    set.seed(input$seed)
    count <- input$count
    dataPlacebo <- data.frame("itch" = rnorm(count, muPlacebo, sePlacebo), "TRT" = "Placebo")
    dataTrt <- data.frame("itch" = rnorm(count, muTrt, 0.328), "TRT" = "Rx")
    finalData <- rbind(dataPlacebo, dataTrt)
    random2=runif(nrow(finalData),min=min(finalData$itch),max=max(finalData$itch))
    finalData$DLQI=finalData$itch*input$medItch+random2*0.65
    finalData
  })
  
  itchReactive <- reactive({
    # muPlacebo <- 5.925
    # muTrt <- 5.925 - 1.437 
    # 
    # sePlacebo <- 0.232
    # seTrt <- sqrt(0.232^2/120 + 0.328^2/120)
    # set.seed(input$seed)
    # count <- input$count
    # dataPlacebo <- data.frame("itch" = rnorm(count, muPlacebo, sePlacebo), "TRT" = "Placebo")
    # dataTrt <- data.frame("itch" = rnorm(count, muTrt, 0.328), "TRT" = "Rx")
    # finalData <- rbind(dataPlacebo, dataTrt)
    # random2=runif(nrow(finalData),min=min(finalData$itch),max=max(finalData$itch))
    # finalData$DLQI=finalData$itch*input$medItch+random2*0.65
    # finalData$BSA = 
    medDataGen <- medDataGen()
    b <- lm(formula = DLQI ~ as.factor(TRT), data = medDataGen)
    c <- lm(formula = DLQI ~ as.factor(TRT) + itch, data=medDataGen)
    itchMed <- mediate(b, c, sims=100, treat="as.factor(TRT)", mediator="itch")
    itchMed
  })
  mergeData <- reactive({
    df1 <- simDataGen()
    df2 <- medDataGen()
    df <- cbind(df1, df2)
    df
  })
  BSAReactive <- reactive({
    df <- mergeData()
    print(head(df))
    b <- lm(formula = DLQI ~ as.factor(TRT), data = df)
    c <- lm(formula = DLQI ~ as.factor(TRT) + BSA, data=df)
    BSAMed <- mediate(b, c, sims=100, treat="as.factor(TRT)", mediator="BSA")
    BSAMed
    
  })
  rednessReactive <- reactive({
    # browser()
    df <- mergeData()
    b <- lm(formula = DLQI ~ as.factor(TRT), data = df)
    c <- lm(formula = DLQI ~ as.factor(TRT) + redness, data=df)
    rednessMed <- mediate(b, c, sims=100, treat="as.factor(TRT)", mediator="redness")
    rednessMed
  })
  output$itchPlot <- renderPlot({
      plot(itchReactive())
  })
  output$BSAPlot <- renderPlot({
    plot(BSAReactive())
  })
  output$rednessPlot <- renderPlot({
    plot(rednessReactive())
  })
  output$itchMedPerc <- renderText({
    paste("Proportions mediated = ", round(itchReactive()$n0, 2), "%")
  })
  
 
  
}

shinyApp(ui, server)