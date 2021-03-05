packages <- c("shiny", "shinydashboard", "dplyr", "DT", "shinyBS", "ggplot2", "mice", 
              "shinydashboardPlus", "mediation", "plyr")
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
library(plyr)
library(readr)
library(diagram)
options(digits=5)

fileData <- read.csv("./data/mediation_data.csv")

ui <- dashboardPage(skin="red",
                    dashboardHeader(title = h4(HTML("Mediation Analysis<br/> Data Viz and Simulation"))
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Data", tabName = "Data", icon = icon("server")),
                        menuItem("About", tabName = "about", icon = icon("info")),
                        menuItem("Mediation Analysis", tabName = "Results", icon = icon("chart-line")),
                        menuItem("Simulation", tabName = "simulation", icon = icon("sync"))
                        
                      ),
                      column(10,align = "center", tags$img(src = "PwC_Outline_Logo_White.png",width="70%", style= "margin-top: 350px; "))
                      
                    ),
                    dashboardBody(
                      
                      tabItems(
                        #First tab content
                        tabItem(tabName = "Data",
                                fluidRow(
                                  DT::dataTableOutput("table"), style = "overflow-x: scroll;"
                                )
                                
                        ),
                        tabItem(tabName = "about",
                          h2("About data"),
                          p("The data was simulated based on a parallel study design studying a treatment vs placebo. Treatment variable,
                            TRT, is encoded as \"Rx\" for the experimental treatment and \"placebo\" for placebo. There were 120 patients
                            randomized to each arm. The endpoint of interest is a patient reported outcome, DLQI, at 24 weeks. DLQI
                            ranges from 0, ..., 30, the lower score the better. Possible mediator variables are:"),
                          tags$li("itch: Patient self report this measure every day in a diary. The measurements range from 0, ..., 10 and
                          are averaged every week to give a weekly measure. The lower the score the better. The average of the
                          measurements taken the week prior to week 24 is provided in this data set."
                                              # uiOutput('list')
                          ),
                          tags$li("redness: Patient self report this measure every day in a diary. The measurements range from 0, ..., 10
                          and are averaged every week to give a weekly measure. The lower the score the better. The average of
                          the measurements taken the week prior to week 24 is provided in this data set."),
                          tags$li("BSA: Measured by the physician at each visit. The measurements range from 0, ..., 100%. The lower
                        the score, the better. The body surface area reported at week 24 is provided in this data set.")
                          ),
                       
                        tabItem(tabName = "Results",
                                fluidRow(column(6,
                                                selectInput(inputId = "indVar",choices = c("itch", "BSA", "redness"), 
                                                            selected = "itch", label = "Choose the mediating variable:"),
                                                plotOutput("distPlot"),
                                                plotOutput('blockDiag')
                                ),
                                column(6,plotOutput("plot1"),
                                       verbatimTextOutput(outputId = "RegSum"))
                                )
                        ),
                        tabItem(tabName = "simulation",
                                fluidRow(column(3,
                                                textInput("seed", "Simulation seed",value = 12345)),
                                         column(3,
                                                textInput("count", "No. of observations", 50)),
                                         column(3, downloadButton("downloadData", "Save Simulated Data")),
                                         column(3, downloadButton("downloadmissingData", "Save Simulated missing Data")),
                                         sliderInput("medItch", "Mediation%", 0, 1, 0.25)), #0.1, 3, 2.67
                                                               fluidRow(column(4, verbatimTextOutput("itchMedPerc"))),
                                        fluidRow(column(4,
                                                plotOutput("itchPlot")),
                                         column(4,
                                                plotOutput("BSAPlot")),
                                         column(4,
                                                plotOutput("rednessPlot"))),
                                fluidRow(plotOutput(("senstivityPlot")))
                                
                                
                        )
                      )
                    )
                    
)

server <- function(input, output) {
  
  # b <- lm(formula = itch ~ TRT, data = fileData)
  # print(summary(b))
  lm1 <- reactive({
    mod <- lm(reformulate(termlabels = c('TRT', input$indVar), response="DLQI"), data=fileData)
    print(summary(mod))
    mod
  })
  lm2 <- reactive({
    mod <- lm(reformulate(termlabels = c('TRT'), response=input$indVar), data=fileData)
    print(summary(mod))
    mod
  })
  contcont1 <- reactive({
    contcont <- mediate(lm2(), lm1(), sims=100, treat='TRT', mediator=input$indVar)
    print(summary(contcont))
    contcont
  })
  output$plot1 <- renderPlot({
     plot(contcont1(), effect.type = c("indirect", "direct", "total"), xlab = "Effect value", ylab = "Effect type", 
         main = "Mediation effect by itch", col="blue")
  })
  output$RegSum <- renderPrint({
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
      labs(title=("Plot showing Rx versus Placebo effect"),x="DLQI", y = "Density")+
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
  output$table <- DT::renderDataTable({
    fileData
  })
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
    count <- input$count
    dataPlacebo <- data.frame("itch" = rnorm(count, muPlacebo, sePlacebo), "TRT" = "Placebo")
    dataTrt <- data.frame("itch" = rnorm(count, muTrt, 0.328), "TRT" = "Rx")
    finalData <- rbind(dataPlacebo, dataTrt)
    set.seed(input$seed)
    random2=runif(nrow(finalData),min=min(finalData$itch),max=max(finalData$itch))
    finalData$DLQI=finalData$itch*input$medItch+random2*0.45
    finalData
  })
  
  itchReactive <- reactive({
    medDataGen <- medDataGen()
    b <- lm(formula = itch ~ as.factor(TRT), data = medDataGen)
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
  amputeData <- reactive({
    df <- mergeData()
    # amputedDf <- f %>%
    #   group_by(TRT) %>%
    #   group_map(~ ampute(.x))
    placeboDf <- subset(df[df$TRT == "Placebo",])
    RxDf <- placeboDf <- subset(df[df$TRT == "Rx",])
    
    placeboPercMissing <- c(24, 22, 34, 34)
    rxPercMissing <- c(18, 7, 15, 15)
    
    countMissingPlacebo <- round((placeboPercMissing * nrow(placeboDf))/100)
    countMissingRx <- round((rxPercMissing / nrow(RxDf))*100)
    
    # placeboDf1 <- ampute(placeboDf[,c(1, 3:5)], prop = 0.24)$amp
    placeboDf$itch[sample(1:nrow(placeboDf),countMissingPlacebo[1] , replace = FALSE)] <- NA
    placeboDf$BSA[sample(1:nrow(placeboDf),countMissingPlacebo[2] , replace = FALSE)] <- NA
    placeboDf$redness[sample(1:nrow(placeboDf),countMissingPlacebo[3] , replace = FALSE)] <- NA
    placeboDf$DLQI[sample(1:nrow(placeboDf),countMissingPlacebo[4] , replace = FALSE)] <- NA
    
    RxDf$itch[sample(1:nrow(RxDf),countMissingRx[1] , replace = FALSE)] <- NA
    RxDf$BSA[sample(1:nrow(RxDf),countMissingRx[2] , replace = FALSE)] <- NA
    RxDf$redness[sample(1:nrow(RxDf),countMissingRx[3] , replace = FALSE)] <- NA
    RxDf$DLQI[sample(1:nrow(RxDf),countMissingRx[4] , replace = FALSE)] <- NA
    
    simulatedMissingData <- rbind(placeboDf, RxDf)
    simulatedMissingData
  })
  BSAReactive <- reactive({
    df <- mergeData()
    b <- lm(formula = BSA ~ as.factor(TRT), data = df)
    c <- lm(formula = DLQI ~ as.factor(TRT) + BSA, data=df)
    BSAMed <- mediate(b, c, sims=100, treat="as.factor(TRT)", mediator="BSA")
    BSAMed
    
  })
  rednessReactive <- reactive({
    # browser()
    df <- mergeData()
    b <- lm(formula = redness ~ as.factor(TRT), data = df)
    c <- lm(formula = DLQI ~ as.factor(TRT) + redness, data=df)
    rednessMed <- mediate(b, c, sims=100, treat="as.factor(TRT)", mediator="redness")
    rednessMed
  })
  output$itchPlot <- renderPlot({
    plot(itchReactive(), effect.type = c("indirect", "direct", "total"), xlab = "Effect value", ylab = "Effect type", 
         main = "Mediation effect by itch", col="blue")
  })
  output$BSAPlot <- renderPlot({
    plot(BSAReactive(), effect.type = c("indirect", "direct", "total"), xlab = "Effect value", ylab = "Effect type", 
         main = "Mediation effect by BSA", col="blue")
  })
  output$rednessPlot <- renderPlot({
    plot(rednessReactive(), effect.type = c("indirect", "direct", "total"), xlab = "Effect value", ylab = "Effect type", 
         main = "Mediation effect by redness", col="blue")
  })
  output$itchMedPerc <- renderText({
    paste("Proportions mediated = ", round(itchReactive()$n0, 2))
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(mergeData(), file, row.names = FALSE)
    }
  )
  output$downloadmissingData <- downloadHandler(
    filename = function() {
      paste("withMissingData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(amputeData(), file, row.names = FALSE)
    }
  )
  
  output$senstivityPlot <- renderPlot({
    med.out <- BSAReactive()
    sens.out <- medsens(med.out, rho.by = 0.1, sims = 100)
    # par.orig <- par(mfrow = c(2,2))
    plot(sens.out, sens.par = "rho", main = "DLQI")
    
  })
  output$blockDiag <- renderPlot({
    data <- c(0, "'-1.437***'", 0,
              0, 0, 0, 
              "'2.66***'", "'-5.123*** (<2e-1)'", 0) 
    M<- matrix (nrow=3, ncol=3, byrow = TRUE, data=data)
    plot<- plotmat (M, pos=c(1,2), 
                    name= c( "itch","TRT", "DLQI"), 
                    box.type = "rect", box.size = 0.12, box.prop=0.5,  curve=0)
  })
}

shinyApp(ui, server)
