#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#username_input = "ehiguita@eafit.edu.co"
#password_input = "1468543048"

library(rgl)
library(rainbow)
library(demography)
library(zoo)
library(qvcalc)
library(relimp)
library(spam)
library(maps)
library(gnm)
library(rootSolve)
library(fanplot)
library(fields)
library(StMoMo)
library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Mortality models"), 
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput("username_input","User", value = "ehiguita@eafit.edu.co",placeholder = "ehiguita@eafit.edu.co"),
        textInput("password_input","Password", value = "1468543048",placeholder = "1468543048"),
        textInput("country_input","Country", value = "AUS",placeholder = "Australia"),
        
        # Specification of range within an interval
        sliderInput("range_age", "Range age:",
                    min = 0, max = 90, value = c(0,90)),
        sliderInput("range_year", "Range year:",
                    min = 1921, max = 2011 , value = c(1970,2011))
      ),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Ranking", dataTableOutput("tabla")),
                    tabPanel("Death Rates", plotOutput("improvement_rates")), 
                    tabPanel("LC - Parameters", plotOutput("lc")), 
                    tabPanel("LC - Residuals", plotOutput("lc_residuals")), 
                    tabPanel("CBD - Parameters", plotOutput("cbd")), 
                    tabPanel("CBD - Residuals", plotOutput("cbd_residuals"))
                    )
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
    output$tabla = renderDataTable({
      AUSdata <- hmd.mx(country = input$country_input, username = input$username_input, password = input$password_input)
      # Extract men data
      #load("AUSdata.RData")
      Ext <- AUSdata$pop$male 
      Dxt <- round(AUSdata$rate$male * Ext)
      ages <- AUSdata$age     #0-110
      years <- AUSdata$year   #1921-2011
      # Define Lee-Carter model
      LC <- lc()
      
      ages.fit <- input$range_age[1]:input$range_age[2]
      #years <- input$range_year[1]:input$range_year[2]
      
      #We now fit the model to data for ages 0-90
      
      LCfit <- fit(LC, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                   ages.fit = ages.fit)
      
      #Define the CBD
      CBD <- cbd(link = "log")
      
      ages.fit <- input$range_age[1]:input$range_age[2]
      years.fit <- input$range_year[1]:input$range_year[2]
      
      CBDfit <- fit(CBD, Dxt = Dxt, Ext = Ext, ages = ages, years = years, ages.fit = ages.fit,
                    years.fit = years.fit)
      
      logverolc<-LCfit$loglik
      nparlc<-LCfit$npar
      aiclc<-AIC(LCfit)
      biclc<-BIC(LCfit)
      logverocbd<-CBDfit$loglik
      nparcbd<-CBDfit$npar
      aiccbd<-AIC(CBDfit)
      biccbd<-BIC(CBDfit)
      
      
      modelos <- c("Lee Carter","CBD")
      Maximium_Log_Likelihood <- c(logverolc, logverocbd)
      Effective_Number_of_Parameters <- c(nparlc,nparcbd)
      AIC_Rank <- c(aiclc,aiccbd)
      BIC_Rank <- c(biclc, biccbd)
      tabla <- data.frame(modelos, Maximium_Log_Likelihood,Effective_Number_of_Parameters,BIC_Rank, AIC_Rank)
    })
  
   output$improvement_rates <- renderPlot({
     AUSdata <- hmd.mx(country = input$country_input, username = input$username_input, password = input$password_input)
     # Extract men data
     #load("AUSdata.RData")
     Ext <- AUSdata$pop$male 
     Dxt <- round(AUSdata$rate$male * Ext)
     ages <- AUSdata$age     #0-110
     years <- AUSdata$year   #1921-2011
     
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      ages <- input$range_age[1]:input$range_age[2]
      years<- input$range_year[1]:input$range_year[2]
      #ages <- 0:max_age
      Dxt <- Dxt[as.character(ages), ]
      Ext <- Ext[as.character(ages), ]
      
      #Compute mxt
      mxt <- Dxt / Ext
      
      #Reduction factors
      rx_1 <- (mxt[, as.character(input$range_year[2])]/mxt[, as.character(input$range_year[1])])^(1/(input$range_year[2]-input$range_year[1]))
      #rx_1 <- (mxt[, "2011"]/mxt[, "1921"])^(1/(2011-1921))
      #rx_2 <- (mxt[, "2011"]/mxt[, "1971"])^(1/(2011-1971))
      #Compyute Improvement rates and smooth
      ix_1 <- 1 - rx_1
      #ix_2 <- 1 - rx_2
      ix_smooth_1 <- fitted(smooth.spline(ages, ix_1))
      #ix_smooth_2 <- fitted(smooth.spline(ages, ix_2))
      
      plot(ages, ix_1, type = "l", main = "Improvement rates AUS")
      lines(ages, ix_smooth_1, col = "blue", lwd = 2)
      #lines(ages, ix_2)
      #lines(ages, ix_smooth_2, col = "red", lwd = 2)
      
   })
   output$lc <- renderPlot({
     AUSdata <- hmd.mx(country = input$country_input, username = input$username_input, password = input$password_input)
     # Extract men data
     #load("AUSdata.RData")
     Ext <- AUSdata$pop$male 
     Dxt <- round(AUSdata$rate$male * Ext)
     ages <- AUSdata$age     #0-110
     years <- AUSdata$year   #1921-2011
     # Define Lee-Carter model
     LC <- lc()
     
     ages.fit <- input$range_age[1]:input$range_age[2]
     #years <- input$range_year[1]:input$range_year[2]
     
     #We now fit the model to data for ages 0-90
     
     LCfit <- fit(LC, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                  ages.fit = ages.fit)
     #d. Plot the estimated parameter of the Lee-Carter model and discuss their interpretation.
     plot(LCfit)
   })
   
   output$lc_residuals <- renderPlot({
     AUSdata <- hmd.mx(country = input$country_input, username = input$username_input, password = input$password_input)
     # Extract men data
     #load("AUSdata.RData")
     Ext <- AUSdata$pop$male 
     Dxt <- round(AUSdata$rate$male * Ext)
     ages <- AUSdata$age     #0-110
     years <- AUSdata$year   #1921-2011
     # Define Lee-Carter model
     LC <- lc()
     ages.fit <- input$range_age[1]:input$range_age[2]
     years.fit <- input$range_year[1]:input$range_year[2]
     
     #We now fit the model to data for ages 0-90
     
     LCfit <- fit(LC, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                  ages.fit = ages.fit)
     LCres <- residuals(LCfit)
     
     plot(LCres)
     plot(LCres, type = "colourmap", reslim = c(-4,4))
   })
   
   output$cbd <- renderPlot({
     AUSdata <- hmd.mx(country = input$country_input, username = input$username_input, password = input$password_input)
     # Extract men data
     #load("AUSdata.RData")
     Ext <- AUSdata$pop$male 
     Dxt <- round(AUSdata$rate$male * Ext)
     ages <- AUSdata$age     #0-110
     years <- AUSdata$year   #1921-2011
     
     #Define the CBD
     CBD <- cbd(link = "log")
     
     ages.fit <- input$range_age[1]:input$range_age[2]
     years.fit <- input$range_year[1]:input$range_year[2]
    
     CBDfit <- fit(CBD, Dxt = Dxt, Ext = Ext, ages = ages, years = years, ages.fit = ages.fit,
                   years.fit = years.fit)
     plot(CBDfit, parametricbx = FALSE)
    
   })
   
   output$cbd_residuals <- renderPlot({
     
     AUSdata <- hmd.mx(country = input$country_input, username = input$username_input, password = input$password_input)
     # Extract men data
     #load("AUSdata.RData")
     Ext <- AUSdata$pop$male 
     Dxt <- round(AUSdata$rate$male * Ext)
     ages <- AUSdata$age     #0-110
     years <- AUSdata$year   #1921-2011
     #Define the CBD and M7 models
     CBD <- cbd(link = "log")
     
     ages.fit <- input$range_age[1]:input$range_age[2]
     years.fit <- input$range_year[1]:input$range_year[2]
     
     CBDfit <- fit(CBD, Dxt = Dxt, Ext = Ext, ages = ages, years = years, ages.fit = ages.fit,
                   years.fit = years.fit)
     CBDres <- residuals(CBDfit)
     plot(CBDres, type = "colourmap", reslim = c(-3.5,3.5))
     
   })
   
   
})

# Run the application 
shinyApp(ui = ui, server = server)
