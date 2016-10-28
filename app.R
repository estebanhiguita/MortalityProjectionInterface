#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

#General settings
Dxt <- EWMaleData$Dxt
Ext <- EWMaleData$Ext
ages <- EWMaleData$ages #0-100
years <- EWMaleData$years #1961-2011

#Define Models
LC <- lc()
CBD <- cbd(link = "log")
APC <- apc()
M7 <- m7(link = "log")
RH <- rh(link = "logit",  cohortAgeFun = "1")
#Falta PLAT

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("Mortality models"), 
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("data", "Choose Input Data",
                   c("Male Wales data" = "default",
                     "Input mortality.org data" = "mortality")
      ),
      
      conditionalPanel(
        condition = "input.data == 'mortality'",
        textInput("username_input","User", value = "ehiguita@eafit.edu.co",placeholder = "ehiguita@eafit.edu.co"),
        textInput("password_input","Password", value = "1468543048",placeholder = "1468543048"),
        textInput("country_input","Country", value = "AUS",placeholder = "Australia"),
        radioButtons("gender","Choose gender",
                     c("Male" = "male",
                       "Famele" = "female",
                       "Total" = "total"
                     )
        )
      ),
      
      radioButtons("m1", "Choose Model 1:",
                   c("Lee Carter" = "LC",
                     "CBD" = "CBD",
                     "APC" = "APC",
                     "M7" = "M7",
                     "RH" = "RH")),
      
      radioButtons("m2", "Choose Model 2:",
                   c("CBD" = "CBD",
                     "Lee Carter" = "LC",
                     "APC" = "APC",
                     "M7" = "M7",
                     "RH" = "RH")),
      
      
      # Specification of range within an interval
      sliderInput("range_age", "Range age:",
                  min = range(ages)[1], max = range(ages)[2], value = c(mean(ages),range(ages)[2])),
      sliderInput("range_year", "Range year:",
                  min = range(years)[1], max = range(years)[2] , value = c(mean(years),range(ages)[2]))
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(
        condition = "input.data == 'mortality'",
        tabsetPanel(type = "tabs", 
                    tabPanel("Ranking", dataTableOutput("mtabla")),
                    tabPanel(" Parameters",
                             fluidRow(
                               column(8, h3("Model 1"), plotOutput("mplotparametersm1")),
                               column(12, h3("Model 2"), plotOutput("mplotparametersm2"))
                             )),
                    tabPanel("Residuals",
                             fluidRow(
                               column(8,  h3("Model 1"),plotOutput("mplotresidualsm1")),
                               column(12, h3("Model 2"),plotOutput("mplotresidualsm2"))
                             ))
                    
        )
      ),
      conditionalPanel(
        condition = "input.data == 'default'",
        tabsetPanel(type = "tabs", 
                    tabPanel("Ranking", dataTableOutput("tabla")),
                    tabPanel(" Parameters",
                             fluidRow(
                               column(8, h3("Model 1"), plotOutput("plotparametersm1")),
                               column(12, h3("Model 2"), plotOutput("plotparametersm2"))
                             )),
                    tabPanel("Residuals",
                             fluidRow(
                               column(8,  h3("Model 1"),plotOutput("plotresidualsm1")),
                               column(12, h3("Model 2"),plotOutput("plotresidualsm2"))
                             ))
                    
        )
      )
      
    )
  )
)
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  #Default
  output$tabla = renderDataTable({
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    LCfit <- fit(LC, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                 ages.fit = ages.fit, years.fit = years.fit)
    
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
  output$plotparametersm1 <- renderPlot({
    model <- switch(input$m1,
                    LC = LC,
                    CBD = CBD,
                    M7 = M7,
                    APC = APC,
                    RH = Rh,
                    LC)
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    mFit <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                ages.fit = ages.fit,years.fit = years.fit)
    #d. Plot the estimated parameter of the Lee-Carter model and discuss their interpretation.
    plot(mFit)
  })
  output$plotparametersm2 <- renderPlot({
    model <- switch(input$m2,
                    LC = LC,
                    CBD = CBD,
                    M7 = M7,
                    APC = APC,
                    RH = Rh,
                    LC)
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    mFit <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                ages.fit = ages.fit,years.fit = years.fit)
    #d. Plot the estimated parameter of the Lee-Carter model and discuss their interpretation.
    plot(mFit)
  })
  output$plotresidualsm1 <- renderPlot({
    model <- switch(input$m1,
                    LC = LC,
                    CBD = CBD,
                    M7 = M7,
                    APC = APC,
                    RH = Rh,
                    CBD)
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    mFit <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                ages.fit = ages.fit)
    mRes <- residuals(mFit)
    
    plot(mRes)
    plot(mRes, type = "colourmap", reslim = c(-4,4))
  })
  output$plotresidualsm2 <- renderPlot({
    model <- switch(input$m2,
                    LC = LC,
                    CBD = CBD,
                    M7 = M7,
                    APC = APC,
                    RH = Rh,
                    CBD)
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    mFit <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                ages.fit = ages.fit)
    mRes <- residuals(mFit)
    
    plot(mRes)
    plot(mRes, type = "colourmap", reslim = c(-4,4))
  })
  
  #Mortality
  output$mtabla = renderDataTable({
    
    mortData <- hmd.mx(country = input$country_input, username = input$username_input, password = input$password_input)
    # Extract men data
    #load("AUSdata.RData")
    Ext <- switch(input$gender,
                  male = mortData$pop$male,
                  female = mortData$pop$female,
                  total = mortData$pop$total,
                  mortData$pop$male)
    Dxt <- switch(input$gender,
                  male = round(mortData$rate$male * Ext),
                  female = round(mortData$rate$female * Ext),
                  total = round(mortData$rate$total * Ext),
                  round(mortData$rate$male * Ext))
    
    ages <- mortData$age     #0-110
    years <- mortData$year   #1921-2011
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    LCfit <- fit(LC, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                 ages.fit = ages.fit, years.fit = years.fit)
    
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
  output$mplotparametersm1 <- renderPlot({
    mortData <- hmd.mx(country = input$country_input, username = input$username_input, password = input$password_input)
    # Extract men data
    #load("AUSdata.RData")
    Ext <- mortData$pop$male 
    Dxt <- round(mortData$rate$male * Ext)
    ages <- mortData$age     #0-110
    years <- mortData$year   #1921-2011
    
    model <- switch(input$m1,
                    LC = LC,
                    CBD = CBD,
                    M7 = M7,
                    APC = APC,
                    RH = Rh,
                    LC)
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    mFit <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                ages.fit = ages.fit,years.fit = years.fit)
    #d. Plot the estimated parameter of the Lee-Carter model and discuss their interpretation.
    plot(mFit)
  })
  output$mplotparametersm2 <- renderPlot({
    model <- switch(input$m2,
                    LC = LC,
                    CBD = CBD,
                    M7 = M7,
                    APC = APC,
                    RH = Rh,
                    LC)
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    mFit <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                ages.fit = ages.fit,years.fit = years.fit)
    #d. Plot the estimated parameter of the Lee-Carter model and discuss their interpretation.
    plot(mFit)
  })
  output$mplotresidualsm1 <- renderPlot({
    model <- switch(input$m1,
                    LC = LC,
                    CBD = CBD,
                    M7 = M7,
                    APC = APC,
                    RH = Rh,
                    CBD)
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    mFit <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                ages.fit = ages.fit)
    mRes <- residuals(mFit)
    
    plot(mRes)
    plot(mRes, type = "colourmap", reslim = c(-4,4))
  })
  output$mplotresidualsm2 <- renderPlot({
    model <- switch(input$m2,
                    LC = LC,
                    CBD = CBD,
                    M7 = M7,
                    APC = APC,
                    RH = Rh,
                    CBD)
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    #We now fit the model to data for ages 0-90
    
    mFit <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                ages.fit = ages.fit)
    mRes <- residuals(mFit)
    
    plot(mRes)
    plot(mRes, type = "colourmap", reslim = c(-4,4))
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

