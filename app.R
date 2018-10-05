#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# by Esteban Higuita Garcia, ehiguita@eafit.edu.co
# student Mathematical Engeneering in EAFIT University
# MEDELLIN, COLOMBIA
# Tutors: 
# Francisco Zuluaga, EAFIT University COLOMBIA
# Andres Villegas, UNSW AUSTRALIA

packages <- c("RColorBrewer","rgl", "rainbow", "demography", "zoo", "qvcalc", "relimp", "spam", "maps", "gnm", "rootSolve", "fanplot", "fields", "StMoMo", "shiny")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

library(rgl)
library(rainbow)
library(demography)
library(zoo)
library(RColorBrewer)
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
#Missing implement PLAT

nulls_models <- c("CBD" = NULL,
                  "Lee Carter" = NULL,
                  "APC" = NULL,
                  "M7" = NULL,
                  "RH" = NULL)

mFit <- nulls_models

i_aic <- nulls_models

i_bic <- nulls_models

i_logvero <- nulls_models

i_npar <- nulls_models

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  # Application title
  h1("Mortality models", style = "color:#2A7BFF", align="center"),
  img(src="http://www.cepar.edu.au/images/cepar_logo2.png", height = "auto", width = 200),
  img(src="http://www.eafit.edu.co/SiteCollectionImages/logo_eafit_55.png", height = "auto", width = 200),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("data", "Choose Input Data",
                   c("Male England and Wales data" = "default",
                     "Input mortality.org data" = "mortality")
      ),
      
      conditionalPanel(
        condition = "input.data == 'mortality'",
        a(href="http://www.mortality.org/","Mortality"),
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
      
      checkboxGroupInput("models", "Input checkbox",
                         c("CBD" = "CBD",
                           "Lee Carter" = "LC",
                           "APC" = "APC",
                           "M7" = "M7",
                           "RH" = "RH")),
      
      
      # Specification of range within an interval
      sliderInput("range_age", "Range age:",
                  min = range(ages)[1], max = range(ages)[2], value = c(mean(ages),range(ages)[2])),
      sliderInput("range_year", "Range year:",
                  min = range(years)[1], max = range(years)[2] , value = c(mean(years),range(ages)[2])),
      checkboxInput("a_c", "Advaced configuration", FALSE),
      conditionalPanel(
        condition = "input.a_c",
        sliderInput("h_input","Year ahead central projections of the period indexes",
                    min = 0, max = 200, value = 50),
        h5("Specification ARIMA model"),
        textInput("p_input", "p",placeholder = 1),
        textInput("d_input","d", placeholder = 1),
        textInput("q_input","q",placeholder = 0),
        textInput("n_sim_input","Number of simulations",
                  value = 500),
        textInput("seed_input","Set seed", value = 1234)
      )
      #,submitButton("Submit")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Ranking", dataTableOutput("tabla")),
                  tabPanel(" Parameters",
                           fluidRow(
                             uiOutput("plotsParameters")
                             
                           )),
                  tabPanel("Residuals",
                           fluidRow(
                             uiOutput("plotsResiduals")
                           )),
                  tabPanel("Forecast",
                           fluidRow(
                             uiOutput("plotsForecast")
                           )),
                  tabPanel("Simulate",
                           fluidRow(
                             uiOutput("plotsSimulate")
                           ))
      )
    )
    
  ),
  a(href="www.linkedin.com/in/estebanhiguita", "by Esteban Higuita G.",align="right"),
  br(),
  a(href="#", "Tutors: Francisco Zuluaga, EAFIT University COLOMBIA and Andres Villegas, UNSW AUSTRALIA", align="right")
)
)


server <- shinyServer(function(input, output) {
  
  #Default
  output$tabla = renderDataTable({
    
    
    i_models <- input$models
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    for(i_model in i_models){
      model <- switch(i_model,
                      LC = LC,
                      CBD = CBD,
                      M7 = M7,
                      APC = APC,
                      RH = RH,
                      LC)
      fitModel <- fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                      ages.fit = ages.fit)
      mFit[i_model] <- fitModel
      i_aic[i_model] <- AIC(fitModel)
      i_bic[i_model] <- BIC(fitModel)
      i_npar[i_model] <- fitModel$npar
      i_logvero[i_model] <- fitModel$loglik
    }
    tabla <- data.frame(Models=i_models,Log_Likelihood=i_logvero,Effective_number_of_parameters=i_npar, BIC = i_bic, AIC = i_aic)
  })
  
  ### Render Parameters ###
  plotInput <- reactive({
    i_models <- input$models
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    total_data <- lapply(i_models, function(i){
      model <- switch(i,
                      LC = LC,
                      CBD = CBD,
                      M7 = M7,
                      APC = APC,
                      RH = RH,
                      LC)
      fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
          ages.fit = ages.fit)
    })
    total_data_r <- lapply(i_models, function(i){
      model <- switch(i,
                      LC = LC,
                      CBD = CBD,
                      M7 = M7,
                      APC = APC,
                      RH = RH,
                      LC)
      residuals(fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                    ages.fit = ages.fit))
    })
    total_data_f <- lapply(i_models, function(i){
      model <- switch(i,
                      LC = LC,
                      CBD = CBD,
                      M7 = M7,
                      APC = APC,
                      RH = RH,
                      LC)
      forecast(fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                   ages.fit = ages.fit))
    })
    total_data_s <- lapply(i_models, function(i){
      set.seed(input$seed_input)
      model <- switch(i,
                      LC = LC,
                      CBD = CBD,
                      M7 = M7,
                      APC = APC,
                      RH = RH,
                      LC)
      simulate(fit(model, Dxt = Dxt, Ext = Ext, ages = ages, years = years,
                   ages.fit = ages.fit), nsim = input$n_sim_input, h =input$h_input)
    })
    n_plot <- length(i_models)
    return (list("n_plot"=n_plot, "total_data"=total_data, "total_data_r"=total_data_r,"total_data_f"= total_data_f,"total_data_s"= total_data_s,  "i_models"=i_models, "years"=years, "age", ages))
  })
  ##### Create divs Parameters######
  output$plotsParameters <- renderUI({
    titles <- lapply(1:plotInput()$n_plot, function(i) {
      h1(paste("Plot Parameters",plotInput()$i_models[i] , sep=" "))
    }
    )
    output_list <- lapply(1:plotInput()$n_plot, function(i) {
      plotname <- paste("plot",plotInput()$i_models[i] , sep="")
      plotOutput(plotname, height = 480, width = 700, hover = plotname)
    })
    
    list_sort = vector()
    i <- 1
    j <- 1
    while (i<length(titles)+1) {
      list_sort[j] = titles[i]
      list_sort[j+1] = output_list[i]
      i <- i+1
      j <- j+2
    }
    plot_output_list <- list_sort 
    
    do.call(tagList, plot_output_list)
  })
  
  ## --- ##
  
  ##### Create divs Residuals######
  output$plotsResiduals <- renderUI({
    titles_r <- lapply(1:plotInput()$n_plot, function(i) {
      h1(paste("Plot Residuals",plotInput()$i_models[i] , sep=" "))
    }
    )
    output_list_r <- lapply(1:plotInput()$n_plot, function(i) {
      plotname <- paste("plotResidual",plotInput()$i_models[i] , sep="")
      plotOutput(plotname, height = 480, width = 700)
    })
    
    list_sort_r = vector()
    i <- 1
    j <- 1
    while (i<length(titles_r)+1) {
      list_sort_r[j] = titles_r[i]
      list_sort_r[j+1] = output_list_r[i]
      i <- i+1
      j <- j+2
    }
    plot_output_list_r <- list_sort_r 
    
    do.call(tagList, plot_output_list_r)
  })
  ## --- ##
  
  ##### Create divs Forecast######
  output$plotsForecast <- renderUI({
    titles <- lapply(1:plotInput()$n_plot, function(i) {
      h1(paste("Plot Forecast",plotInput()$i_models[i] , sep=" "))
    }
    )
    output_list <- lapply(1:plotInput()$n_plot, function(i) {
      plotname <- paste("plotForecast",plotInput()$i_models[i] , sep="")
      plotOutput(plotname, height = 480, width = 700)
    })
    
    list_sort = vector()
    i <- 1
    j <- 1
    while (i<length(titles)+1) {
      list_sort[j] = titles[i]
      list_sort[j+1] = output_list[i]
      i <- i+1
      j <- j+2
    }
    plot_output_list_f <- list_sort
    
    do.call(tagList, plot_output_list_f)
  })
  ##### Create divs Simulate######
  output$plotsSimulate <- renderUI({
    titles <- lapply(1:plotInput()$n_plot, function(i) {
      h1(paste("Plot Simulate",plotInput()$i_models[i] , sep=" "))
    }
    )
    output_list <- lapply(1:plotInput()$n_plot, function(i) {
      plotname <- paste("plotSimulate",plotInput()$i_models[i] , sep="")
      plotOutput(plotname, height = 480, width = 700)
    })
    
    list_sort = vector()
    i <- 1
    j <- 1
    while (i<length(titles)+1) {
      list_sort[j] = titles[i]
      list_sort[j+1] = output_list[i]
      i <- i+1
      j <- j+2
    }
    plot_output_list_s <- list_sort
    
    do.call(tagList, plot_output_list_s)
  })
  
  observe({
    if(input$data == "mortality"){
      #Mortality
      print("entro al if")
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
      
      ages <- mortData$age     
      years <- mortData$year   
      
      nulls_models <- c("CBD" = NULL,
                        "Lee Carter" = NULL,
                        "APC" = NULL,
                        "M7" = NULL,
                        "RH" = NULL)
      
      mFit <- nulls_models
      
      i_aic <- nulls_models
      
      i_bic <- nulls_models
      
      i_logvero <- nulls_models
      
      i_npar <- nulls_models
    }
    if(input$data == "default"){
      Dxt <- EWMaleData$Dxt
      Ext <- EWMaleData$Ext
      ages <- EWMaleData$ages 
      years <- EWMaleData$years 
      
      #Define Models
      LC <- lc()
      CBD <- cbd(link = "log")
      APC <- apc()
      M7 <- m7(link = "log")
      RH <- rh(link = "logit",  cohortAgeFun = "1")
      #Missing implement PLAT
      
      nulls_models <- c("CBD" = NULL,
                        "Lee Carter" = NULL,
                        "APC" = NULL,
                        "M7" = NULL,
                        "RH" = NULL)
      
      mFit <- nulls_models
      
      i_aic <- nulls_models
      
      i_bic <- nulls_models
      
      i_logvero <- nulls_models
      
      i_npar <- nulls_models
    }
    
    
    ages.fit <- input$range_age[1]:input$range_age[2]
    years.fit <- input$range_year[1]:input$range_year[2]
    
    lapply(1:plotInput()$n_plot, function(i){
      output[[paste("plotResidual", plotInput()$i_models[i], sep="") ]] <- renderPlot({
        plot(plotInput()$total_data_r[[i]])
        plot(plotInput()$total_data_r[[i]], type = "colourmap", reslim = c(-4,4))
      })
    })
    lapply(1:plotInput()$n_plot, function(i){
      output[[paste("plot", plotInput()$i_models[i], sep="") ]] <- renderPlot({
        plot(plotInput()$total_data[[i]])
      })
    })
    lapply(1:plotInput()$n_plot, function(i){
      output[[paste("plotForecast", plotInput()$i_models[i], sep="") ]] <- renderPlot({
        plot(plotInput()$total_data_f[[i]], only.kt = TRUE)
      })
    })
    lapply(1:plotInput()$n_plot, function(i){
      output[[paste("plotSimulate", plotInput()$i_models[i], sep="") ]] <- renderPlot({
        plot(plotInput()$total_data[[i]]$years, plotInput()$total_data[[i]]$kt[1,],
             xlim=c(range(plotInput()$years)[1],range(plotInput()$years)[2]+input$h_input), ylim=range(plotInput()$total_data_s[[i]]$kt.s$sim[1,,1:20],plotInput()$total_data[[i]]$kt[1,]),
             type="l", xlab="year", ylab="kt",
             main="Period index")
        matlines(plotInput()$total_data_s[[i]]$kt.s$years, plotInput()$total_data_s[[i]]$kt.s$sim[1,,1:20],
                 type="l", lty=1)
        #plot(plotInput()$total_data_s[[i]]$kt.s$years,plotInput()$total_data_s[[i]]$kt.s$sim[1,,1:20])
      })
    })
  })
})

# Run the application 
shinyApp(ui = ui, server = server)
