#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readxl)
library(shiny)
library(magrittr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(scales)
library(markdown)
library(imputeTS)
library(zoo)
library(forecast)
library(corrplot)
library(data.table)
library(plotly)
#library(highcharter)
#setwd("C:/Users/stevan.caric/Desktop/R Projects/GfK/hhp_app_a")
Y <-2011; M<-1 # Panel starting year and month
hhp<-read.csv("hhp_anonym.csv")
hhp<-as_tibble(hhp)
hhp$date<-as.Date(as.yearmon(hhp$date))

hhp1 <- select(hhp, union(starts_with("date"),starts_with("volume_")))
names(hhp1) <-sub("volume_","", names(hhp1))
hhp1 <- hhp1 %>% gather(product,volume, product_1:product_8)
hhp2 <- select(hhp, union(starts_with("date"),starts_with("price_")))
names(hhp2) <-sub("price_","", names(hhp2))
hhp2 <- hhp2 %>% gather(product,price, product_1:product_8)
hhpx <- inner_join(hhp1, hhp2, by=c("date","product"))
rm(hhp1); rm(hhp2)


hhp_dl<-diff(log(ts(hhp[2:ncol(hhp)], start=c(Y,M), freq=12))) 
date <-hhp$date[2:nrow(hhp)]
hhp_dl<-cbind(date,as.data.frame(hhp_dl))
hhp1 <- select(hhp_dl, union(starts_with("date"),starts_with("volume_")))
names(hhp1) <-sub("volume_","", names(hhp1))
hhp1 <- hhp1 %>% gather(product,volume, product_1:product_8)
hhp2 <- select(hhp_dl, union(starts_with("date"),starts_with("price_")))
names(hhp2) <-sub("price_","", names(hhp2))
hhp2 <- hhp2 %>% gather(product,price, product_1:product_8)
hhp_dlx <- inner_join(hhp1, hhp2, by=c("date","product"))

econ<-read_excel("Economic_indicators.xlsx")
econ$tour_ar_all<-econ$tour_ar_for<-econ$tour_ns_all<-econ$tour_ns_for<-NULL
econ$unemp_rate<-na.interpolation(econ$unemp_rate, option = "linear")
econ$emp_rate<-na.interpolation(econ$emp_rate, option = "linear")
# econ$ek01<-na.interpolation(econ$ek01, option = "linear")
# econ$ek02<-na.interpolation(econ$ek02, option = "linear")
# econ$ek04<-na.interpolation(econ$ek04, option = "linear")
# econ$ek01<-(econ$ek01+100)/2
# econ$ek02<-(econ$ek02+100)/2
# econ$ek04<-(econ$ek04+100)/2
# econ$cons_climate<-(econ$ek01+econ$ek02+econ$ek04)/3
i<-nrow(econ)-nrow(hhp)
econ <- econ[1:(nrow(econ)-i),]
econ$date<-as.Date(as.yearmon(econ$date))
econ_cor <- cor(econ[,2:ncol(econ)])
econ_dl<-diff(log(ts(econ[2:ncol(econ)], start=c(Y,M), freq=12))) 
date <-econ$date[2:nrow(econ)]
econ_dl<-cbind(date,as.data.frame(econ_dl))
econ_dl$avg_temp <- diff(ts(econ$avg_temp, start=c(Y,M), freq=12))

hhp_econ <- left_join(hhp, econ, by="date")
hhpx_econ <- left_join(hhpx, econ, by="date")
hhpx_econ_dl <- left_join(hhp_dlx, econ_dl, by="date")
hhpx_econ_dl<-tbl_df(hhpx_econ_dl)
hhp_econ_round<-hhp_econ
hhp_econ_round<-round(hhp_econ_round[,-1],2)

N<-as.numeric(nrow(hhp))
#n <- N-12 # poslednji podatak za trening set (2016,4)

# Logged = FALSE;
# my_username <- "sc"
# my_password <- "sc"

# ui1 <- function(){
#   tagList(
#     div(id = "login",
#         wellPanel(textInput("userName", "Username:"),
#                   passwordInput("passwd", "Password:"),
#                   br(),actionButton("Login", "Log in"))),
#     tags$style(type="text/css", "#login {font-size:12px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}")
#   )}

ui <- function() {tagList(navbarPage("Household Panel",
                 tabPanel("Plots",
                          # selectInput("serija","Choose the product:", 
                          #             choices=levels(factor(hhpx$product)),
                          #             selected = "product_1"),
                          #sidebarLayout(
                          sidebarPanel(
                            img(src="datasci.png", height = 50, width = 150),
                            hr(),
                            selectInput("serija","Choose the product:", 
                                        choices=levels(factor(hhpx$product)),
                                        selected = "product_1"),
                            radioButtons("nivo", "Choose the series level:",
                                         c("Original series"=1, "Growth rate"=2)
                            ),
                            radioButtons("tip", "Choose the plot type:",
                                         c("Regular"=1, "Seasonal"=2)),
                            width=3
                          ),
                          mainPanel(
                            plotlyOutput("plot")
                          )
                          #)
                 ),
                 tabPanel("Decomposition",
                          sidebarPanel(
                            selectInput("serija1","Choose the product:", 
                                        choices=levels(factor(hhpx$product)),
                                        selected = "product_1"), width = 3
                          ),
                          mainPanel(
                            plotOutput("decomp")
                          )
                 ),
                 tabPanel("Correlation matrix",
                          sidebarPanel(
                            selectInput("serija2","Choose the product:", 
                                        choices=levels(factor(hhpx$product)),
                                        selected = "product_1"),
                            radioButtons("nivo2", "Choose the series level:",
                                         c("Original series"=1, "Growth rate"=2)),
                            width=3
                          ),
                          mainPanel(
                            plotOutput("cor")
                          )
                 ),
                 tabPanel("OLS model",
                          sidebarPanel(
                            selectInput("serija3","Choose the product:", 
                                        choices=levels(factor(hhpx$product)),
                                        selected = "product_1"),
                            radioButtons("nivo3", "Choose the series level:",
                                         c("Original series"=1, "Growth rate"=2)
                            ),
                            checkboxGroupInput('varijable', 'Choose the variables:',
                                               names(hhpx_econ)[4:ncol(hhpx_econ)], 
                                               selected = "price"),
                            width = 3
                          ),
                          mainPanel(
                            verbatimTextOutput("model")
                          )
                 ),
                 tabPanel("Time series models",
                          sidebarPanel(
                            selectInput("serija4", "Choose the product:", 
                                        choices=levels(factor(hhpx$product)),
                                        selected = "product_1"),
                            sliderInput("n_pred", "Choose the prediction horizon:",
                                        min=1, max=12, value=12, step=1),
                            radioButtons("modeli", "Choose the time series model:",
                                         c("ARIMA"=1, "ETS"=2, "STL"=3, "TBATS"=4)
                            ),
                            width = 3
                          ),
                          mainPanel(
                            plotOutput("forecast"),
                            br(),
                            verbatimTextOutput("accuracy"),
                            br(),
                            verbatimTextOutput("summary")
                          )
                 ),
                 tabPanel("Data",
                          DT::dataTableOutput("table")
                 )
))}

#ui = (htmlOutput("page"))

server = (function(input, output,session) {
  
  # USER <- reactiveValues(Logged = Logged)
  # 
  # 
  # observe({ 
  #   if (USER$Logged == FALSE) {
  #     if (!is.null(input$Login)) {
  #       if (input$Login > 0) {
  #         Username <- isolate(input$userName)
  #         Password <- isolate(input$passwd)
  #         Id.username <- which(my_username == Username)
  #         Id.password <- which(my_password == Password)
  #         if (length(Id.username) > 0 & length(Id.password) > 0) {
  #           if (Id.username == Id.password) {
  #             USER$Logged <- TRUE
  #           } 
  #         }
  #       } 
  #     }
  #   }    
  # })
  # observe({
  #   if (USER$Logged == FALSE) {
  #     
  #     output$page <- renderUI({
  #       div(class="outer",do.call(bootstrapPage,c("",ui1())))
  #     })
  #   }
  #   if (USER$Logged == TRUE) 
  #   {
  #     output$page <- renderUI({
  #       #div(class="outer",do.call(navbarPage,c(inverse=TRUE,ui2())))
  #       div(class="outer",do.call(bootstrapPage,c("",ui2())))
  #     })
  #     print(ui)
  #   }
  # })
  
  output$plot <- renderPlotly({
    if (input$nivo==1) {
      df <- hhpx %>% filter(product==input$serija)
      if (input$tip==1) {
        ggplot(df, aes(date,volume))+geom_line(col="dodgerblue3")+
          theme_light()+labs(x = " ", y = " ")+
          scale_x_date(date_breaks= "1 year", date_labels= "%Y") +
          scale_y_continuous(labels = scales::comma)
        #hchart(df,"line", hcaes(x=date, y=volume))
      } else {
        ggseasonplot(ts(df$volume, start=c(Y,M), freq=12), ylab="", xlab="", main="",
                     year.labels=TRUE, year.labels.left=TRUE, col=rainbow(7)) +
          theme_light()+labs(x = " ", y = " ")+
          scale_y_continuous(labels = scales::comma)
      }
    } else {
      df <- hhp_dlx %>% filter(product==input$serija)
      if (input$tip==1) {
        ggplot(df, aes(date,volume))+geom_line(col="dodgerblue3")+
          theme_light()+labs(x = " ", y = " ")+
          scale_x_date(date_breaks= "1 year", date_labels= "%Y") +
          scale_y_continuous(labels = scales::comma)
      } else {
        ggseasonplot(ts(df$volume, start=c(Y,M+1), freq=12), ylab="", xlab="", main="",
                     year.labels=TRUE, year.labels.left=TRUE, col=rainbow(7)) +
          theme_light()+labs(x = " ", y = " ")+
          scale_y_continuous(labels = scales::comma)
      }
    }
  })
  
  output$decomp <- renderPlot({
    df <- hhpx %>% filter(product==input$serija1)
    autoplot(stl(ts(df$volume, start=c(Y,M), freq=12),s.window = "periodic")) +
      labs(x="") + theme_light()
  })
  
  output$cor <- renderPlot({
    if (input$nivo2==1) {
      df <- hhpx %>% filter(product==input$serija2)
      econ_cor <- cor(cbind(volume=df$volume, price=df$price, econ[,2:ncol(econ)]))
      corrplot.mixed(econ_cor, lower="number", upper="shade", tl.pos = "lt")
    } else {
      df <- hhp_dlx %>% filter(product==input$serija2)
      econ_dl_cor <- cor(cbind(df$volume, df$price, econ_dl[,2:ncol(econ_dl)]))
      corrplot.mixed(econ_dl_cor, lower="number", upper="shade", tl.pos = "lt")
    }
  }, width = 700, height = 700)
  
  output$model <- renderPrint({
    options(scipen=5, digits=4)
    if (input$nivo3==1) {
      df <- hhpx_econ %>% filter(product==input$serija3)
      if (!is.null(input$varijable)) {
        df <- cbind(df[,"volume"],df[,input$varijable])
        summary(lm(volume ~ . , df))
      }
      
    } else {
      df <- hhpx_econ_dl %>% filter(product==input$serija3)
      if (!is.null(input$varijable)) {
        df <- cbind(df[,"volume"],df[,input$varijable])
        summary(lm(volume ~ . , df))
      }
    }
    
  })
  
  combo_output <- reactive({
    df <- hhpx_econ %>% filter(product==input$serija4)
    n <- N-input$n_pred
    m <- (n+1)%%12
    y <- Y + (n+1)%/%12
    train<- ts(df$volume[1:n], start=c(Y,M), freq=12)
    test<- ts(df$volume[(n+1):N], start=c(y,m), freq=12)
    if (input$modeli==1) {
      m <- auto.arima(train) 
    } else if (input$modeli==2) {
      m <- ets(train) 
    } else if (input$modeli==3) {
      m <- stl(train, s.window = "periodic") 
    } else if (input$modeli==4) {
      m <- tbats(train)
    } else {}
    m_f <- forecast(m, h =N-n)
    dfc<- data.frame(date=df$date, obs=df$volume, fcst=c(rep(NA,n), m_f$mean),
                     hi80=c(rep(NA,n), m_f$upper[,1]), lo80=c(rep(NA,n), m_f$lower[,1]),
                     hi95=c(rep(NA,n), m_f$upper[,2]), lo95=c(rep(NA,n), m_f$lower[,2]))
    combo <- list(df = dfc, model=m, fcst = m_f$mean, obs_test = test)
    return(combo)
  })
  
  output$forecast <- renderPlot({
    combo <- combo_output()
    clr<-"red"
    ggplot(combo$df, aes(x=date, y=obs)) + geom_line() + 
      geom_line(aes(y=fcst), col=clr, lwd=1.1) +
      geom_point(aes(y=fcst), col=clr, pch=19, size=1.5)+
      geom_ribbon(aes(ymin=lo95,ymax=hi95),fill=clr, alpha=.25) +
      geom_ribbon(aes(ymin=lo80,ymax=hi80),fill=clr, alpha=.40) + 
      labs(x="", y="") + theme_light()+
      scale_x_date(date_breaks= "1 year", date_labels= "%Y") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$accuracy <- renderPrint({
    combo <- combo_output()
    accuracy(combo$fcst, combo$obs_test)
  })
  
  output$summary <- renderPrint({
    combo <- combo_output()
    summary(combo$model)
  })
  
  output$table = DT::renderDataTable({
    hhp_econ_round
  })
})

shinyApp(ui = ui, server = server)