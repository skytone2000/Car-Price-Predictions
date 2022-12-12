library(shiny)
library(shinydashboard)
library(tidyverse)
library(shiny)
library(dplyr)
#library(cowplot)
#library(corrplot)
#library(broom)
library(ggplot2)

library(tidyr) 
library(tidyselect)
library(plotly)
library(dplyr)
library(reactable)
library(htmlwidgets)
library('IRdisplay')
library(data.table)
library(mgcv)
library(forcats)
#install.packages("sm")
library(brglm)
library(MASS)
#library(splines)

#install.packages("shinydashboard")


dat <- read.csv(file = 'car_price_prediction.csv')

#dat <- read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")

dat$Levy[dat$Levy == "-"] <- 0
dat$Levy= as.numeric(as.character(dat$Levy))


dat <- dat %>% mutate(Car.Age =2022- Prod..year )

# replace funny name with others 
dat$Manufacturer[dat$Manufacturer == "სხვა"] <- "Others"


#spliting engine volume to engine and turbo status
dat[c('Engine','Turbo.status')] <- str_split_fixed(dat$Engine.volume, ' ', 2)

#encoding turbo status
dat$Turbo.status <- ifelse(dat$Turbo.status == "Turbo",1,0)

#removing km from mileage column
dat$Mileage <- gsub("km*",'',dat$Mileage)
dat$Mileage= as.numeric(as.character(dat$Mileage))
dat<-dat[!(dat$Mileage> 400000),]

#dropping engine.volume column
dat <- subset(dat, select = -c(Engine.volume) )


# 
dat<-dat[!(dat$Car.Age<10 & dat$Price<500),]

dat<-dat[!(dat$Price== 26307500),]


dat<-dat[!(dat$Price<= 2500),]

dat$Doors[dat$Doors == "04-May"] <- "4"
dat$Doors[dat$Doors == "02-Mar"] <- "2"

dat <- dat[,-c(1)]
###########################################

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(dat), replace=TRUE, prob=c(0.7,0.3))
dattrain  <- dat[sample, ]
dattest   <- dat[!sample, ]
dattrain
dattestaActual <- dattest[1]
dattestaActual
dattestRm <- dattest[-1]
#dim(dattrain)
#View(dattestRm)

#############fit model#########year production remove cos of car age#########################

#class(dat$Airbags)

lmodel<- lm(Price ~ Levy  + Mileage+ Cylinders+Doors+ Car.Age+Airbags+Engine+Turbo.status+ Manufacturer+Color, dattrain)
#glance(lmodel)

rlmodel <- rlm(Price ~ Levy  + Mileage+ Cylinders+Doors+ Car.Age+Airbags+Engine+Turbo.status+ Manufacturer+Color, dattrain)

#glance(rlmodel)

wlmodel<- lm(Price ~ Levy  + Mileage+ Cylinders+Doors+ Car.Age+Airbags+Engine+Turbo.status+ Manufacturer+Color,weights=Prod..year, dattrain)
#glance(wlmodel)
#plot(wlmodel)


#<- gam(Price ~ s(Levy)  + s(Mileage)+ s(Cylinders)+s(Doors)+ s(Car.Age)+  s(Airbags)+ s(Engine)+ s(Turbo.status)+ s(Manufacturer)+s(Color),family=poisson, data=sample)

#glance(glmodel)

#Testing model with test data
#data2 <- tibble(Levy= 1000,Mileage=2000,Cylinders=5,Doors='2',Car.Age=3,Airbags=16,Engine='3.3',Turbo.status=1,Manufacturer='CHEVROLET',Color='Black')


testmodel <- predict(lmodel, data=dattestRm)
testrlmodel <- predict(lmodel, data=dattestRm)
testwlmodel <- predict(lmodel, data=dattestRm)
#lmodel
#data2
#summary(testmodel)
#View(dattest1)
##View(testmodel)

#testmodel
##dim(dattestaActual)
#Plotting Actual vs predicted values of Training data

#"#9A1663","#E0144C","#FF5858","#FF5858","#CF0A0A","#DC5F00","#DD5353","#B73E3E"
#col = ifelse(dat$Manufacturer == "FORD", "#F8766D", ifelse(dat$Manufacturer == LAMBORGHINI", "#00BA38", ifelse(dat$Manufacturer == "MERCEDES-BENZ", "#619CFF", "grey75")))     


#"OPEL"         
#"TOYOTA"        
#"MERCEDES-BENZ" 
#"PORSCHE"       
#"BMW"

actualvsPredicted = cbind(actualValue = dattestaActual, 
                          predictedValue = testmodel[1:4587])
#View(actualvsPredicted)
####################################################################################################
#################scater [lots######################
############plot selection##########3##

plot_for_loop <- function(dat, x_var, y_var) {
  
  ggplot(dat, aes(y = .data[[y_var]],x = .data[[x_var]])) + 
    geom_point() + 
    # geom_smooth(size=1) +
    #geom_bar()
    # geom_point(size=1) + 
    labs(x = x_var, y = y_var) +
    theme(axis.text.x = element_text(angle = 90))
  #theme_classic(base_size = 12)
}

plot_list <- colnames(dat)[-1] %>% 
  map( ~ plot_for_loop(dat,.x, colnames(dat)[1]))




ui <- dashboardPage(
  skin="blue", 
  dashboardHeader(title='Car Price Prediction',titleWidth = 300),
  #####SIDE BAR #########
  dashboardSidebar(
    sidebarMenu(
      id ='menu1',
      menuItem('Exploratory Data Analysis',tabName = 'analysis',icon=icon("chart-simple")),
      menuItem('Prediction',tabName = 'prediction',icon=icon("car")),
      menuItem('About',tabName = 'about',icon=icon("info"))
    )
    ,
    #####CONDITIONAL PANEL FOR ANALYSIS PAGE #########
    
    conditionalPanel(
      condition = "input.menu1 =='analysis'",
      tags$hr(),
      h4('Controls'),
      selectInput(
        inputId = 'plotvar',
        label = 'Car Features',
        choices = colnames(dat)[-1],
        selected = "Manufacturer"
      ),
      sliderInput(
        "num",
        "Maximum number of Parameters",
        min=1,max=21,value=3,step = 1),
      sliderInput(
        "num2",
        "Minimum number of Parameters",
        min=1,max=21,value=6,step = 1)
    ),
    conditionalPanel(width = 3,
                     collapsed = TRUE, style = "margin-top: -20px;",
                     
                     condition = "input.menu1 =='prediction'",
                     tags$hr(),
                     h4('Model Selection'),
                     selectInput('model','Select Linear Regression Model Type:',
                                 c(
                                   'Linear model'='lm',
                                   'Robust Linear Model' = 'rlm',
                                   'Weighted Linear Model' = 'wlm'),
                                 selected = "Linear model"
                     )
                     
                     
    )  
    
  ),
  ############# BODY ####################
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
        margin-bottom: 2px;
      }
    '))),
    
    
    tabItems(
      tabItem(tabName = 'analysis',
              plotlyOutput("test"),
              plotlyOutput('test2'),
              plotlyOutput("test3")
      ),
      ###
      
      tabItem(tabName = 'prediction',
              
              #p("Waitting for price preditions"),
              #####
              plotlyOutput("test4"),
              taskItem(value = 75, color = "yellow",
                       "Car price predictions models still under contruction"
              ),
              
              
              ###############################
              
              
              fluidRow(
                box(
                  width=2,status = 'primary',solidHeader = TRUE,
                  selectInput(
                    inputId = 'manufacturer',
                    label = 'Manufacturers',
                    choices = dat$Manufacturer%>%unique()
                  )
                ),
                box(width=2,status = 'primary',solidHeader = TRUE,
                    selectInput(
                      inputId = 'color',
                      label = 'Color',
                      choices =  dat$Color%>%unique()
                    ) 
                ),
                box(width=2,status = 'primary',solidHeader = TRUE,
                    selectInput(
                      inputId = 'cat',
                      label = 'Category',
                      choices =  dat$Category%>%unique(),
                      selected = "Sedan"
                    )
                ),
                box(width=2,status = 'primary',solidHeader = TRUE,
                    numericInput(
                      inputId = 'mileage',
                      label = 'Mileage',
                      value = 2000
                    )
                ),
                box(width=2,status = 'primary',solidHeader = TRUE,
                    numericInput(
                      inputId = 'age',
                      label = 'Car Age',
                      value = 4
                    )  
                ),
                
                box(
                  width=2,status = 'primary',solidHeader = TRUE,
                  selectInput(
                    inputId = 'airbag',
                    label = 'Air Bag',
                    choices =  dat$Airbags%>%unique(),
                    selected = "12"
                  )
                )
                
                
              ) ,
              fluidRow(
                box(
                  width=2,status = 'primary',solidHeader = TRUE,
                  numericInput(
                    inputId = 'levy',
                    label = 'Levy',
                    value = 1000
                  )
                ),
                box(
                  width=2,status = 'primary',solidHeader = TRUE,
                  selectInput(
                    inputId = 'cyl',
                    label = 'Cylinders',
                    choices =  dat$Cylinders%>%unique(),
                    selected = "10"
                  )
                ),
                box(
                  width=2,status = 'primary',solidHeader = TRUE,
                  selectInput(
                    inputId = 'eng',
                    label = 'Engine',
                    choices =  dat$Engine%>%unique(),
                    selected = "5"
                  )
                ),
                box(
                  width=2,status = 'primary',solidHeader = TRUE,
                  selectInput(
                    inputId = 'turbo',
                    label = 'Turbo Status',
                    choices =  dat$Turbo.status%>%unique(),
                    selected = "5"
                  )
                ),
                box(
                  width=2,status = 'primary',solidHeader = TRUE,
                  selectInput(
                    inputId = 'door',
                    label = 'Door',
                    choices =  dat$Doors%>%unique(),
                    selected = "4"
                  )
                ),
                
                box(width= 2,title = "Predicted Price",solidHeader = TRUE, status='primary',
                    textOutput("testttt"))
                
              )
              
              ,
            
                actionButton("Submit",'submit',style="display: inline-block;
                    border-radius: 4px;
                    background-color: #ff8600;
                    color: #FFFFFF;
                    text-align: center;
                    font-size: 15px;
                    padding: 10px;
                    width: 80px;
                    cursor: pointer;
                    margin: 5px;")
              
              
             
      ),
      
      ################################
      
      
      
      ##################################
      
      
      
      
      
      tabItem(tabName = 'about',
              
              fluidRow(
              box(
              p('A very good fit, but we must be cautious. Notice that all the predictors are strongly
significant. This might be a reflection of reality or it could just be that the ACE
model is overfitting the data by using implausible transformations as seen on the
'),

br(),
p('ibt variable above. Another problem is in constructing new predictions from fresh
inputs. The nature of the smoothing used does not make it obvious how this can be
achieved.
ACE can be useful in searching for good transformations while building a linear
model. We might examine the fitted transformations as seen in Figure 15.7'),
              ),
              box(
                p('that will be $200'),
              )
              ),
              
            
              #################
      )
    )
  )
)



server <-function(input,output){
  #Levy  + Mileage+ Cylinders+Doors+ Car.Age+Airbags+Engine+Turbo.status+ Manufacturer+Color
  cap <- eventReactive(input$Submit,{
    door <- input$door
    levy <- input$levy
    mileage <- input$mileage
    cyl <- input$cyl
    age <- input$age
    airbag <- input$airbag
    engine<- input$eng
    turbo <- input$turbo
    manufacturer<- input$manufacturer
    color <- input$color
    
    data2 <- tibble(Levy= as.numeric(levy),Mileage=mileage,Cylinders=as.numeric(cyl),Doors=door,Car.Age=age,Airbags=as.numeric(airbag),Engine=engine,Turbo.status=as.numeric(turbo),Manufacturer=manufacturer,Color=color)
    
    
    
    if(input$model=='rlm'){
      testmodel2 <- predict(rlmodel, data2)
    }
    else if(input$model=='wlm') {
      testmodel2 <- predict(wlmodel, data2)
    }
    
    else {
      testmodel2 <- predict(lmodel, data2)
    }
    
    round(testmodel2)
  })
  
  output$testttt <- renderText({
    input$Submit
    isolate(sprintf("$ %s",cap()))
  })
  
  
  
  output$test2 <- renderPlotly({
    
    dat%>%group_by(across(all_of(input$plotvar)))%>%summarise(Highest_price=sort(max(Price, na.rm = T)))%>%top_n(input$num)%>%
      ggplot(aes_string(x=noquote(input$plotvar[1])     ,y='Highest_price', fill=noquote( unique(sort(input$plotvar[1]) ) )  ))+
      
      geom_bar(stat = 'identity', color="lightblue")+
      
      ylab("Highest Car Price ")+
      
      theme(axis.text.x = element_text(angle = 45))
    
  })
  output$test3 <- renderPlotly({
    
    
    # })
    
    
    dat%>%group_by(across(all_of(input$plotvar)))%>%summarise(Lowest_price=max(Price, na.rm = T)) %>%arrange(desc(Lowest_price))%>%tail(input$num2)%>%
      ggplot(aes_string(x=noquote(input$plotvar[1]) ,y='Lowest_price',fill=noquote( unique(sort(input$plotvar[1]) ) )   ))+
      
      geom_bar(stat = 'identity',color="grey")+
      
      ylab("Lowest Car Price") +
      
      theme(axis.text.x = element_text(angle = 45))
    
    
  })
  
  output$test <- renderPlotly({
    ggplot(dat, aes_string(x=noquote(input$plotvar[1]) ,y='Price')) + 
      geom_point() + 
      # geom_smooth(size=1) +
      #geom_bar()
      # geom_point(size=1) + 
      #labs(x = Car.Age, y = Price) +
      theme(axis.text.x = element_text(angle = 45))
  })
  
  #########test on iris on server with test4###########
  
  ################
  
  output$test4 <- renderPlotly({
    
    
    dat%>%group_by(Manufacturer, Prod..year)%>%summarise(Count=length(Manufacturer))%>%
      plot_ly(x =~Prod..year, y = ~Count,type="scatter",mode='lines') %>%
      layout(title='Number of Cars Produced per Year', yaxis=list(title='Overall Total number of cars'), xaxis=list(title='Production Year'))
    #theme(axis.text.x = element_text(angle = 90))
  })
  
}

shinyApp(ui = ui,server=server)


