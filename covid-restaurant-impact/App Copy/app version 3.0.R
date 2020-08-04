#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(maps)

## revenue data
revenue <- read.csv("State_Split_Daily_719.csv")
revenue$Date <- as.Date(as.character(revenue$Date))
revenue$Date.Week <- as.Date(as.character(revenue$Date.Week), format = "%d-%b-%y")

## weekly data
weekly.data <- read.csv("weekly_data_avg_full_updated.csv")
weekly.data$Date <- as.Date(as.character(weekly.data$Date))

## COVID data
us.confirmed <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

us.deaths <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

states.abbr<- read.csv("State_Abbreviation_Data.csv") 
states.abbr$State <- as.character(states.abbr$State)

## mobility 
mobility <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv?cachebust=5a11e4fc1b8138c6")

# Change format for date
mobility$date <- as.Date(as.character(mobility$date))
mobility$sub_region_1 <- as.character( mobility$sub_region_1)


## Prediction result
pct.plot <- read.csv("dine_in_pct_plot_data.csv")
amount.plot <- read.csv("amount_2020_plot_data.csv")
pct.plot$Date <- as.Date(as.character(pct.plot$Date))
amount.plot$Date <- as.Date(as.character(amount.plot$Date))



# ui
ui <- shinyUI(
    dashboardPage(title = "Demo App",
                  
                  dashboardHeader(title = "CMU Hackathon",
                                  
                                  dropdownMenu(type = "messages",
                                               messageItem(
                                                   from = "Developers",
                                                   message = "COVID-19 & Mobility data is up-to-date.",
                                                   time = Sys.Date()),
                                               
                                               messageItem(
                                                   from = "Support",
                                                   message = "Developed by CMU Hackathon Team #3.",
                                                   icon = icon("life-ring"))
                                  )
                  ),
                  
                  dashboardSidebar(
                      
                      sidebarMenu(
                          
                          sidebarSearchForm("searchText", "buttonSearch", "search"),
                          
                          menuItem("Welcome", tabName = "welcome", icon = icon("laugh-wink")),
                          
                          menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
                          
                          menuItem("Data Analysis", tabName = "analysis", icon = icon("pencil-alt"),
                                   
                                   menuSubItem("Restaurant Revenues", tabName = "revenue",
                                               icon= NULL),
                                   menuSubItem("COVID-19 Pandemic", tabName = "pandemic",
                                               icon= NULL),
                                   menuSubItem("Mobility", tabName = "mobility",
                                               icon= NULL),
                                   menuSubItem("Explore Correlation", tabName = "correlation",
                                               icon= NULL),
                                   startExpanded = TRUE
                          ),
                          
                          
                          menuItem(text = "Prediction", tabName = "prediction", icon = icon("line-chart"),
                                   
                                   menuSubItem("Short-term: 4 Weeks", tabName = "short",
                                               icon= NULL),
                                   menuSubItem("Long-term", tabName = "long",
                                               icon= NULL),
                                   startExpanded = TRUE)
                      )
                      
                  ),
                  
                  
                  
                  dashboardBody(
                      
                      tags$head(includeCSS('www/style.css')),
                      
                      tabItems(
                          tabItem(tabName = "welcome",
                                  br(), br(),
                                  h1("Welcome to this web application!"),
                                  br(),
                                  tagList(
                                      fluidRow(tags$img(src = "welcome.jpg", width = "100%", height = "100%", align = "left"))),
                                  #img(src="welcome.jpg",  height = 560, width = 1170, align = "left")
                          ),
                          
                          tabItem(tabName = "dashboard",
                                  br(), br(),
                                  h1(strong("How COVID-19 is Affecting US Restaurants")),
                                  br(),
                                  tagList(
                                      fluidRow(tags$img(src = "restaurant.jpg", width = "100%", height = "100%", align = "left")),
                                      tags$br()),
                                  #img(src="restaurant.jpg",  height = 350, width = 1170, align = "left"),
                                  h2(strong("Background")),
                                  h4(),
                                  br(),
                                  h2(strong("Methodology")),
                                  h4(),
                                  br(),
                                  h2(strong("Our Work")),
                                  h4(),
                                  br(),
                                  h2(strong("About the Data")),
                                  h4(),
                                  br()
                                  
                                  
                          ),
                          
                          tabItem(tabName = "revenue",
                                  br(), br(),
                                  h1("Analysis of Restaurant Revenues"),
                                  br(),
                                  p("Source: Toast Platform & Rally for Restaurant"),
                                  
                                  fluidRow(
                                      
                                      tabBox(
                                          
                                          tabPanel(title = "Weekly Average Revenue vs. Date by Year", 
                                                   status = "primary", solidHeader = T,
                                                   plotlyOutput("totalPlot.1")),
                                          
                                          tabPanel(title = "Daily Revenue vs. Date by Year", 
                                                   status = "primary", solidHeader = T,
                                                   plotlyOutput("totalPlot.2")),  # total revenue
                                          
                                          width = 9),
                                      
                                      box(title = "Select State and Date", status = "warning", solidHeader = T,
                                          selectInput("var", 
                                                      label = "Choose a State to display",
                                                      choices = c("(All)","AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE",
                                                                  "FL", "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
                                                                  "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "NE", "NH", 
                                                                  "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
                                                                  "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"),  #levels(revenue$State),  
                                                      selected = "(All)"),
                                          
                                          dateRangeInput('dateRange',
                                                         label = 'Date range input: yyyy-mm-dd',
                                                         start = as.Date("2020-2-17"), end = as.Date("2020-7-19"),
                                                         min = as.Date("2020-2-17"), max = as.Date("2020-7-19")),
                                          width = 3
                                      ),
                                      
                                      
                                      tabBox(
                                          
                                          tabPanel(title = "Weekly Average Revenue by Dining Options", 
                                                   status = "primary", solidHeader = T,
                                                   plotlyOutput("pctPlot.1")),  
                                          
                                          tabPanel(title = "Daily Revenue by Dining Options", 
                                                   status = "primary", solidHeader = T,
                                                   plotlyOutput("pctPlot.2")),  
                                          
                                          width = 9)
                                      
                                  )
                                  
                          ),
                          
                          tabItem(tabName = "pandemic",
                                  br(), br(),
                                  h1("Analysis of COVID-19 Pandemics"),
                                  h2("(Data Updated Today)"),
                                  br(),
                                  p("Source: John Hopkins University"),
                                  
                                  fluidRow(
                                      tabBox(
                                          tabPanel(title = "Total Comfirmed Cases in US",
                                                   plotlyOutput("heatmap.1")),
                                          
                                          tabPanel(title = "Total Death Cases in US",
                                                   plotlyOutput("heatmap.2")),
                                          width = 9
                                      ),
                                      
                                      tabBox(
                                          tabPanel(title = "Time-series Cases in US",
                                                   plotlyOutput("casePlot.1")),
                                          
                                          tabPanel(title = "Weekly Average Reported Cases in US",
                                                   plotlyOutput("casePlot.2")),
                                          width = 9
                                      ), 
                                      
                                      box(title = "Select State and Date", status = "warning", solidHeader = T,
                                          selectInput("var3", 
                                                      label = "Choose a State to display",
                                                      choices = c("(All)","AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE",
                                                                  "FL", "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
                                                                  "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "NE", "NH", 
                                                                  "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
                                                                  "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"),  #levels(revenue$State),  
                                                      selected = "(All)"),
                                          
                                          dateRangeInput('dateRange3',
                                                         label = 'Date range input: yyyy-mm-dd',
                                                         start = as.Date("2020-1-22"), end = Sys.Date()-1,
                                                         min = as.Date("2020-1-22"), max = Sys.Date()-1),
                                          width = 3
                                      )
                                  )
                                  
                          ),
                          
                          
                          tabItem(tabName = "mobility",
                                  br(), br(),
                                  h1("Community Mobility since COVID-19 Pandemics"),
                                  br(),
                                  p("Source: Google"),
                                  
                                  fluidRow(
                                      
                                      tabBox(
                                          
                                          tabPanel(title = "Weekly Average Mobility Percent Change in US", 
                                                   status = "primary", solidHeader = T,
                                                   plotlyOutput("mobPlot.1")),
                                          
                                          tabPanel(title = "Daily Mobility Percent Change in US", 
                                                   status = "primary", solidHeader = T,
                                                   plotlyOutput("mobPlot.2")),
                                          
                                          width = 9),
                                      
                                      box(title = "Select State and Date", status = "warning", solidHeader = T,
                                          selectInput("var2", 
                                                      label = "Choose a State to display",
                                                      choices = c("(All)","AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE",
                                                                  "FL", "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
                                                                  "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "NE", "NH", 
                                                                  "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
                                                                  "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"),  #levels(revenue$State),  
                                                      selected = "(All)"),
                                          
                                          dateRangeInput('dateRange2',
                                                         label = 'Date range input: yyyy-mm-dd',
                                                         start = as.Date("2020-2-17"), end = Sys.Date()-2,
                                                         min = as.Date("2020-2-17"), max = Sys.Date()-2),
                                          width = 3
                                      )
                                      
                                  )
                                  
                          ),
                          
                          tabItem(tabName = "correlation",
                                  br(), br(),
                                  h1("Explore Correlation between "),
                                  
                                  fluidRow(
                                      
                                      box(title = "Weekly Average Dine-in Revenues Trend & Mobility Trend", 
                                          status = "primary", solidHeader = T,
                                          plotlyOutput("corPlot.1"), 
                                          width = 9),
                                      
                                      box(title = "Select State and Date", status = "warning", solidHeader = T,
                                          selectInput("var4", 
                                                      label = "Choose a State to display",
                                                      choices = c("(All)","AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE",
                                                                  "FL", "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
                                                                  "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "NE", "NH", 
                                                                  "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
                                                                  "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"),  #levels(revenue$State),  
                                                      selected = "(All)"),
                                          
                                          width = 3
                                      )
                                  )
                          ),
                          
                          
                          tabItem(tabName = "prediction",
                                  br(), br(),
                                  h1("Prediction dashboard")
                                  
                          ),
                          
                          tabItem(tabName = "short",
                                  br(), br(),
                                  h1("Prediction for 4 Weeks"),br(),
                                  
                                  fluidRow(
                                      
                                      box(title = "Prediction of Total Revenues", 
                                          status = "primary", solidHeader = T,
                                          plotlyOutput("amountPlot.1"), 
                                          width = 9),
                                      
                                      box(title = "Select State", status = "warning", solidHeader = T,
                                          selectInput("var5", 
                                                      label = "Choose a State to display",
                                                      choices = c("(All)","AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE",
                                                                  "FL", "GA", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA",
                                                                  "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "NE", "NH", 
                                                                  "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC",
                                                                  "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV"),  #levels(revenue$State),  
                                                      selected = "(All)"),
                                          
                                          width = 3
                                      ),
                                      
                                      box(title = "Prediction of Dine-in Revenues Percentage", 
                                          status = "primary", solidHeader = T,
                                          plotlyOutput("dine.in.pctPlot.1"), 
                                          width = 9)
                                      
                                      
                                  )
                                  
                          )
                          
                          
                      )
                      
                  )
    )
)





# server
server <- shinyServer(function(input, output){
    
    ######
    ## revenue
    ###### 
    rev.dataInput <- reactive({
        
        if(input$var != "(All)") {
            data <- revenue %>%
                filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
                filter(State == input$var)
        } else {
            data <- revenue %>%
                group_by(Date) %>%
                summarize(Amount_2019 = sum(Amount_2019),
                          Amount_2020 = sum(Amount_2020),
                          Total_Trend = (Amount_2020 - Amount_2019)/Amount_2019,
                          Dine_In_2020 = sum(Dine_In_2020),
                          Off_Prem_2020 = sum(Off_Prem_2020),
                          Dine_In_2020_pct = Dine_In_2020/Amount_2020,
                          Off_Prem_2020_pct = Off_Prem_2020/Amount_2020) %>%
                filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
        }
        data
    })
    
    
    rev.weekly.dataInput <- reactive({
        
        data <- weekly.data[,c(1,2,4:9)] %>%
            mutate(Dine_In_2020_weekly_pct = Dine_In_2020_weekly/Amount_2020_weekly,
                   Off_Prem_2020_weekly_pct = Off_Prem_2020_weekly/Amount_2020_weekly)
        
        if(input$var != "(All)") {
            data <- data %>%
                filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
                filter(State == input$var)
        } else {
            data <- data %>%
                group_by(Date) %>%
                summarize(Amount_2019_weekly = sum(Amount_2019_weekly),
                          Amount_2020_weekly = sum(Amount_2020_weekly),
                          Dine_In_2020_weekly = sum(Dine_In_2020_weekly),
                          Off_Prem_2020_weekly = sum(Off_Prem_2020_weekly),
                          Dine_In_2020_weekly_pct = Dine_In_2020_weekly/Amount_2020_weekly,
                          Off_Prem_2020_weekly_pct = Off_Prem_2020_weekly/Amount_2020_weekly) %>%
                filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
        }
        data
    })
    
    
    output$totalPlot.1 <- renderPlotly({
        rev.weekly.dataInput() %>%
            plot_ly( x = ~Date, y = ~Amount_2019_weekly, type ="scatter", mode = "lines", 
                     color = I("blue"), name = "2019") %>%
            add_trace(x = ~Date, y = ~Amount_2020_weekly, type ="scatter", mode = "lines", 
                      color = I("red"), name = "2020") %>%
            layout(title = "Weekly Average Revenue vs. Date by Year", yaxis = list(title = "Total revenues"))
    })
    
    output$totalPlot.2 <- renderPlotly({
        rev.dataInput() %>%
            plot_ly( x = ~Date, y = ~Amount_2019, type ="scatter", mode = "lines", 
                     color = I("blue"), name = "2019") %>%
            add_trace(x = ~Date, y = ~Amount_2020, type ="scatter", mode = "lines", 
                      color = I("red"), name = "2020") %>%
            layout(title = "Daily Revenue vs. Date by Year", yaxis = list(title = "Total revenues"))
    })
    
    
    output$pctPlot.1 <- renderPlotly({
        rev.weekly.dataInput() %>%
            plot_ly( x = ~Date, y = ~Dine_In_2020_weekly_pct*100, type ="scatter", mode = "lines", 
                     color = I("blue"), name = "Dine-in") %>%
            add_trace(x = ~Date, y = ~Off_Prem_2020_weekly_pct*100, type ="scatter", mode = "lines", 
                      color = I("red"), name = "Off-premise") %>%
            layout(title = "Weekly Average Revenue by Dining Options", yaxis = list(title = "Percentage"))
        
    })
    
    output$pctPlot.2 <- renderPlotly({
        rev.dataInput() %>%
            plot_ly( x = ~Date, y = ~Dine_In_2020_pct*100, type ="scatter", mode = "lines", 
                     color = I("blue"), name = "Dine-in") %>%
            add_trace(x = ~Date, y = ~Off_Prem_2020_pct*100, type ="scatter", mode = "lines", 
                      color = I("red"), name = "Off-premise") %>%
            layout(title = "Daily Revenue by Dining Options", yaxis = list(title = "Percentage"))
        
    })
    
    
    
    ######
    ## case
    ###### 
    
    
    combined.dataInput <- reactive({
        
        # Format time
        time.stamp <- seq.Date(as.Date("2020/1/22"), 
                               as.Date(colnames(us.confirmed)[ncol(us.confirmed)], format =  "%m/%d/%y"), "days")
        colnames(us.confirmed)[12:ncol(us.confirmed)] <- as.character(time.stamp)
        colnames(us.deaths)[13:ncol(us.deaths)] <-as.character(time.stamp)
        
        # Tidy data, combine time series
        us.confirmed <- us.confirmed %>%
            pivot_longer(12:ncol(us.confirmed),
                         names_to = "time", values_to = "confirmed")
        us.deaths <- us.deaths %>%
            pivot_longer(13:ncol(us.deaths),
                         names_to = "time", values_to = "deaths")
        us.confirmed$time <- as.Date(us.confirmed$time)
        us.deaths$time <- as.Date(us.deaths$time)
        
        # Combine the confirmed and deaths data into single one, only for US
        us.combined <- left_join(us.confirmed, us.deaths[,c(11,13,14,12)],
                                 by = colnames(us.confirmed)[11:12]) %>%
            filter(iso2 == "US") 
        
        us.combined
    })
    
    
    # for every day, t-s cases and added cases for all states -> length = # of days
    total.daily.case.dataInput <- reactive({
        us.total.case <-combined.dataInput() %>%
            group_by(time) %>%
            summarize(confirmed = sum(confirmed),
                      deaths = sum(deaths)) %>%
            mutate(confirmed.add = confirmed, deaths.add = deaths)  # daily add
        
        for (i in 2:nrow(us.total.case)) {
            us.total.case[i,4] = us.total.case[i,2] - us.total.case[i-1,2] 
            us.total.case[i,5] = us.total.case[i,3] - us.total.case[i-1,3] 
        }
        us.total.case
    })
    
    
    # t-s cases for each state -> length = # days * # states
    ts.case.state.dataInput <- reactive({
        
        # Combine county data in every state
        cases.total <- combined.dataInput() %>%
            #left_join(states.abbr[, c(1,3)], by = c("Province_State" = "State")) %>%  # add state abbr
            #na.omit() %>%
            group_by(Province_State) %>%  # combine counties in each ST
            mutate(sum.population = sum(Population)/nrow(total.daily.case.dataInput())) 
        
        cases.total <- cases.total %>%
            group_by(Province_State, time) %>% 
            summarize(confirmed = sum(confirmed),
                      deaths = sum(deaths),
                      population = mean(sum.population))
        
        # Calculate daily-increase cases
        cases.total <- mutate(cases.total, confirmed.add = confirmed, deaths.add = deaths) 
        
        for (i in setdiff(2:nrow(cases.total), seq((nrow(total.daily.case.dataInput())+1), 
                                                   nrow(cases.total), nrow(total.daily.case.dataInput())))) {
            cases.total[i,6] = cases.total[i,3] - cases.total[i-1,3] 
            cases.total[i,7] = cases.total[i,4] - cases.total[i-1,4] 
        }
        
        cases.total <- cases.total %>%
            left_join(states.abbr[, c(1,3)], by = c("Province_State" = "State") )
        
        cases.total
    })
    
    
    # total cases in each state, up to date
    total.case.dataInput <- reactive({
        data <- combined.dataInput() %>%
            filter(time == last(time)) %>%
            group_by(Province_State) %>%
            summarize(sum.confirmed = sum(confirmed, na.rm = TRUE),
                      sum.deaths = sum(deaths, na.rm = TRUE)) %>%
            left_join(states.abbr, by=c("Province_State"= "State")) %>%
            na.omit() 
        data
    })
    
    
    
    output$heatmap.1 <- renderPlotly({
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        
        plot_geo(total.case.dataInput(), locationmode = 'USA-states') %>% 
            add_trace(
                z = ~sum.confirmed, text = ~Province_State, 
                locations = ~Code,
                color = ~log(sum.confirmed), colors = 'Reds') %>% 
            colorbar(title = "Total Confirmed Cases") %>% 
            layout(title = "Total Confirmed Cases in US", 
                   geo = g)
        
    })
    
    
    output$heatmap.2 <- renderPlotly({
        
        g <- list(
            scope = 'usa',
            projection = list(type = 'albers usa'),
            showlakes = TRUE,
            lakecolor = toRGB('white')
        )
        
        plot_geo(total.case.dataInput(), locationmode = 'USA-states') %>% 
            add_trace(
                z = ~sum.deaths, text = ~Province_State, 
                locations = ~Code,
                color = ~log(sum.deaths), colors = 'Reds') %>% 
            colorbar(title = "Total Deaths Cases") %>% 
            layout(title = "Total Death Cases in US", 
                   geo = g)
        
    })
    
    
    ## weekly case data, based on t-s case data
    case.weekly.dataInput <- reactive({
        # num of days: 
        num <- nrow(total.daily.case.dataInput() )
        num.week.day <- (num %/% 7)*7
        num.single.day <- num %% 7
        
        if (num.single.day != 0) {
            # full week
            weekly.case.1 <- ts.case.state.dataInput() %>%
                mutate(confirmed.add_weekly = 0,
                       deaths.add_weekly = 0) %>%
                filter(time <= as.Date("2020-1-22") + num.week.day - 1 )
            
            for (j in 9:10) {
                for (i in seq(1, nrow(weekly.case.1 ), 7)) {
                    weekly.case.1 [i,j] = sum(weekly.case.1 [i:(i+6),j-3]/7)
                }
            }
            weekly.case.1 <- weekly.case.1[seq(1, nrow(weekly.case.1), 7),]
            
            # left days
            weekly.case.2 <- ts.case.state.dataInput()%>%
                mutate(confirmed.add_weekly = 0,
                       deaths.add_weekly = 0) %>%
                filter(time >= as.Date("2020-1-22") + num.week.day)
            
            for (j in 9:10) {
                for (i in seq(1, nrow(weekly.case.2), num.single.day)) { 
                    weekly.case.2[i,j] = sum(weekly.case.2[i:(i+num.single.day-1),j-3]/num.single.day)
                }
            }
            weekly.case.2 <- weekly.case.2[seq(1, nrow(weekly.case.2), num.single.day),]
            
            # combine subsets
            weekly.case <- rbind(weekly.case.1, weekly.case.2)[, -c(6,7)] %>%
                arrange(Province_State)   # complete!
            
        } else {
            weekly.case <- ts.case.state.dataInput() %>%
                mutate(confirmed.add_weekly = 0,
                       deaths.add_weekly = 0) %>%
                filter(time <= as.Date("2020-1-22") + num.week.day - 1 )
            
            for (j in 9:10) {
                for (i in seq(1, nrow(weekly.case), 7)) {
                    weekly.case[i,j] = sum(weekly.case[i:(i+6),j-3]/7)
                }
            }
            weekly.case <- weekly.case[seq(1, nrow(weekly.case), 7), -c(6,7)]
        }
        
        weekly.case
    })
    
    
    casePlot.1.dataInput <- reactive({
        if(input$var3 != "(All)") {
            data <- ts.case.state.dataInput() %>%
                filter(time >= input$dateRange3[1] & time<= input$dateRange3[2]) %>%
                filter(Code == input$var3)
        }  else {
            data <- ts.case.state.dataInput() %>%
                filter(time >= input$dateRange3[1] & time<= input$dateRange3[2]) %>%
                group_by(time) %>%
                summarize(confirmed= sum(confirmed),
                          deaths = sum(deaths))
        }
        data
    })
    
    
    casePlot.2.dataInput <- reactive({
        if(input$var3 != "(All)") {
            data <- case.weekly.dataInput() %>%
                filter(time >= input$dateRange3[1] & time<= input$dateRange3[2]) %>%
                filter(Code == input$var3)
        }  else {
            data <- case.weekly.dataInput() %>%
                filter(time >= input$dateRange3[1] & time<= input$dateRange3[2]) %>%
                group_by(time) %>%
                summarize(confirmed.add_weekly = sum(confirmed.add_weekly),
                          deaths.add_weekly = sum( deaths.add_weekly))
        }
        data
    })
    
    
    output$casePlot.1 <- renderPlotly({
        casePlot.1.dataInput()  %>%
            plot_ly( x = ~time, y = ~confirmed, type ="scatter", mode = "lines", 
                     color = I("blue"), name = "Total Confirmed") %>%
            add_trace(x = ~time, y = ~deaths, yaxis = "y2", type ="scatter", mode = "lines", 
                      color = I("red"), name = "Total Deaths") %>%
            layout(title = "Time-series Cases in US", xaxis = list(title = "Date"),
                   yaxis = list(title = "Confirmed Cases"),
                   yaxis2 = list(title = "Death Cases", overlaying = "y", side = "right"))
        
    })
    
    output$casePlot.2 <- renderPlotly({
        casePlot.2.dataInput() %>%
            plot_ly( x = ~time, y = ~confirmed.add_weekly, type ="scatter", mode = "lines", 
                     color = I("blue"), name = "Weekly Average Confirmed") %>%
            add_trace(x = ~time, y = ~deaths.add_weekly, yaxis = "y2", type ="scatter", mode = "lines", 
                      color = I("red"), name = "Weekly Average Deaths") %>%
            layout(title = "Weekly Average Reported Cases in US", xaxis = list(title = "Date"),
                   yaxis = list(title = "Confirmed Cases"),
                   yaxis2 = list(title = "Death Cases", overlaying = "y", side = "right"))
        
    })
    
    
    ######
    ## mobility
    ###### 
    mob.dataInput <- reactive({
        
        us.mob <- mobility[,c(1,3,4,5,7,8,12,13)] %>%
            filter(country_region_code == "US"& sub_region_2 == "") %>%
            left_join(states.abbr[,c(1,3)], by =c("sub_region_1" = "State"))
        colnames(us.mob)[6:8] <- c("retail_and_recreation_pct", "workplace_pct", "residential_pct")
        
        if(input$var2 != "(All)") {
            data <- us.mob %>%
                filter(date >= input$dateRange2[1] & date <= input$dateRange2[2]) %>%
                filter(Code == input$var2) 
        } else{
            data <- us.mob  %>%
                filter(sub_region_1 == "") %>%
                filter(date >= input$dateRange2[1] & date <= input$dateRange2[2])
        }
        data
    })
    
    mob.weekly.dataInput <- reactive({
        
        us.mob <- mobility[,c(1,3,4,5,7,8,12,13)] %>%
            filter(country_region_code == "US" & sub_region_2 == "") %>%
            left_join(states.abbr[,c(1,3)], by =c("sub_region_1" = "State")) %>%
            filter(date >= as.Date("2020-2-17"))  # only get 2/17 and later
        
        colnames(us.mob)[6:8] <- c("retail_and_recreation_pct", "workplace_pct", "residential_pct")
        us.total.mob <- us.mob %>%
            filter(sub_region_1 == "")
        
        # num of days: nrow(us.total.mob)
        num <- nrow(us.total.mob)
        num.week.day <- (num %/% 7)*7
        num.single.day <- num %% 7
        
        us.mob <- us.mob %>%
            mutate(retail_and_recreation_pct_weekly = 0,
                   workplace_pct_weekly = 0,
                   residential_pct_weekly = 0)
        
        if (num.single.day != 0) {
            # full week
            us.mob.1 <- us.mob %>%
                filter(date <= as.Date("2020-2-17") + num.week.day - 1 )
            
            for (j in 10:12) {
                for (i in seq(1, nrow(us.mob.1), 7)) {
                    us.mob.1[i,j] = sum(us.mob.1[i:(i+6),j-4]/7)
                }
            }
            us.mob.1 <- us.mob.1[seq(1, nrow(us.mob.1), 7),]
            
            # left days
            us.mob.2 <- us.mob %>%
                filter(date >= as.Date("2020-2-17") + num.week.day)
            
            for (j in 10:12) {
                for (i in seq(1, nrow(us.mob.2), num.single.day)) {
                    us.mob.2[i,j] = sum(us.mob.2[i:(i+num.single.day-1),j-4]/num.single.day)
                }
            }
            us.mob.2 <- us.mob.2[seq(1, nrow(us.mob.2), num.single.day),]
            
            # combine subsets
            us.mob <- rbind(us.mob.1, us.mob.2) %>%
                arrange(sub_region_1)   # complete!
            
        } else {
            us.mob <- us.mob %>%
                filter(date <= as.Date("2020-2-17") + num.week.day - 1 )
            
            for (j in 10:12) {
                for (i in seq(1, nrow(us.mob), 7)) {
                    us.mob[i,j] = sum(us.mob[i:(i+6),j-4]/7)
                }
            }
            us.mob <- us.mob[seq(1, nrow(us.mob), 7),]
        }
        
        
        if(input$var2 != "(All)") {
            data <- us.mob %>%
                filter(date >= input$dateRange2[1] & date <= input$dateRange2[2]) %>%
                filter(Code == input$var2) 
        } else{
            data <- us.mob %>%
                filter(date >= input$dateRange2[1] & date <= input$dateRange2[2]) %>%
                filter(sub_region_1== "") 
        }
        data
    })
    
    
    output$mobPlot.1 <- renderPlotly({
        mob.weekly.dataInput() %>%
            plot_ly( x = ~date, y = ~retail_and_recreation_pct_weekly, type ="scatter", mode = "lines", 
                     color = I("blue"), name = "Retail & Recreation") %>%
            add_trace(x = ~date, y = ~residential_pct_weekly, type ="scatter", mode = "lines", 
                      color = I("red"), name = "Residential") %>%
            add_trace(x = ~date, y = ~workplace_pct_weekly, type ="scatter", mode = "lines", 
                      color = I("orange"), name = "Workplace") %>%
            layout(title = "Weekly Average Mobility Percent Change in US", xaxis = list(title = "Date"),
                   yaxis = list(title = "Percentage compared to baseline"))
        
    })
    
    
    output$mobPlot.2 <- renderPlotly({
        mob.dataInput() %>%
            plot_ly( x = ~date, y = ~retail_and_recreation_pct, type ="scatter", mode = "lines", 
                     color = I("blue"), name = "Retail & Recreation") %>%
            add_trace(x = ~date, y = ~residential_pct, type ="scatter", mode = "lines", 
                      color = I("red"), name = "Residential") %>%
            add_trace(x = ~date, y = ~workplace_pct, type ="scatter", mode = "lines", 
                      color = I("orange"), name = "Workplace") %>%
            layout(title = "Daily Mobility Percent Change in US", xaxis = list(title = "Date"),
                   yaxis = list(title = "Percentage compared to baseline"))
        
    })
    
    
    
    
    
    
    ######
    ## correlation
    ###### 
    
    
    cor.weekly.dataInput <- reactive({
        if(input$var4 != "(All)") {
            data <- weekly.data %>%
                filter(State == input$var4)
        } else{
            data <- weekly.data %>%
                group_by(Date) %>%
                summarize(Dine_In_2020_weekly = sum(Dine_In_2020_weekly)) 
            
            us.mob <- mobility[,c(1,3,4,5,7,8,12,13)] %>%
                filter(country_region_code == "US" & sub_region_2 == "") %>%
                left_join(states.abbr[,c(1,3)], by =c("sub_region_1" = "State")) %>%
                filter(date >= as.Date("2020-2-17") )  # & date <= as.Date("2020-7-5")
            
            us.total.mob <- us.mob %>%
                filter(sub_region_1 == "")
            
            us.total.mob <- us.total.mob %>%
                mutate(recreation_mob_weekly = 0,
                       workplace_mob_weekly = 0,
                       residential_mob_weekly = 0)
            
            for (j in 10:12) {
                for (i in seq(1, 152, 7)) {
                    us.total.mob[i,j] = sum(us.total.mob[i:(i+6),j-4]/7)
                }
            }
            data <- cbind(data, us.total.mob[seq(1, 152, 7), 10:12])
            
        }
        data
    })
    
    output$corPlot.1 <- renderPlotly({
        cor.weekly.dataInput() %>%
            plot_ly( x = ~Date, y = ~Dine_In_2020_weekly, type ="scatter", mode = "lines", 
                     color = I("dark green"), name = "Dine-in Revenues") %>%
            add_trace(x = ~Date, y = ~residential_mob_weekly, yaxis = "y2", type ="scatter", mode = "lines", 
                      color = I("red"), name = "Residential") %>%
            add_trace(x = ~Date, y = ~recreation_mob_weekly, yaxis = "y2", type ="scatter", mode = "lines", 
                      color = I("blue"), name = "Retail & Recreation") %>%
            layout(title = "Weekly Average Dine-in Revenues Trend & Mobility Trend in 2020", xaxis = list(title = "Date"),
                   yaxis = list(title = "Dine-in Revenues"),
                   yaxis2 = list(title = "Percentage compared to baseline", overlaying = "y", side = "right"))
        
    })
    
    
    amount.dataInput <- reactive({
        if(input$var5 != "(All)") {
            data <- amount.plot %>%
                filter(State == input$var5)
        } else{
            data <- amount.plot %>%
                group_by(Date) %>%
                summarize(pred.1 = sum(pred.1),
                          pred.2 = sum(pred.2),
                          pred.3 = sum(pred.3),
                          pred.4 = sum(pred.4),
                          pred.5 = sum(pred.5),
                          pred.6 = sum(pred.6),
                          pred.7 = sum(pred.7),
                          pred.8 = sum(pred.8),
                          Amount_2020_weekly = sum(Amount_2020_weekly))
        }
        data
    })
    
    output$amountPlot.1 <- renderPlotly({
        amount.dataInput() %>%
            filter(Date <= as.Date("2020-7-6")) %>%
            plot_ly(x = ~Date, y = ~Amount_2020_weekly, name = 'Real Data', type = 'scatter', mode = 'lines') %>%
            add_trace(data = amount.dataInput() [amount.dataInput() $Date >= as.Date("2020-7-6"),],
                      y = ~pred.2, name = 'Best Case', mode = 'none', fill = 'tozeroy', fillcolor = '#99e699') %>%    
            add_trace(y = ~pred.3, name = 'Base Case', mode = 'none', fill = 'tozeroy', fillcolor = '#6699ff') %>% 
            add_trace(y = ~pred.5, name = 'Worst Case', mode = 'none', fill = 'tozeroy', fillcolor = '#ff884d') %>% 
            add_trace(y = ~pred.6, name = '', mode = 'none', fill = 'tozeroy', fillcolor = 'white')  %>%
            add_trace(y = ~pred.8, name = 'Average', mode = 'lines')%>%
            layout(title = 'Prediction Total Revenues in 4 Weeks',
                   xaxis = list(title = "Date",
                                showgrid = FALSE),
                   yaxis = list(title = "Total Revenues",
                                showgrid = FALSE))
    })
    
    
    dine.in.pct.dataInput <- reactive({
        if(input$var5 != "(All)") {
            data <- pct.plot %>%
                filter(State == input$var5)
        } else{
            avg.amount.1 <- amount.plot[amount.plot$Date >  as.Date("2020-7-6"), c(1,2,10)]
            colnames(avg.amount.1)[3] = "Amount_2020_weekly"
            avg.amount <- rbind(avg.amount.1, amount.plot[amount.plot$Date <=  as.Date("2020-7-6"), c(1,2,11)]) %>%
                arrange(State, Date) %>%
                left_join(pct.plot, by = c("State" = "State", "Date" = "Date"))
            
            data <-  avg.amount %>%
                group_by(Date) %>%
                summarize(Dine_In_2020_weekly_pct = sum(Amount_2020_weekly*Dine_In_2020_weekly_pct)/sum(Amount_2020_weekly))
        }
        data
    })
    
    
    
    output$dine.in.pctPlot.1 <- renderPlotly({
        dine.in.pct.dataInput() %>%
            filter( Date <= as.Date("2020-7-6")) %>%
            plot_ly(x = ~Date, y = ~Dine_In_2020_weekly_pct, name = 'Real Data', type = 'scatter', mode = 'lines') %>%
            add_trace(data = dine.in.pct.dataInput()[dine.in.pct.dataInput()$Date >= as.Date("2020-7-6"),],
                      y = ~Dine_In_2020_weekly_pct, name = 'Predict', mode = 'lines') %>%
            layout(title = 'Prediction of Dine-in Revenues Percentage in 4 Weeks',
                   xaxis = list(title = "Date",
                                showgrid = FALSE),
                   yaxis = list(title = "Dine-in Revenues Percentage",
                                showgrid = FALSE))
    })
    
    
    
    
})

# Run the application 
shinyApp(ui = ui, server = server)
