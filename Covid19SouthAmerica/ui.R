
#user interface: front-end

library(shiny)
library(shinythemes)







shinyUI(fluidPage(
  theme=shinytheme('sandstone'),
    navbarPage(
        "Covid-19 in South American Countries",
        tabPanel("Map-COVID 19",
                 fluidPage(
                   fluidRow(
                     column(2,
                            selectInput("cl_map", "Select the required information:",
                               choices=c("Total infections" ,"Total deaths", "Fatality rate (%)" ),
                               selected=c("Total infections"),
                               multiple=FALSE)),
                     column(width=8,offset = 1,
                            sliderInput('date_range3',"Select the desired time ranges",
                                        min = as.Date("2020-03-01","%Y-%m-%d"),
                                        max = today(),
                                        value=range(unique(df_covid$dateRep)),
                                        timeFormat="%y-%m-%d",width='600px',
                                        animate = animationOptions(loop = TRUE, interval = 1000)))) ,
                   
                   
                   
                   fluidRow(
                     splitLayout(cellWidths = c("50%", "50%"), leafletOutput("map1",height="600px" ), plotlyOutput("plot3",height="600px"))
                     
                     
                   )
                 )
                 
                 
                 
        ),
        tabPanel("General trends",
                 tabsetPanel(
                     tabPanel("Daily cases",
                              sidebarPanel(
                                  selectInput("yvar", "Select the dependant variable:",
                                              choices=c("Daily cases" ,"Daily deaths", "Cumulative cases" ),
                                              selected=c("Daily cases"),
                                              multiple=FALSE) ,
                                  dateRangeInput("date_range","Enter the desired time window",
                                                 min="2019-12-31",
                                                 max=today(),
                                                 start="2019-12-31",
                                                 format = "yyyy-mm-dd"),
                                  checkboxGroupInput("countries","Select the country:",choices=unique(df_covid$countriesAndTerritories),
                                                     selected = unique(df_covid$countriesAndTerritories)),
                                  width = 3
                              ),
                              mainPanel(h3("Daily statistics"),
                                        plotlyOutput("plot1",height ="600px" ))
                     ),
                     tabPanel("Total cases",
                              sidebarPanel(
                                  selectInput("country", "Select the country:",
                                              choices=unique(df_covid$countriesAndTerritories),
                                              selected=c("Argentina"),
                                              multiple=FALSE),
                                  sliderInput('date_range2',"Select the desired time ranges",
                                              min = as.Date("2020-03-01","%Y-%m-%d"),
                                              max = today(),
                                              value=range(unique(df_covid$dateRep)),
                                              timeFormat="%y-%m-%d",width='400px'),
                                  width=3
                                                                
                                  ),
                              mainPanel(h3("Total statistics "),
                                        plotlyOutput("plot2",height ="400px" ),
                                        strong("Total number of covid-19 infections within the selected time window:"),
                                        span(textOutput("infections"),style="color:red"),
                                        br(),
                                        strong("Total number of deaths by covid-19 within the selected time window:"),
                                        span(textOutput("countrydeaths"),style="color:blue"))
                              
                              
                              ))
        ),
        
        tabPanel("Raw Data ",
                 titlePanel("Data from the European Centre for Disease Prevention and Control"),
                 
                 fluidRow(column(DT::dataTableOutput("RawData")
                                 ,width=12)),
                 fluidRow(
                     column(6, align="center", offset = 3,
                            downloadButton("download",label = "Download Updated Raw Data")
                     ))
                 ),
        tabPanel("About this web app",
                 tags$h4("Motivation"),
                 "This site was created with the main objective of visualize the impact of COVID-19 over 
                 south american countries from the start of the pandemic until nowadays. ", tags$br(),
                 tags$h4("Data"),
                 "All the information used in this site belongs to",
                 tags$a(href="https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide",
                        "European Centre for Disease Prevention and Control"),
                 "and is retrieved periodically in order show the most updated data.", tags$br(),
                 tags$h4("Map-COVID 19 Tab"),
                 "This part of the app allows the user to visualize the total statistics of the pandemic by country.
                 There’s a select window where you are able to choose between total infections, total deaths 
                 and fatality rate. The quantities represent total numbers which means the sum of daily cases 
                 or deaths. The fatality rate is the computed by dividing the number of total deaths by the total
                 number of infections, all of this multiply by 100% factor. 
                 Next to it , there’s a date slider, where the user capable  to adjust the time window for the selected 
                 variable in last step. The slider includes a play button, if clicked the time window will automatically 
                 move until the actual date.
                 Bellow these options you will find two graphs containing the selected data. If the users drag 
                 their mouse over the map, the information of each of countries will be displayed. 
                 In the right-hand side, there’s a bar chart which makes easier to compare the total
                 quantities between the south American countries.",tags$br(),
                 tags$h4("General trend Tab"),
                 "The first tab of this section, “Daily cases”, allows to visualize day by day data. As in the last tab,
                 there is a select window where the user can choose between daily cases, daily deaths, and cumulative 
                 cases for 14 days per 100 000 inhabitants. Bellow, there is a date range input window that is used to 
                 specify the time window for analysis. The last option allows to choose between countries for graphical purposes.
                 Finally, a time series line graph is presented in the right, containing all the information selected 
                 from the last options.The second tab, total cases, allows the user to visualize the cumulative statistics by country. 
                 A date slider is included, as well as a select window to filter between countries. The main goal of the graph
                 is to identify how the cumulative statistics changes over time.", tags$br(),
                 tags$h4("Raw data Tab"),
                 "This tab presents the data used in last sections, allowing the user to filter/search data in an interactive way.
                 At the bottom, there is a “download button” in case the user wants to get the data for further analysis. ",tags$br(),
                 tags$h4("Author"),
                 "Name: Pablo Rueda",tags$br(),
                 "Contact email: pablo.rueda89@gmail.com",tags$br(),
                 tags$a(href="https://www.upwork.com/freelancers/~01fcee364fb92a13dd?viewMode=1","Up-work profile"),tags$br(),
                 tags$a(href="https://www.linkedin.com/in/pablo-rueda-4337b750/","Linkedin profile"),tags$br()
        
                 
                 
                 )
    
        
    )
))
                     
 
            
                     
                         
                             
                             
             
                     
                     
                 
                     
  
  
             
   