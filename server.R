

####Server: back-end

library(shiny)






shinyServer(function(input, output,session) {
  
  
    
   
     output$plot1<- renderPlotly({
         
         if(input$yvar=="Daily cases"){
             df_covid$y <-df_covid$cases
             title_cv<-c("Daily cases of COVID-19 in South American countries")
             xaxis_cv<-list(title="Date")
             yaxis_cv<-list(title="#cases/day")
                  
         }
         
         if(input$yvar=="Daily deaths"){
             df_covid$y <-df_covid$deaths
             title_cv<-c("Daily deaths of COVID-19 in South American countries")
             xaxis_cv<-list(title="Date")
             yaxis_cv<-list(title="#deaths/day")
             
            
         }
         
         if(input$yvar=="Cumulative cases"){
             df_covid$y <-df_covid$Cumulative_number_for_14_days_of_COVID.19_cases_per_100000
             title_cv<-c("Cumulative number for 14 days of Covid 19 cases per 100000 people")
             xaxis_cv<-list(title="Date")
             yaxis_cv<-list(title="#Cumulative cases")            
             
         }
         df_covid %>% 
             dplyr::filter(countriesAndTerritories %in% input$countries,
                           dateRep>=input$date_range[1] & dateRep<=input$date_range[2]) %>% 
             plot_ly(x=~dateRep,y=~y,color=~countriesAndTerritories, 
                    colors=mycolors, type="scatter",mode='lines') %>% 
             layout(title=title_cv,xaxis=xaxis_cv,yaxis=yaxis_cv)
             
        })
     
     output$plot2<-renderPlotly({
         total<-df_covid %>%
             filter(countriesAndTerritories %in% input$country,
                    dateRep>=input$date_range2[1] & dateRep<=input$date_range2[2]) %>%
             arrange(dateRep) %>% 
             mutate("total_cases"=cumsum(cases),"total_deaths"=cumsum(deaths)) %>% 
             select(dateRep,total_cases,total_deaths) %>% 
             gather(key="variable",value="count",c(-dateRep))
         
         g<-ggplot(total, aes(x=dateRep, y=count, fill=variable )) + 
             geom_area(stat="identity", position="stack")+
             scale_fill_manual(values=c("darkred", "blue2"))+
             labs(x="Date")+
             theme_minimal()
         ggplotly(g)
             
     
             
         
         
     })
     
     output$infections<-renderText({
         
         total<-df_covid %>%
             filter(countriesAndTerritories %in% input$country,
                    dateRep>=input$date_range2[1] & dateRep<=input$date_range2[2]) %>%
             arrange(dateRep) 
         
         sum(total$cases)
             
         
        
         
     })
     
     output$countrydeaths<-renderText({
         
         total<-df_covid %>%
             filter(countriesAndTerritories %in% input$country,
                    dateRep>=input$date_range2[1] & dateRep<=input$date_range2[2]) %>%
             arrange(dateRep) 
         
         sum(total$deaths)
         
         
         
         
     })
     
     output$RawData<-DT::renderDataTable(
         DT::datatable({
             df_covid
             
         },
         options=list(lengthMenu=list(c(7,15,-1),c("5","15","All")),pageLength=15),
         filter='top',
         selection='multiple',
         style='bootstrap'
         
         
         ))
     
     output$download<-downloadHandler(
         filename = "sa_covid.csv",
         content=function(file){
             write.csv(df_covid,file)
         }
         
     )
     
     output$map1<-renderLeaflet({
       
       
       
       
       map_total<-df_covid %>%
         filter(dateRep>=input$date_range3[1] & dateRep<=input$date_range3[2]) %>%
         arrange(dateRep) %>% 
         group_by(countriesAndTerritories) %>% 
         summarize(total_deaths=sum(deaths),total_cases=sum(cases)) %>% 
         mutate(fatality=round((total_deaths/total_cases*100),2))
       
       colnames(map_total)[1]<-"name"
       
       
       countries_merged <- merge(sa_sp,map_total,by="name")
       countries_merged<-st_as_sf(countries_merged) 
       
       
  
       
       labels1 <- sprintf(
         "<strong>%s</strong><br/>%g reported infections",
         countries_merged$name, countries_merged$total_cases
       ) %>% lapply(htmltools::HTML)
       
       labels2<- sprintf(
         "<strong>%s</strong><br/>%g reported deaths",
         countries_merged$name, countries_merged$total_deaths
       ) %>% lapply(htmltools::HTML)
       
       
       labels3 <- sprintf(
         "<strong>%s</strong><br/>%g &#37",
         countries_merged$name, countries_merged$fatality
       ) %>% lapply(htmltools::HTML)
       
       if(input$cl_map=="Total infections"){
         
         leaflet(countries_merged) %>% 
           addProviderTiles("Esri.WorldImagery") %>%
           setView(lat = -18.94,lng = -64.77,zoom=3) %>% 
           
           
           addPolygons(data=countries_merged,
                       fillColor = ~pal1(log10(total_cases)),
                       color = "black",
                       dashArray = "2",
                       fillOpacity = 0.7,
                       weight = 0.7,
                       smoothFactor = 0.2,
                       highlight= highlightOptions(
                         weight=5,
                         color="#666",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = labels1,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto")) %>%
           
           
           addLegend(pal=pal1,
                     values=~log10(total_cases),
                     position="bottomright",
                     title="Total infections ",
                     labFormat = labelFormat(transform = function(x) round(10^x))) 
         
         
       }
       
       else if(input$cl_map=="Total deaths"){
        
         
         leaflet(countries_merged) %>% 
           addProviderTiles("Stamen.Watercolor") %>% 
           setView(lat = -18.94,lng = -64.77,zoom=3) %>% 
           addPolygons(data=countries_merged,
                       fillColor = ~pal2(log10(total_deaths)),
                       color = "white",
                       dashArray = "2",
                       fillOpacity = 0.7,
                       weight = 0.7,
                       smoothFactor = 0.2,
                       highlight= highlightOptions(
                         weight=5,
                         color="#666",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = labels2,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto")) %>% 
           
           addLegend(pal=pal2,
                     values=~log10(total_deaths),
                     position="bottomright",
                     title="Total deaths",
                     labFormat = labelFormat(transform = function(x) round(10^x))) 
         
         
         
       }
       else if(input$cl_map=="Fatality rate (%)"){
         
         leaflet(countries_merged) %>% 
           addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>% 
           setView(lat = -18.94,lng = -64.77,zoom=3) %>% 
           
           
           addPolygons(data=countries_merged,
                       fillColor = ~pal3(fatality*1000),
                       color = "black",
                       dashArray = "2",
                       fillOpacity = 0.9,
                       weight = 0.7,
                       smoothFactor = 0.2,
                       highlight= highlightOptions(
                         weight=5,
                         color="#666",
                         fillOpacity = 0.7,
                         bringToFront = TRUE),
                       label = labels3,
                       labelOptions = labelOptions(
                         style = list("font-weight" = "normal", padding = "3px 8px"),
                         textsize = "15px",
                         direction = "auto")) %>%
           
           
           addLegend(pal=pal3,
                     values=~(1000*fatality),
                     position="bottomright",
                     title="Fatality rate (%)",
                     labFormat = labelFormat(transform = function(x) round((x/1000),2)))
         
         
         
       }
       
       
       
     })
     
     output$plot3<- renderPlotly({
       
       
       map_total<-df_covid %>%
         filter(dateRep>=input$date_range3[1] & dateRep<=input$date_range3[2]) %>%
         arrange(dateRep) %>% 
         group_by(countriesAndTerritories) %>% 
         summarize(total_deaths=sum(deaths),total_cases=sum(cases)) %>% 
         mutate(fatality=round((total_deaths/total_cases*100),2))
       
       colnames(map_total)[1]<-"name"
       
       pal1<- colorNumeric("viridis", NULL)
       pal2<-colorNumeric("YlOrRd",NULL)
       pal3<-colorNumeric("YlGnBu",NULL)
       
       if(input$cl_map=="Total infections"){
         infections<-map_total %>% arrange(total_cases)
         bc1<-ggplot(infections,aes(x=reorder(name,total_cases),y=total_cases,fill=as.factor(total_cases)))+
           geom_bar(stat="identity",position = "stack",show.legend = FALSE)+
           coord_flip()+
           scale_fill_manual(values=pal1(log10(infections$total_cases)))+
           xlab("")+
           ylab("Reported infections")+
           theme_bw()
         
         bc1<-ggplotly(bc1)  
         hide_legend(bc1)
         
         
         
       }
       else if(input$cl_map=="Total deaths"){
         
         
         deaths<-map_total %>% arrange(total_deaths)
         
         
         bc2<-ggplot(deaths,aes(x=reorder(name,total_deaths),y=total_deaths,fill=as.factor(total_deaths)))+
           geom_bar(stat="identity",position = "stack",show.legend = FALSE)+
           coord_flip()+
           scale_fill_manual(values=pal2(log10(deaths$total_deaths)))+
           xlab("")+
           ylab("Reported deaths")+
           theme_bw()
         
         bc1<-ggplotly(bc2)
         hide_legend(bc2)
       }
       
       
       else if(input$cl_map=="Fatality rate (%)"){
         
         
         fatality_t<-map_total %>% arrange(fatality)
         
         bc3<-ggplot(fatality_t,aes(x=reorder(name,fatality),y=fatality,fill=as.factor(fatality)))+
           geom_bar(stat="identity",position = "stack",show.legend = FALSE)+
           coord_flip()+
           scale_fill_manual(values=pal3((fatality_t$fatality)*1000))+
           xlab("")+
           ylab("Fatality rate(%)")+
           theme_bw()
         
         bc3<-ggplotly(bc3)
         hide_legend(bc3)
       }
       
       
      
       
     })
     
     
     }) 
    
    
  
   
        

       
    


