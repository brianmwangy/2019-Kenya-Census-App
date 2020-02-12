

library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(DT)
library(RColorBrewer)
library(readr)
library(reshape2)

#loading population trend file
trend<-read_csv("./www/trend.csv")

#loading county data
county_data<-read_csv("./www/Kenya_census.csv")
cnt<-county_data %>% melt(id.vars="County")
#loading county shapefile
county_shp<-readOGR("./www/shp/county.shp")




# Define UI 
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "2019 KENYA CENSUS STATISTICS.", titleWidth = 500,
    tags$li(actionLink("LinkedIn", 
                       label = "", 
                       icon = icon("linkedin"),
                       onclick = "window.open('https://www.linkedin.com/in/brianmwangi/')"),
            class = "dropdown"),
    tags$li(actionLink("GitHub", 
                       label = "", 
                       icon = icon("github"),
                       onclick = "window.open('https://github.com/brynmwangy')"),
            class = "dropdown")
  ),
  
  dashboardSidebar(
    
    sidebarUserPanel("Brian Mwangi",
                     subtitle = "Data Analyst"
                     ),
    sidebarMenu(
      menuItem(
        "Introduction",
        tabName = "intro",
        icon = icon("info")
      ),
      
      menuItem(
        "Map",
        tabName = "map",
        icon = icon("map")
      ),
      
      menuItem(
        "Data",
        tabName = "data",
        icon = icon("table")
      ),
      
      menuItem(
        "About me",
        tabName = "author",
        icon = icon("user")
      )
      
    )             
    
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML('
                              .main-header .logo {
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 20px;
                              }
                              '))),
    tabItems(
      
      tabItem(
        tabName = "intro",
        
        
        fluidRow(
          
           h2(HTML("<strong>Overview.</strong>")),
           h3(tags$p("The first known population census in Kenya was conducted in 1897 and was basically a headcount. 
                  This was followed by the 1948 census that focused on non-natives. 
                  A complete census that enumerated 8.6 million persons was conducted in 1962 and was used to set up 
                  political and administrative structures. The first post-independence census was undertaken in 1969 and enumerated 10.9 million persons. 
                  Since then, the country has conducted decennial Population and Housing Censuses on a de facto basis with the midnight of 24th/25th August as the reference point. 
                  The censuses have been implemented in accordance with the UN Principles and Recommendations for conducting population and housing censuses.")),
           h3(tags$p("The plot below shows the population trend from 1969 to 2019.")),
           plotlyOutput("lineplot",width = 1050),
           h2(HTML("<strong>Objective of the 2019 census.</strong>")),
           h3(tags$p("The main objective of the 2019 KPHC was to collect information on the size, composition, 
                     distribution and socio-economic characteristics of the population. 
                     This information will be used in planning, budgeting and programming for important services; 
                     future policy formulation, resource allocation; creation of administrative and political units; 
                     monitoring and evaluation of programmes and projects; research; development of a master household 
                     sampling frame; development of geo-spatial database; benchmark for agricultural census/surveys; 
                     business, industry and labour; and formulation of housing policy and programmes.")),
           h2(HTML("<strong>Theme of the 2019 Census.</strong>")),
           h3(tags$p("The theme of the census was “Counting Our People for Sustainable Development and Devolution of
                     Services”. This is in response to the demand for statistical information for implementation of
                     Kenya’s development agenda such as the Big Four and Vision 2030 and other global initiatives 
                     including the Sustainable Development Goals (SDGs)."))
          )
        #end fluidrow
        
          ), #end tabname
      
      tabItem(
        tabName = "map",
        
        fluidRow(

          valueBoxOutput(
             "total"

          ),#end box

          valueBoxOutput(
             "male"

          ), #end box

          valueBoxOutput(
             "female"

          ),
          
          valueBoxOutput(
             "hh"
            
          ),
          valueBoxOutput(
             "hhsize"
            
          ),
          
          valueBoxOutput(
            "pop"
            
          )

        ),
        
        fluidRow(
          column(width = 3,
                     selectInput(
                       inputId = "stats",
                       label = "Select Indicator",
                       choices =c(
                         "Total Population"=1,
                         "Population Density"=2,
                         "Male Population"=3,
                         "Female Population"=4,
                         "Intersex Population"=5,
                         "Number of Households"=6,
                         "Average Household Size"=7
                       ),selected = 1
                        
                     ) 
                 
                 
          ) #end column
          
          
        ),
        #end row
        fluidRow(
          column(
            width = 6,
            #box(
              # title = "MAP",
              # status = "primary",solidHeader = TRUE,
              # width = NULL,height = 600,collapsible = TRUE,
              leafletOutput("maps",height = 500)
            #)
          ),#end column
          
          column(width = 3,
                 plotlyOutput("top",height = 500)
                 ),
          
          column(width = 3,
                 plotlyOutput("bottom",height = 500)
          )
          
          
          
        )#end row
        
        
        
      ), #end tab item
      
      tabItem(
        tabName = "data",
        
        div(
          h1(strong("Dataset.")),
          h3("The table below displays the total,male,female and intersex population, population density, 
              HH size, land area for each of the 47 counties in Kenya.
             ")
          ),
        dataTableOutput("table")
        ),
      tabItem(
        tabName = "author",
        fluidRow(
          br(),
          
          img(src ="profile.jpg", width = "17%", style = "display: block; margin-left: auto; margin-right: auto;")
          
        ),
        
        fluidRow(
          h3(strong("Brian Mwangi"), style = "text-align: center"),
          h4("brynmwangy@gmail.com", style = "text-align: center")
        ),
        
        hr(),
        fluidRow(column(5, ""),
                 column(
                   3,
                   tags$h3(
                     HTML('&nbsp;'),
                     HTML('&nbsp;'),
                     HTML('&nbsp;'),
                     tags$a(
                       href = 'https://www.linkedin.com/in/brianmwangi/',
                       img(
                         src = 'LinkedIn.png',
                         height = "50px"
                       )
                     ),
                     HTML('&nbsp;'),
                     tags$a(href = 'https://github.com/brynmwangy', img(
                       src = 'github.jpg',
                       height = "50px"
                     ))
                   )
                 )),
        
        
        fluidRow(
          column(2, ""),
          column(
            1,
            h3(icon("briefcase"), style = "text-align: right; line-height: 165%;"),
            br(),
            br(),
            h3(icon("globe"), style = "text-align: right; line-height: 200%"),
            br(),
            h3(icon("heart"), style = "text-align: right; line-height: 170%;")
          ),
          column(
            6,
            h4(
              "Currently I work as a Research Assistant at Aquaya but I am 
              open to new and challenging opportunities in the data science sector.",
              style = "text-align: left; line-height: 150%;"
            ),
            br(),
            h4(
              "Data is the new electricity that is going to light up all the other industries.
              I turn data into insights for informed decision making and believe that the future belongs to people 
              and companies that turn data into products.Obsessed with creating beautiful, neat, and informative data visualizations.",
              style = "text-align: left; line-height: 150%;"
            ),
            br(),
            h4(
              "Passionate about good music. I love adventure;travelling, experiencing different cultures, and meeting new people! ",
              style = "text-align: left; line-height: 150%;"
            )
            
          ),
        
          column(3, "")
        
        
      
      )#end row
      
    )
          )#end tab items
    
    )
  
  
    )

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  
  
  output$lineplot<-renderPlotly({
    ggplotly(
      ggplot(data = trend,aes(x=Year,y=Population))+
        geom_line(color="red")+
        labs(
          title = "Population trends from 1969 to 2019.",
          x="Year",
          y="Population in millions."
        )+
        scale_x_continuous(breaks = c(1969,1979,1989,1999,2009,2019
                                      )) +
      geom_text(aes(label =Population))+
      theme_minimal()  
      
    )
  })
  
  
  output$total<-renderValueBox({
    valueBox(
      "47,564,300", "TOTAL POPULATION", icon = icon("users"),
      color = "red"
    )
  })
  
  output$male<-renderValueBox({
    valueBox(
      "23,548,100", "MALE POPULATION", icon = icon("male"),
      color = "blue"
    )
  })
  
  output$female<-renderValueBox({
    valueBox(
      "24,014,700", "FEMALE POPULATION", icon = icon("female"),
      color = "yellow"
    )
  })
  
  output$hh<-renderValueBox({
    valueBox(
      "12,143,900", "NUMBER OF HHs", icon = icon("home"),
      color = "green"
    )
  })
  
  output$hhsize<-renderValueBox({
    valueBox(
      "3.9", "AVERAGE HH SIZE", icon = icon("store"),
      color = "orange"
    )
  })
  
  output$pop<-renderValueBox({
    valueBox(
      "82", "POPULATION DENSITY", icon = icon("users"),
      color = "blue"
    )
  })
  
  
  
  
  #rendering the basemap
  output$maps<-renderLeaflet(
    leaflet(county_shp) %>%
      setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
      addPolygons(
        color = ~palpop(Popultn),
        smoothFactor = 0.5,
        weight = 2, opacity = 1.0,
        fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "brown",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = paste(
          "<strong>County:</strong>",county_shp$ADM1_EN,
          "<br>",
          "<strong>Total Population:</strong>",county_shp$Popultn
          
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                  padding = "3px 8px"), 
                                     textsize = "13px", direction = "auto"),
        
        popup = ~paste(
          "<strong>County:</strong>",ADM1_EN,
          "<br>",
          "<strong>Total Population:</strong>",Popultn
          
        )
        
      ) %>%
      addLegend(title = "Total Population",
                pal = palpop, values = county_shp$Popultn, opacity = 1)
        
      )

  
  
 #color functions
  #population legend
  palpop<-colorBin("YlOrBr", county_shp$Popultn)
  
  #population density
  pal2<-colorBin("YlOrBr", county_shp$Ppltn_D)
  
  #male population
  pal3<-colorBin("YlOrBr", county_shp$Ml_Pplt)
  
  #female population
  pal4<-colorBin("YlOrBr", county_shp$Fml_Ppl)
  
  #intersex population
  pal5<-colorBin("YlOrBr", county_shp$Intrsx_)
  
  #Number of HHs
  pal6<-colorBin("YlOrBr", county_shp$Nmbr__H)
  
  #HH size
  pal7<-colorBin("YlOrBr", county_shp$Avrg_H_)
  
  
  observe({
    proxy<-leafletProxy("maps") %>% clearControls()
    if ("1" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color = ~palpop(Popultn),
          smoothFactor = 0.5,
          weight = 2, opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$ADM1_EN,
            "<br>",
            "<strong>Total Population:</strong>",county_shp$Popultn
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Total Population:</strong>",Popultn
            
          )
          
        ) %>%
        addLegend(title = "Total Population",
                  pal = palpop, values = county_shp$Popultn, opacity = 1)
    }

   else if ("2" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~pal2(Ppltn_D),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$ADM1_EN,
            "<br>",
            "<strong>Population Density(Per KM2):</strong>",county_shp$Ppltn_D

          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Population Density(per KM2):</strong>",Ppltn_D

          )

        ) %>%
        addLegend(title = "Population Density(per KM2)",
                  pal = pal2, values = county_shp$Ppltn_D, opacity = 1)
    }
    
    
   else if ("3" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~pal3(Ml_Pplt),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$ADM1_EN,
            "<br>",
            "<strong>Male Population:</strong>",county_shp$Ml_Pplt
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Male Population:</strong>",Ml_Pplt
            
          )
          
        ) %>%
        addLegend(title = "Male Population",
                  pal = pal3, values = county_shp$Ml_Pplt, opacity = 1)
    }
    
    
  else if ("4" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~pal4(Fml_Ppl),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$ADM1_EN,
            "<br>",
            "<strong>Female Population:</strong>",county_shp$Fml_Ppl
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Female Population:</strong>",Fml_Ppl
            
          )
          
        ) %>%
        addLegend(title = "Female Population",
                  pal = pal4, values = county_shp$Fml_Ppl, opacity = 1)
    }
    
    
  else  if ("5" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~pal5(Intrsx_),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$ADM1_EN,
            "<br>",
            "<strong>Intersex Population:</strong>",county_shp$Intrsx_
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Intersex Population:</strong>",Intrsx_
            
          )
          
        ) %>%
        addLegend(title = "Intersex Population",
                  pal = pal5, values = county_shp$Intrsx_, opacity = 1)
    }
    
    
   else if ("6" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~pal6(Nmbr__H),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$ADM1_EN,
            "<br>",
            "<strong>Number of Households:</strong>",county_shp$Nmbr__H
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Number of Households:</strong>",Nmbr__H
            
          )
          
        ) %>%
        addLegend(title = "Number of Households",
                  pal = pal6, values = county_shp$Nmbr__H, opacity = 1)
    }
    

    
   else if ("7" %in% input$stats){
      proxy %>%
        addPolygons(
          data = county_shp,
          color =  ~pal7(Avrg_H_),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp$ADM1_EN,
            "<br>",
            "<strong>Average household size:</strong>",county_shp$Avrg_H_
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",ADM1_EN,
            "<br>",
            "<strong>Average household size:</strong>",Avrg_H_
            
          )
          
        ) %>%
        addLegend(title = "Average household size",
                  pal = pal7, values = county_shp$Avrg_H_, opacity = 1)
    }
    
  })
  
  
  
  
  output$top<-renderPlotly({
    if("1" %in% input$stats){
      county_data %>% select(County,Population) %>%
        arrange(desc(Population)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Population),y=Population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Population))+
        labs(
          title = "Top 5 counties",
          y="Total Population",
          x="County"
        )+
        coord_flip()
  
    }
    
   else if("2" %in% input$stats){
      county_data %>% select(County,Population_Density) %>%
        arrange(desc(Population_Density)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Population_Density),y=Population_Density))+
        geom_col(fill="#FF6666")+
       geom_text(aes(label=Population_Density))+
        labs(
          title = "Top 5 counties",
          y="Population Density",
          x="County"
        )+
        coord_flip()
   }
    
    else if("3" %in% input$stats){
      county_data %>% select(County,Male_Population) %>%
        arrange(desc(Male_Population)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Male_Population),y=Male_Population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Male_Population))+
        labs(
          title = "Top 5 counties",
          y="Male Population",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("4" %in% input$stats){
      county_data %>% select(County,Female_Population) %>%
        arrange(desc(Female_Population)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Female_Population),y=Female_Population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Female_Population))+
        labs(
          title = "Top 5 counties",
          y="Female Population",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("5" %in% input$stats){
      county_data %>% select(County,Intersex_population) %>%
        arrange(desc(Intersex_population)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Intersex_population),y=Intersex_population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Intersex_population))+
        labs(
          title = "Top 5 counties",
          y="Intersex population",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("6" %in% input$stats){
      county_data %>% select(County,Number_of_Households) %>%
        arrange(desc(Number_of_Households)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Number_of_Households),y=Number_of_Households))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Number_of_Households))+
        labs(
          title = "Top 5 counties",
          y="Number of Households",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("7" %in% input$stats){
      county_data %>% select(County,Average_Household_size) %>%
        arrange(desc(Average_Household_size)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Average_Household_size),y=Average_Household_size))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Average_Household_size))+
        labs(
          title = "Top 5 counties",
          y="Average Household size",
          x="County"
        )+
        coord_flip()
      
    }  
    
   
  })
  
  
  output$bottom<-renderPlotly({
    if("1" %in% input$stats){
      county_data %>% select(County,Population) %>%
        arrange(desc(Population)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Population),y=Population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Population))+
        labs(
          title = "Bottom 5 counties",
          y="Total Population",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("2" %in% input$stats){
      county_data %>% select(County,Population_Density) %>%
        arrange(desc(Population_Density)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Population_Density),y=Population_Density))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Population_Density))+
        labs(
          title = "Bottom 5 counties",
          y="Population Density",
          x="County"
        )+
        coord_flip()
    }
    
    else if("3" %in% input$stats){
      county_data %>% select(County,Male_Population) %>%
        arrange(desc(Male_Population)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Male_Population),y=Male_Population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Male_Population))+
        labs(
          title = "Bottom 5 counties",
          y="Male Population",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("4" %in% input$stats){
      county_data %>% select(County,Female_Population) %>%
        arrange(desc(Female_Population)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Female_Population),y=Female_Population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Female_Population))+
        labs(
          title = "Bottom 5 counties",
          y="Female Population",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("5" %in% input$stats){
      county_data %>% select(County,Intersex_population) %>%
        arrange(desc(Intersex_population)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Intersex_population),y=Intersex_population))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Intersex_population))+
        labs(
          title = "Bottom 5 counties",
          y="Intersex population",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("6" %in% input$stats){
      county_data %>% select(County,Number_of_Households) %>%
        arrange(desc(Number_of_Households)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Number_of_Households),y=Number_of_Households))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Number_of_Households))+
        labs(
          title = "Bottom 5 counties",
          y="Number of Households",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("7" %in% input$stats){
      county_data %>% select(County,Average_Household_size) %>%
        arrange(desc(Average_Household_size)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Average_Household_size),y=Average_Household_size))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Average_Household_size))+
        labs(
          title = "Bottom 5 counties",
          y="Average Household size",
          x="County"
        )+
        coord_flip()
      
    }  
    
  })
  

  
  output$table<-renderDataTable({
   datatable( county_data,
    class = 'cell-border stripe',
    editable = TRUE,
    options = list(scrollX = T)
   ) 
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

