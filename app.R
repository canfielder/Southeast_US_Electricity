#Library Calls###################################
{
  if (!require(tidyverse)) {install.packages('tidyverse')}
  library(tidyverse)
  
  if (!require(shiny)) {install.packages('shiny')}
  library(shiny)
  
  if (!require(reactlog)) {install.packages('shiny')}
  library(reactlog)
  
  if (!require(leaflet)) {install.packages('leaflet')}
  library(leaflet)
  
  if (!require(memoise)) {install.packages('memoise')}
  library(memoise)
  
  if (!require(shinythemes)) {install.packages('shinythemes')}
  library(shinythemes)
  
  if (!require(highcharter)) {install.packages('highcharter')}
  library(highcharter)
  
  if (!require(markdown)) {install.packages('markdown')}
  library(markdown)
}

#Data Input######################################
{
  #Energy Dataset
  EIA_923_SE <- read.csv("./data/EIA_923_860M_PROCESSED_Southeast.csv")
  EIA_923_SE <- EIA_923_SE %>% 
    select(-X)

  #Color Palette Definition
  color_palette <- read.csv("./data/color_palette.csv", stringsAsFactors = FALSE)
  
  #State Latitude, Longitude, and Zoom Level Information
  state_info <- read.csv("./data/state_info.csv")
  }

southeast_states <- c("Southeast United States" = "SEUS",
                      "Alabama" = "AL", 
                      "Arkansas" = "AR",
                      "Florida" = "FL", 
                      "Georgia" = "GA",
                      "Kentucky" = "KY",
                      "Louisiana" = "LA",
                      "Mississippi" = "MS", 
                      "North Carolina" = "NC",
                      "South Carolina" = "SC", 
                      "Tennessee" = "TN",
                      "Virginia" = "VA")

#Energy Type Factor Levels

energy_level <- c("Wind", "Solar", "Other", "Oil", "Biomass",
                  "Hydroelectric", "Natural Gas", "Coal", "Nuclear")


#Color pallete re-definition to match stacked bar order plots
#Based on energy type factor levels

energy_level.df <- data.frame(energy_level)

energy_level.df$energy_level <- as.character(energy_level.df$energy_level)

color_palette_ordered <- energy_level.df %>% 
  left_join(y = color_palette, c("energy_level" = "project_energy_type" ))

colors_total <- as.list(color_palette_ordered$color_code)

color_palette_renew <- color_palette %>% 
  filter(project_energy_code %in% c("BIO", "HYD", "SOL", "WND"))

colors_renew <- c("#C0392B", "#196F3D")

colors_ff <- c("#EE1C25", "#F78B29", "#99979A")

#Dataframe Development For Generated Plots#######
#The following dataframes are generated outside of the UI or server
#and are used in one or more plots in the app. As much data processing
#as possible is performed before the dataframe enters the server.

###NET GENERATION###
#By State
net_gen_type_state <- EIA_923_SE %>% 
  #Convert project energy type to character
  mutate(project_energy_type = as.character(project_energy_type)) %>% 
  group_by(Year, State_SF, project_energy_type) %>% 
  summarise(total_gen = sum(net_gen_mw)) %>% 
  ungroup %>% 
  
  #join color palette data by project energy type
  full_join(y = color_palette, by = "project_energy_type") %>% 
  select(-c(project_energy_code:color_name)) %>% 
  
  #spread data so each project energy type is a seperate column
  spread(key = project_energy_type, value = total_gen) %>% 
  filter(!is.na(Year)) %>% 
  replace(is.na(.),0) %>% 
  #re-gather now that zeros have been substituted
  gather(key = project_energy_type, value = total_gen, Biomass:Wind)

net_gen_type_state$project_energy_type <- factor(net_gen_type_state$project_energy_type, ordered = TRUE, 
                                                    levels = energy_level)

#Net Generation By Type, All States
net_gen_type_seus <- net_gen_type_state %>% 
  group_by(Year, project_energy_type) %>% 
  summarise(total_gen = sum(total_gen)) %>% 
  ungroup

###RENEWABLES###
#By State
renewable_state <- EIA_923_SE %>% 
  group_by(Year, State_SF, renewable) %>% 
  summarise(total_gen = sum(net_gen_mw)) %>% 
  ungroup %>% 
  spread(key = renewable, value = total_gen) %>% 
  filter(!is.na(Year)) %>% 
  replace(is.na(.),0) %>% 
  gather(key = renewable, value = total_gen, N:Y) %>% 
  mutate(renewable = recode(renewable, Y = "Renewable"),
         renewable = recode(renewable, N = "Non-Renewable"))

#All States
renewable_seus <- renewable_state %>% 
  group_by(Year, renewable) %>% 
  summarise(total_gen = sum(total_gen)) %>% 
  ungroup 


###EMISSIONS###
#Total Generation By State - For Emissions Dataframe Join
net_get_total_state <- net_gen_type_state %>% 
  group_by(Year, State_SF) %>% 
  summarise(total_gen = sum(total_gen)) %>% 
  ungroup

emission_by_energy_type_state <- EIA_923_SE %>% 
  
  #Convert project energy type to character
  mutate(project_energy_type = as.character(project_energy_type)) %>% 
  group_by(Year, State_SF, project_energy_type) %>% 
  summarise(total_emis = sum(emission_output)) %>% 
  ungroup %>%
  
  #join color palette data by project energy type
  full_join(y = color_palette, by = "project_energy_type") %>% 
  select(-c(project_energy_code:color_name)) %>% 
  
  #spread data so each project energy type is a seperate column
  spread(key = project_energy_type, value = total_emis) %>%  
  filter(!is.na(Year)) %>%  
  replace(is.na(.),0) %>% 
  gather(key = project_energy_type, value = total_emis, Biomass:Wind)

#For consistency with Stacked Bar and Net Generation By Type
emission_by_energy_type_state$project_energy_type <- factor(emission_by_energy_type_state$project_energy_type, ordered = TRUE, 
                                                 levels = energy_level)

#Emission By Type, All States
emission_by_energy_type_seus <- emission_by_energy_type_state %>% 
  group_by(Year, project_energy_type) %>% 
  summarise(total_emis = sum(total_emis))

#Emission Total, By State
emission_total_state <- emission_by_energy_type_state %>% 
  group_by(Year, State_SF) %>% 
  summarise(total_emis = sum(total_emis)) %>% 
  left_join(y = net_get_total_state, by = c("Year", "State_SF")) %>% 
  mutate(emis_per_mw = total_emis / total_gen) %>% 
  replace(is.na(.),0)

#Emission Total, All States
emission_total_seus <- emission_total_state %>% 
  group_by(Year) %>% 
  summarise(total_emis = sum(total_emis),
            total_gen = sum(total_gen)) %>% 
  mutate(emis_per_mw = total_emis / total_gen) %>% 
  replace(is.na(.),0)

#Functions#######################################
#Color Assignment
pal <- colorFactor(palette = color_palette$color_code, 
                   levels  = color_palette$project_energy_type)

pal_lookup <- memoise(function(energy_code) {
  
  color_palette[match(x = energy_code, table = color_palette$project_energy_code),3]
  
})

#Filtering by Year, Selected by Slider, State if Selected via Dropdown
EIA_923_SE_Filtered <- memoise(function(year, state) {
  
  if (state == "SEUS"){
    EIA_923_SE %>% 
      filter(Year == year)
  } else {
    EIA_923_SE %>% 
      filter(Year == year, State_SF == state)
  }
})

#Net Generation By Type
gen_by_type <- memoise(function(year, state) {
  
  df <- if (state == "SEUS"){
    net_gen_type_seus %>% 
      filter(Year %in% (2001:year))
    
  } else {
    net_gen_type_state %>%
      filter(Year %in% (2001:year), State_SF == state)
  }
  
  df
  
})

#Renewables
renewables <- memoise(function(year, state) {
  
  df <- if (state == "SEUS"){
    renewable_seus %>% 
      filter(Year %in% (2001:year))
    
  } else {
    renewable_state %>%
      filter(Year %in% (2001:year), State_SF == state)
  }
  
  df
  
})

#Emissions - Total
emis_total <- memoise(function(year, state) {
  
  df <- if (state == "SEUS"){
    emission_total_seus %>% 
      filter(Year %in% (2001:year))
    
  } else {
    emission_total_state %>%
      filter(Year %in% (2001:year), State_SF == state)
  }
  
  df
  
})

#Emissions - By Type
emis_by_type <- memoise(function(year, state) {
  
  df <- if (state == "SEUS"){
    emission_by_energy_type_seus %>% 
      filter(Year %in% (2001:year))
    
  } else {
    emission_by_energy_type_state %>%
      filter(Year %in% (2001:year), State_SF == state)
  }
  
  df
  
})

#Input Settings to be used in determining map zoom level
map_settings <- memoise(function(state) {
  if (state == ""){
    state_info %>% 
      filter(state.abb == "SEUS") 
  } else {
    state_info %>% 
      filter(state.abb == state)
  }
})

#Base Leaflet Map
map <- leaflet('map',
               options = leafletOptions(minZoom = 5, preferCanvas = TRUE),
               sizingPolicy = leafletSizingPolicy(padding = "10px")) %>% 
  addTiles() %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas, 
                   options =  providerTileOptions(updateWhenZooming = FALSE,
                                                  updateWhenIdle = TRUE )) %>% 
  setView(lng = -83.977937, 
          lat = 32.190283, 
          zoom = 5) %>% 
  setMaxBounds(lng1 = -105, 
               lat1 = 40, 
               lng2 = -75,
               lat2 = 24) %>% 
  addLegend(position = "bottomright", 
            colors = color_palette$color_code, 
            labels = color_palette$project_energy_type, 
            opacity = 0.75) 

#Shiny App##############################################
ui <- fluidPage( theme = shinytheme("spacelab"),
   
   # Application title
      fluidRow(column(width = 10,
              titlePanel(title = "Electricity Generation in the Southeast United States: 2001 - 2017", 
              windowTitle = "Energy Generation in SE US")),
              column(width = 2,
                     br(),
              selectInput(inputId = "state", 
                          label =  "Select State:", 
                          choices = southeast_states)
              )),
              sliderInput(inputId = "year", 
                          label = "Select Year or Push Play Button", 
                          min = 2001,
                          max = 2017,
                          value = 2017,
                          width = "100%", 
                          sep = "", 
                          ticks = FALSE,
                          animate = animationOptions(interval = 750, loop = FALSE)
              ),
        fluidRow(column(width = 8,
                        leafletOutput(outputId = "map", height = "700px")
                        ),
                 column(width = 4,
                        tabsetPanel(
                          tabPanel("About",
                                   br(),
                                   includeMarkdown(path = "./data/About_This_App.Rmd")
                                   ),
                          tabPanel("Total",
                                   highchartOutput(outputId = "net_gen_stacked_bar", height = "675px")
                          ),
                           tabPanel("By Energy Source",
                                    highchartOutput(outputId = "net_gen_type_total", height = "675px")
                                    ),
                          tabPanel("Renewables",
                                   highchartOutput(outputId = "renew", height = "675px")
                          ),
                          tabPanel("Emissions",
                                   #highchartOutput(outputId = "emissions", height = "150px"),
                                   #br(),
                                   highchartOutput(outputId = "emissions_per_mw", height = "325px"),
                                   br(),
                                   highchartOutput(outputId = "emissions_ff", height = "325px")
                                  )
                        )
        )
  )
)



server <- function(input, output) {
  
  #Reactive Values
  ##For Leaflet Plot
  df_filtered_year <- reactive({
    EIA_923_SE_Filtered(year = input$year, state = input$state)
  })
  
  ##Net Generation By Type
  df_gen_by_type <- reactive({
    gen_by_type(year = input$year, state = input$state)
  }) 
  
  ##Renewable Generation
  df_renewables <- reactive({
    renewables(year = input$year, state = input$state)
  }) 

  ##Emission Generation Total
  df_emis_total<- reactive({
    emis_total(year = input$year, state = input$state)
  }) 
  
  ##Emission Generation By Type
  df_emis_by_type <- reactive({
    emis_by_type(year = input$year, state = input$state)
  }) 
  
  #Leafet Layers
  ##Add Power Plant Markers on Default Leaflet Map
  observe({
    #Import Reactive Dataframe, filtered via slider selected year
    EIA_923_SE <- df_filtered_year()
    
    #Import Zoom level on Leaflet Map
    zoom <- input$map_zoom
    
    #Apply Factor to Zoom level to aid map legibility
    zoom_factor <- zoom/200
    radius <- (EIA_923_SE$Effective_Rate * zoom_factor)^0.5
    
    #Update Leaflet Map - Circle Markers
    leafletProxy(mapId = 'map',
                 data = EIA_923_SE) %>% 
      clearMarkers() %>%
      addCircleMarkers(lng = ~Longitude,
                       lat = ~Latitude,  
                       color = ~pal(project_energy_type), 
                       fill = ~pal(project_energy_type),
                       fillOpacity = 0.85,
                       radius = ~radius,
                       label =  ~paste(Plant.Name," - ", Effective_Rate, "MW"), 
                       labelOptions = labelOptions(direction = "bottom",
                                                  style = list(
                                                    "font-family" = "sans-serif",
                                                    "font-style" = "bold",
                                                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                    "font-size" = "13px",
                                                    "border-color" = "rgba(0,0,0,0.5)")
                                                  )
                   )
  })
  
  observeEvent(input$state, {
  #Latitude, Longitude, Zoom Data
  location <- map_settings(state = input$state)
  
    #Update Location Only on State Selection
    leafletProxy(mapId = 'map') %>% 
      setView(lng = location$lon.central, 
              lat = location$lat.central, 
              zoom = location$zoom)
  })
  
  #Outputs
  ##Leaflet Map Update
  output$map <- renderLeaflet(map)
  
  
  ##Net Generation By Type - Line Graph
  output$net_gen_type_total <- renderHighchart({
    
    df <- df_gen_by_type()
    
    hchart(df, 
           "line",
           hcaes(x = Year, y = total_gen, group = project_energy_type)) %>%
      hc_plotOptions(series = list(animation = FALSE)) %>%  
      hc_xAxis(title = list(text="Year"), 
               max = 2017) %>% 
      hc_yAxis(title = list(text="Net Generation (MW)")) %>% 
      hc_colors(colors_total) %>% 
      hc_title(text = "Net Generation By Energy Source") 
      
  })
  
  ##Net Generation By Type - Stacked Bar
  output$net_gen_stacked_bar <- renderHighchart({
    
    df <- df_gen_by_type()
    
    hchart(df, 
           "column",
           hcaes(x = Year, y = total_gen, group = project_energy_type)) %>%
      hc_plotOptions(series = list(animation = FALSE)) %>%  
      hc_xAxis(title = list(text="Year"), 
               max = 2017) %>% 
      hc_yAxis(title = list(text="Net Generation (MW)")) %>% 
      hc_colors(colors_total) %>% 
      hc_title(text = "Net Generation By Energy Source") %>% 
      hc_plotOptions(column=list(stacking='normal'))
  })
  
  ##Renewable v Non-Renewable - Line Graph
  output$renew <- renderHighchart({
    
    df <- df_renewables()
    
    hchart(df, 
           "line",
           hcaes(x = Year, y = total_gen, group = renewable)) %>%
      hc_plotOptions(series = list(animation = FALSE)) %>%  
      hc_xAxis(title = list(text="Year"), 
               max = 2017) %>% 
      hc_yAxis(title = list(text="Net Generation (MW)"),
               min = 0) %>%
      hc_colors(colors_renew) %>% 
      hc_title(text = "Renewable vs. Non-Renewable Net Generation") 
    
  })
  
  ##Total Emssions - Line Graph
  output$emissions <- renderHighchart({
    
    df <- df_emis_total()
    
    hchart(df, 
           "line",
           hcaes(x = Year, y = total_emis)) %>%
      hc_plotOptions(series = list(animation = FALSE)) %>%  
      hc_xAxis(title = list(text="Year"), 
               max = 2017) %>% 
      hc_yAxis(title = list(text="gCO2eq"),
               min = 0) %>% 
      hc_title(text = "Greenhouse Gas Emissions") 
    
  })
 
  ##Total Emssions Per MW - Line Graph
  output$emissions_per_mw <- renderHighchart({
    
    df <- df_emis_total()
    
    hchart(df, 
           "line",
           hcaes(x = Year, y = emis_per_mw)) %>%
      hc_plotOptions(series = list(animation = FALSE)) %>%  
      hc_xAxis(title = list(text="Year"), 
               max = 2017) %>% 
      hc_yAxis(title = list(text="gCO2eq / MW"),
               min = 0) %>% 
      hc_title(text = "Greenhouse Gas Emissions, Per MW") 
    
  }) 
  
  ##Total Emssions Fossil Fuels
  output$emissions_ff <- renderHighchart({
    
    df <- df_emis_by_type()
    
     df <- df %>% 
       filter(project_energy_type %in% c("Coal", "Natural Gas","Oil"))
    
    hchart(df, 
           "line",
           hcaes(x = Year, y = total_emis, group = project_energy_type)) %>%
      hc_plotOptions(series = list(animation = FALSE)) %>%  
      hc_xAxis(title = list(text="Year"), 
               max = 2017) %>%
      hc_colors(colors_ff) %>% 
      hc_yAxis(title = list(text="gCO2eq"),
               min = 0) %>% 
      hc_title(text = "Greenhouse Gas Emissions") %>% 
      hc_subtitle(text = "Fossil Fuels Only") 
    
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
