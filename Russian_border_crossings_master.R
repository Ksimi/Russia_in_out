library(tidyverse)
library(sf)
library(lubridate)
library(shiny)
library(leaflet)
library(leaflet.minicharts)
library(geosphere) # For charting the big circles
library(shinydashboard)
library(shinythemes)
library(shinybusy)
library(shinyWidgets) # For using selectizeInput
library(dashboardthemes)
library(sparkline)
library(DT)

#### Common elements begin ####

# Reading the World polygons and ISO-3166 values
World <- read_sf(
  "data-raw/World_Adj.shp"
  ) %>% 
  # "data-raw/ne_50m_admin_0_countries.shp"
  # ) %>%
  select("ISO_A3", "geometry")

# Subsetting Russia
Russia <- World %>% 
  filter(ISO_A3 == "RUS")

# Reading the RUB/EUR FX rates
# https://excelrates.com/historical-exchange-rates/EUR-RUB
Euro <- read_csv("data-raw/RUB_EUR.csv",
                 col_types = cols(Date = col_date("%m/%d/%y"))
)

Color_in <- "#0039A6" # Russian flag blue color
Color_out <- "#D52B1E" # Russian flag red color

# Reading inbound data file and pivoting wider
In_raw <- read_csv("data-raw/Russia_in_master.csv",
                   col_types = cols(Date = col_date("%m/%d/%Y"))) %>%
  select(-Type) %>%
  rename(Country = ENG) %>% 
  arrange_at(c("Date", "Trans")) %>% 
  pivot_wider(
    names_from = "Trans",
    values_from = "Value"
  ) %>% 
  mutate(
    Total = Car + Air + Rail + Ship + Walk
  ) %>% 
  select(-c("ISO_A2","M49")) %>% 
  filter(Country != "Russia") %>%
  mutate(Year = as.integer(year(Date))) %>%
  mutate(Month = as.character(month(Date, label = T, abbr = T))) %>%
  mutate(Color = Color_in)

Rus_in <- In_raw %>% 
  group_by(Date) %>% 
  summarise(Total_Rus = sum(Total))

# Reading inbound data file and pivoting wider
Out_raw <- read_csv("data-raw/Russia_out_master.csv",
                    col_types = cols(Date = col_date("%m/%d/%Y"))) %>%
  select(-Type) %>%
  rename(Country = ENG) %>% 
  arrange_at(c("Date", "Trans")) %>% 
  pivot_wider(
    names_from = "Trans",
    values_from = "Value"
  ) %>% 
  mutate(
    Total = Car + Air + Rail + Ship + Walk
  ) %>% 
  select(-c("ISO_A2","M49")) %>% 
  filter(Country != "Russia") %>%
  mutate(Year = as.integer(year(Date))) %>%#
  mutate(Month = as.character(month(Date, label = T, abbr = T))) %>% 
  mutate(Color = Color_out)

Rus_out <- Out_raw %>% 
  group_by(Date) %>% 
  summarise(Total_Rus = sum(Total))

# Creating Moscow coordinates
Mos_latlon <- tibble("lonmow" = 37.618423,
                     "latmow" = 55.751244)

#### Common elements end ####

#### Circles elements begin ####
In <- merge(In_raw, Rus_in, by = "Date") %>% 
  select(-c(Air,Car,Rail,Ship,Walk)) %>% 
  group_by(Date) %>%
  mutate(Sort = rank(-Total), 
         ties.method = "last")

Out <- merge(Out_raw, Rus_out, by = "Date") %>% 
  select(-c(Air,Car,Rail,Ship,Walk)) %>% 
  group_by(Date) %>%
  mutate(Sort = rank(-Total), 
         ties.method = "last")

Bal_raw <- merge(In_raw, Out_raw, by = c("ISO_A3", "Date")) %>%
  mutate(
    Air = Air.x - Air.y,
    Car = Car.x - Car.y,
    Rail = Rail.x - Rail.y,
    Ship = Ship.x - Ship.y,
    Walk = Walk.x - Walk.y,
    Total = Total.x - Total.y
  ) %>%
  select(
    "ISO_A3",
    "Date",
    "Country.x",
    "Air",
    "Car",
    "Rail",
    "Ship",
    "Walk",
    "Total",
    "Total.x",
    "Total.y"
  ) %>%
  rename(Country = Country.x,
         In = Total.x,
         Out = Total.y) %>%
  # filter(Country != "Russia") %>%
  mutate(Year = as.integer(year(Date))) %>%
  mutate(Month = as.character(month(Date, label = T, abbr = T))) %>%
  mutate(Color = ifelse(Total > 0, 
                        Color_in,
                        Color_out)
  ) %>%
  as_tibble()

Rus_bal <- Bal_raw %>% 
  group_by(Date) %>% 
  summarise(Total_Rus = sum(Total))

Bal <- merge(Bal_raw, Rus_bal, by = "Date") %>% 
  group_by(Date) %>% 
  mutate(Total_in = sum(In)) %>% 
  mutate(Total_out = sum(Out))

Bal_pos <- Bal %>%
    group_by(Date) %>%
    mutate(Sort = rank(-Total), 
           ties.method = "last")

Bal_neg <- Bal %>%
    group_by(Date) %>%
    mutate(Sort = rank(Total), 
           ties.method = "last")

Bal_full <- rbind(Bal_pos, Bal_neg)

# Reading Abkhazia & S.Ossetia population file
Pop_A_SO <- read_csv("data-raw/XAB_XOS_pop.csv")

# Reading World population file
Pop_WDI <-
  read_csv("data-raw/API_SP.POP.TOTL_DS2_en_csv_v2_1217749.csv", skip = 4) %>%
  select(-X65) %>%
  pivot_longer(
    cols = c(5:last_col()),
    names_to = "Year",
    values_to = "Pop",
    values_ptypes = c(integer, double)
  ) %>%
  rename("ISO_A3" = "Country Code",) %>%
  select("ISO_A3",
         "Year",
         "Pop") %>%
  rbind(Pop_A_SO) %>% # Adding Abkhazia & South Ossetia
  as_tibble()

Pop_WDI$Year <- as.integer(Pop_WDI$Year)

# Creating inbound  by population
In_by_pop <- In_raw %>%
  merge(Pop_WDI, by = c("ISO_A3", "Year")) %>%
  mutate(Total_by_pop = Total*100/Pop) %>%
  drop_na() %>% 
  mutate(Month = as.character(month(Date, label = T, abbr = T))) %>% 
  select(
    ISO_A3,
    Date,
    Month,
    Year,
    Country,
    Total,
    Total_by_pop,
    Pop
  )

In_by_pop_on_date <- In_by_pop %>% 
  group_by(Date) %>% 
  summarise(Pop_on_date = sum(Pop),
            Total_in_on_date = sum(Total)) %>% 
  mutate(Total_Rus = Total_in_on_date*100/Pop_on_date) %>% 
  merge(In_by_pop, by = "Date") %>% 
  mutate(Total = Total_by_pop,
         Color = Color_in) %>% 
  select(
    ISO_A3,
    Country,
    Date,
    Month,
    Year,
    Total,
    Total_Rus,
    Color
  ) %>% 
  group_by(Date) %>%
  mutate(Sort = rank(-Total, 
         ties.method = "last")
  )

Unique_dates <- c(unique(In_by_pop$Date)) %>% 
  sort()

Dates_new <- c(Date = seq(as.Date(min(Unique_dates)), as.Date(max(Unique_dates)), by = 91))

Switch <- tibble(Unique_dates, Dates_new) %>% 
  rename(Date = Unique_dates,
         Date_n = Dates_new)

Rus_in_trans <- In_raw %>% 
  group_by(Date) %>% 
  summarise(Total = sum(Total),
            Air = sum(Air),
            Car = sum(Car),
            Rail = sum(Rail),
            Ship = sum(Ship),
            Walk = sum(Walk)
  ) %>% 
  cbind(ISO_A3 = "RUS", Country = "Russia") %>% 
  mutate(Year = as.integer(year(Date))) %>%
  mutate(Month = as.character(month(Date, label = T, abbr = T))) %>%
  mutate(Color = Color_in)

Rus_out_trans <- Out_raw %>% 
  group_by(Date) %>% 
  summarise(Total = sum(Total),
            Air = sum(Air),
            Car = sum(Car),
            Rail = sum(Rail),
            Ship = sum(Ship),
            Walk = sum(Walk)
  ) %>% 
  cbind(ISO_A3 = "RUS", Country = "Russia") %>% 
  mutate(Year = as.integer(year(Date))) %>%
  mutate(Month = as.character(month(Date, label = T, abbr = T))) %>%
  mutate(Color = Color_out)

In_raw_full <- rbind(In_raw, Rus_in_trans)

Out_raw_full <- rbind(Out_raw, Rus_out_trans)

# Reading the texting file for info boxes
Texting <- read_csv("data-raw/Texting.csv")

#### Circle elements end ####

#### Minicharts elements begin ####
# Merging all files and creating an SF collection
In_geo <- merge(In_raw_full, World, by = "ISO_A3") %>% 
  st_as_sf

# Merging all files and creating an SF collection
Out_geo <- merge(Out_raw_full, World, by = "ISO_A3") %>% 
  st_as_sf

# Extracting coordinates of countries' centroids
Lat_Lon_In <- In_geo$geometry %>% 
  st_centroid() %>% 
  st_coordinates()

# Extracting coordinates of countries' centroids
Lat_Lon_Out <- Out_geo$geometry %>% 
  st_centroid() %>% 
  st_coordinates()

# Merging Rus_raw file with lat/lon data for centroids
In_clean <- cbind(In_geo, Lat_Lon_In) %>% 
  rename(lat = X, lon = Y) %>% 
  add_column(latmow = Mos_latlon$latmow, 
             lonmow = Mos_latlon$lonmow) %>% 
  st_drop_geometry()

Out_clean <- cbind(Out_geo, Lat_Lon_Out) %>% 
  rename(lat = X, lon = Y) %>% 
  add_column(latmow = Mos_latlon$latmow, 
             lonmow = Mos_latlon$lonmow) %>% 
  st_drop_geometry()


# Replacing the lon & lat of RUS centroid with other coordinates
In_clean$lon[In_clean$ISO_A3 == "RUS"] <- 60
In_clean$lat[In_clean$ISO_A3 == "RUS"] <- 55

Out_clean$lon[Out_clean$ISO_A3 == "RUS"] <- 60
Out_clean$lat[Out_clean$ISO_A3 == "RUS"] <- 55

#### Minicharts UI begin ####

MC_mode <- radioButtons("view_mode",
                        h5("Select pie chart mode"),
                        choices = c("Total", "By transport type"), 
                        selected = "Total"
                        )

Transport <- selectInput("trans",
                         h5("Select transport type"),
                         choices = NULL,
                         multiple = T
                         )

Direction_mc <- selectInput("dir_mc",
                            h5("Select border crossing type"),
                            choices = c("Into Russia", "Out of Russia"),
                            selected = "Into Russia"
                            )

Size <- sliderInput("size", 
                    "Change pie chart sizes",
                    min = 200,
                    max = 400,
                    step = 50,
                    value = 300,
                    ticks = F
                    )

#### Minicharts UI end ####



#### Great circles UI begin ####

Direction <- selectInput(
  "dir",
  h5("Select border crossing type"),
  choices = c("Into Russia",
              "Into Russia - by populaiton",
              "Out of Russia",
              "Crossing balance"
              ),
  selected = "Into Russia"
)


Radio_number <- radioButtons(
  "number",
  h5("Select number of countries"),
  choices = c(1,5,10,20,30,50),
  selected = 10,
  inline = T
)

Slider_time <- sliderInput(
  "date",
  h5("Select date"),
  min = min(Dates_new),
  max = max(Dates_new),
  value = min(Dates_new),
  step = 91,
  timeFormat = "%b-%Y",
  ticks = T,
  animate = animationOptions(interval = 1500)
)

Provider_list <- c(
  "CartoDB.DarkMatter",
  "CartoDB.VoyagerNoLabels",
  "Esri.WorldGrayCanvas",
  "Esri.WorldShadedRelief",
  "Esri.WorldPhysical",
  "NASAGIBS.ViirsEarthAtNight2012",
  "Stamen.Watercolor",
  "Stamen.TerrainBackground"
)

Provider <- selectInput(
  "prov",
  h5("Select map view"),
  choices = Provider_list,
)

Provider_mc <- selectInput(
  "prov_mc",
  h5("Select map view"),
  choices = Provider_list,
)


Top_list_header <- htmlOutput("top_text")
Top_list_header_in <- htmlOutput(("top_text_in"))
Top_list_header_out <- htmlOutput(("top_text_out"))

Top_list_sgl <- fluidRow(
  style = "overflow-y: scroll; max-height: 83vh !important;",
  box(width = 8,
      htmlOutput("top_10_cntr")),
  box(width = 4,
      htmlOutput("top_10_nmbr"))
)

Top_list_pos_in <- fluidRow(
  style = "overflow-y: scroll; max-height: 37.5vh !important;",
  box(width = 8,
      htmlOutput("top_10_cntr_in")),
  box(width = 4,
      htmlOutput("top_10_nmbr_in"))
)

Top_list_pos_out <- fluidRow(
  style = "overflow-y: scroll; max-height: 37.5vh !important;",
  box(width = 8,
      htmlOutput("top_10_cntr_out")),
  box(width = 4,
      htmlOutput("top_10_nmbr_out"))
)

Top_list_panel <- box(width = 3,
                      column(
                        width = 12,
                        conditionalPanel(
                          condition = "input.dir != 'Crossing balance'",
                          Top_list_header,
                          Top_list_sgl),
                        conditionalPanel(
                          condition = "input.dir == 'Crossing balance'",
                          Top_list_header_in,
                          Top_list_pos_in,
                          HTML("<br>"),
                          Top_list_header_out,
                          Top_list_pos_out
                        )
                      ))

Title_text <- fluidRow(
  box(
    width = NULL,
    valueBoxOutput("Mnth_Year",
                 width = NULL)
  )
)

Title_text_mc <- fluidRow(
  box(
    width = NULL,
    valueBoxOutput("Mnth_Year_mc",
                   width = NULL)
  )
)

YOY_box_height <- "8.7vh"

Info_panel <- column(
  width = 3,
  box(
    width = NULL,
    valueBoxOutput(
      "total_value",
      width = NULL
      ),
    plotOutput("ch_total",
               height = YOY_box_height
               )
  ),
  box(
    width = NULL,
    valueBoxOutput("total_trend",
                   width = NULL),
    plotOutput("ch_yoy",
               height = YOY_box_height
               )
  ),
  box(
    width = NULL,
    valueBoxOutput("FX_value",
                   width = NULL),
    plotOutput("ch_fx",
               height = YOY_box_height
               )
  ),
  box(
    width = NULL,
    valueBoxOutput("FX_YOY",
                   width = NULL),
    plotOutput("ch_fx_yoy",
               height = YOY_box_height
               )
  )
)

Source <- h5("Source: Border Service of the Russian Federation", 
   a("(incoming /", href = "https://fedstat.ru/indicator/38479"),
   a(" outgoing)", href = "https://fedstat.ru/indicator/38480"),
   align = "center"
)

Map_panel <- fluidRow(
  conditionalPanel(
    condition = "input.Viz_type == 'Circle'",
    tags$style(type = "text/css",
               "#map {height: 75vh !important;} "),
    # Title_text,
    Info_panel,
    column(width = 6,
           Title_text,
           box(
             width = 12,
             leafletOutput("map", width = "100%"),
             Source
           )),
    Top_list_panel
  ),
  conditionalPanel(
    condition = "input.Viz_type == 'Chart'",
    tags$style(type = "text/css", 
               "#map_mc {height: 75vh !important;}"),
    column(width = 12,
           Title_text_mc,
           box(
             width = 12,
             leafletOutput("map_mc", width = "100%"),
             Source
           ))
  )
)

Control_panel <- dashboardSidebar(
  radioButtons(
    "Viz_type",
    h3("Select dashboard type"),
    c("Great Circles" = "Circle", "Mini Pie Charts" = "Chart")
  ),
  HTML("<br>"),
  conditionalPanel(condition = "input.Viz_type == 'Circle'",
                   Direction,
                   Radio_number,
                   Slider_time,
                   Provider
  ),
  conditionalPanel(condition = "input.Viz_type == 'Chart'",
                   Direction_mc,
                   MC_mode,
                   Transport,
                   Size,
                   Provider_mc
  ),
  h5(" "),
  h6("Coding and design (c) Maxim Kobzev",
     style = "font-family: Segoe Script;",
     align = "center")
)


# Create base leaflet map
basemap <- leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(data = Russia,
              smoothFactor = 0.3,
              fillOpacity = 0.8,
              dashArray = "1",
              color = "gray",
              weight = 1,
              opacity = 0.8,
              fillColor = "white") %>% 
  addLabelOnlyMarkers(
    lng = 37.62,
    lat = 55.75,
    label = "Moscow",
    labelOptions = labelOptions(
      noHide = T,
      textsize = "15px",
      opacity = 0.9
    )
  ) %>%
  setView(37.6, 55.8, 4)

basemap_mc <- leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(data = Russia,
              smoothFactor = 0.3,
              fillOpacity = 1,
              dashArray = "1",
              color = "gray",
              weight = 1,
              opacity = 1,
              fillColor = "white") %>%
  setView(25, 35, 3)

#### Circles UI end ####

ui <- dashboardPage(
  dashboardHeader(
    title ="Border crossings with Russia, 2010 - 2019",
    titleWidth = 635
  ),
  dashboardSidebar(Control_panel,
                   tags$head(includeCSS("styles-1.css")),
                   width = 320),
  dashboardBody(
    shinyDashboardThemes(theme = "purple_gradient"),
    Map_panel)
)


# server function
server <- function(input, output, session) {
  
#### Great circles part begin ####  
  No_of_countries <- reactive({
    as.integer(input$number)
  })
  
  # Selecting direction for GC part
  Direction_full <- reactive({
    if (input$dir == "Out of Russia") {
      Out
    } else {
      if (input$dir == "Into Russia") {
        In
      } else {
        if (input$dir == "Into Russia - by populaiton") {
          In_by_pop_on_date
        } else {
          Bal_full
          }
      }
    }
  })


  Direction_sorted <- reactive({
    req(input$dir)
    Direction_full() %>%
      filter(Sort <= No_of_countries()) %>% 
      merge(Switch, by = "Date") %>%
      merge(Euro, by = "Date") %>% 
      select(-c(Date)) %>%
      add_column(lonmow = Mos_latlon$lonmow,
                 latmow = Mos_latlon$latmow) %>% 
      merge(World, by = "ISO_A3") %>%
      st_as_sf
  })
  
  
  # Extracting centroids' coordinates
  Lat_Lon <- reactive({
    req(input$dir)
    Direction_sorted()$geometry %>%
    st_centroid() %>%
    st_coordinates()
  })

  # Creating master file for countries' polygons
  Master_sorted_geo <- reactive({
    cbind(Direction_sorted(), Lat_Lon()) %>%
      rename(lon = X, lat = Y)
  })
  
  # Creating master file for Crossing balance flow
  Master_sorted <- reactive({
    Master_sorted_geo() %>% 
      st_drop_geometry() %>% 
      as_tibble()
  })
  
  
  Master_box <- reactive({
    Master_sorted() %>%
      select(Date_n, Total_Rus, RUB_EUR) %>%
      unique() %>%
      arrange(Date_n) %>%
      mutate(
        Total_Rus_LY = lag(Total_Rus, 4),
        Total_Rus_YOY = (((Total_Rus - Total_Rus_LY)/abs(Total_Rus_LY))*100 - 1),
        RUB_EUR_LY = lag(RUB_EUR, 4),
        RUB_EUR_YOY = 100*(RUB_EUR - RUB_EUR_LY)/RUB_EUR_LY
      ) %>%
      select(Date_n, Total_Rus, Total_Rus_YOY, RUB_EUR, RUB_EUR_YOY) %>%
      replace_na(list(
        Total_Rus_LY = 0,
        Total_Rus_YOY = 0,
        RUB_EUR_LY = 0,
        RUB_EUR_YOY = 0
      ))
  })
  
  Master_chart <- reactive({
    Master_box() %>% 
    filter(Date_n <= input$date)
  })
  
  # Creating master frame for great circles
  Master_frame <- reactive ({
    req(input$date)
    req(input$dir)
    Master_sorted() %>% 
    filter(Date_n == input$date) 
  })
  
  Top_10 <- reactive({
    req(input$dir)
    Master_frame() %>% 
      filter(Sort <= No_of_countries()) %>% 
      arrange(desc(Total)) %>% 
      select(Date_n, Sort, Country, Total)
  })
  
  Top_10_in <- reactive({
    req(input$dir)
    Top_10() %>% 
    filter(Total >= 0)
  })
  
  Top_10_out <- reactive ({
    req(input$dir)
    Top_10() %>%
      filter(Total < 0) %>%
      arrange(Sort)
  })

  
  # Creating master frame for polygons
  Master_frame_geo <- reactive ({
    req(input$date)
    Master_sorted_geo() %>% 
      filter(Date_n == input$date)
  })
  
  Mon_Yr <- reactive({
    HTML(paste0(
      h3(
        Texting$Top_countries[Texting$Direction == input$dir],
        "quarter ending: ",
        unique(Master_frame()$Month[Master_frame()$Date_n == input$date]),
        "-",
        unique(Master_frame()$Year[Master_frame()$Date_n == input$date]),
        align = "center"
      ),
      sep = ""
    ))
  })
  
  Mon_Yr_mc <- reactive({
    HTML(paste0(
      h3(ifelse(input$dir == "Into Russia",
                "Incoming border crossings split by transport type",
                "Outgoing border crossings split by transport type"
                ),
        align = "center"
      ),
      sep = ""
    ))
  })
  
  Snap_value_cross <- reactive({
    Master_box()$Total_Rus[Master_box()$Date_n == input$date]
  })
  
  Info_box_value <- reactive({
    if (input$dir == "Into Russia - by populaiton") {
      paste0(format(round(Snap_value_cross(), 3),
                    big.mark = ","), 
             "%")
    } else {
      format(Snap_value_cross(), big.mark = ",")
    }
  })
  
  # Composing the text for Total info box
  Info_box_text_total <- reactive({
    if (input$dir != "Crossing balance") {
      paste0(Texting$Box_total[Texting$Direction == input$dir])
    } else {
      ifelse(
        Snap_value_cross() < 0,
        paste0("Outgoing crossings surplus ('Out' > 'In')"),
        paste0("Incoming crossings surplus ('In' > 'Out')")
      )
    }
  })
  
  Box_total <- reactive({
    valueBox(
      paste0(Info_box_value()),
      Info_box_text_total(),
      icon = icon(
        "walking"
        ),
      color = ifelse(
        Info_box_value() == 0,
        "purple",
        ifelse(Info_box_value() > 0, "green", "red")
      )
    )
  })
  
  Snap_value_cross_YOY <- reactive({
    Master_box()$Total_Rus_YOY[Master_box()$Date_n == input$date]
  })
  
  
  Info_box_YOY <- reactive({
      round(Snap_value_cross_YOY(), 2)
  })
  
  # Composing the text for YOY info box
  Info_box_text_YOY <- reactive({
    if (Snap_value_cross_YOY() == 0) {
      paste0("")
    } else {
      if (input$dir != "Crossing balance") {
        paste0(Texting$Box_YOY[Texting$Direction == input$dir])
      } else {
        ifelse(
          Snap_value_cross_YOY() < 0,
          paste0("YOY balance change (in favor of more outgoing border crossings)"),
          paste0("YOY balance change (in favor of more incoming border crossings)")
        )
      }
    }
  })
  
  # Composing the text for top countries info box
  Text_box_countries <- reactive({
    paste0(
      "Top ", 
      No_of_countries(),
      " countries"
    )
  })
  
  Text_box_countries_in <- reactive({
    paste0(
      "Top ", 
      No_of_countries(),
      " countries,",
      " incoming surplus"
    )
  })
  
  Text_box_countries_out <- reactive({
    paste0(
      "Top ", 
      No_of_countries(),
      " countries,",
      " outgoing surplus"
    )
  })
  
  Box_YOY <- reactive({
      valueBox(
        ifelse(Snap_value_cross_YOY() == 0,
               paste0(""),
        paste0(Info_box_YOY(), "%")),
        Info_box_text_YOY(),
        icon = icon(ifelse(
          Info_box_YOY() == 0,
          "",
          ifelse(Info_box_YOY() > 0,
                 "arrow-up",
                 "arrow-down")
        )),
        color = ifelse(
          Info_box_YOY() == 0,
          "purple",
          ifelse(Info_box_YOY() > 0, "green", "red")
        )
      )
  })

# Creating the theme for bar charts
  Theme_chart <- theme(
    panel.background = element_rect(fill = "#40367c"),
    plot.background = element_rect(fill = "#40367c", color = "#40367c"),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_blank()
  )

  Theme_axes <- theme(
    axis.text.x = element_text(color = "#80b1dd"),
    axis.text.y = element_text(color = "#80b1dd"),
    axis.title.y = element_text(color = "#80b1dd",
                                size = 10)
  )
  
  # Creating theme elements for Y axis texts
  Theme_YOY <- reactive ({
    if (Snap_value_cross_YOY() == 0) {
    }
    else {
      Theme_axes
    }
  })
  
  Scale_name <- reactive ({
    if(input$dir != "Into Russia - by populaiton") 
      {
    "Million"
    } else {
    "Percent"
    }
  })
  
  Scale_div <- reactive({
    if(input$dir != "Into Russia - by populaiton") 
    {1000000} else {1}
  })
  
  Round_factor <- reactive({
    if(input$dir != "Into Russia - by populaiton") 
    {1} else {2}
  })
  
  # Scaling the Y axis by c(min, mean, max)
  Scale_seq <- reactive ({
    c(
      round(min(Master_chart()$Total_Rus / Scale_div()), Round_factor()),
      round(mean(Master_chart()$Total_Rus / Scale_div()), Round_factor()),
      round(max(Master_chart()$Total_Rus / Scale_div()), Round_factor())
    )
  })
    
  # Scaling the Y axis by c(min, (max-min)/2, max)
  # Scale_Total <- reactive ({
  #         scale_y_continuous(name = Scale_name(),
  #                      breaks = seq(round(min(Master_chart()$Total_Rus/Scale_div()), Round_factor()),
  #                                   round(max(Master_chart()$Total_Rus/Scale_div()), Round_factor()),
  #                                   by = round((max(Master_chart()$Total_Rus)-min(Master_chart()$Total_Rus))/Scale_div(), Round_factor())/2
  #                                   )
  #                      )
  #   })
  
  Scale_Total <- reactive ({
    scale_y_continuous(name = Scale_name(),
                       breaks = Scale_seq())
  })
  
# Creating the bar chart for total trend
Box_chart_total <- reactive({
  ggplot(Master_chart()) +
    geom_line(
      stat = "identity",
      size = 1.5,
      color = ifelse(Master_chart()$Total_Rus >= 0,
                     "MediumSeaGreen",
                     "#ff5050"),
      aes(x = Date_n,
          y = Total_Rus/Scale_div())
    ) +
    Theme_chart + 
    Theme_axes +
    Scale_Total()
})
  
  # Creating the bar chart for YOY trend
  Box_chart_YOY <- reactive({
    ggplot(Master_chart(),
           aes(x = Date_n,
               y = Total_Rus_YOY)) +
      geom_col(fill = ifelse(Master_chart()$Total_Rus_YOY >= 0, 
                             "MediumSeaGreen", 
                             "#ff5050")
               ) + 
      Theme_chart + 
      Theme_YOY() +
      scale_y_continuous(name = "%%",
        breaks = seq(
        round(min(Master_chart()$Total_Rus_YOY), 0),
        round(max(Master_chart()$Total_Rus_YOY), 0),
        (round(max(Master_chart()$Total_Rus_YOY) - min(Master_chart()$Total_Rus_YOY)))
      )
      )
  })
  
  Snap_value_FX_YOY <- reactive({
    Master_box()$RUB_EUR_YOY[Master_box()$Date_n == input$date]
  })
  
  # Creating an FX value for FX value box
  FX_value <- reactive({
    format(round(Master_box()$RUB_EUR[Master_box()$Date_n == input$date], 2), big.mark = ",")
  })
  
  # Creating an FX value for FX value box
  FX_value_YOY <- reactive({
    format(round(Snap_value_FX_YOY(), 2), big.mark = ",")
  })
  
  # Composing the text for FX info box
  FX_text <- paste0("RUB/EUR exchange rate")
  
  # Composing the text for FX YOY info box
  FX_text_YOY <- reactive({
    if(Snap_value_FX_YOY() == 0) {
      paste0("")
    } else {
    paste0("YOY change, RUB/EUR exchange rate")
    }
  })
  
  Box_FX_total <- reactive({
    valueBox(
      paste0(FX_value()),
      FX_text,
      icon = icon("euro-sign"),
      color = "yellow")
  })
  
  Box_FX_YOY <- reactive({
    valueBox(
      ifelse(Snap_value_FX_YOY() == 0,
             paste0(""),
             paste0(FX_value_YOY(), "%")),
      FX_text_YOY(),
      icon = icon(ifelse(
        FX_value_YOY() == 0,
        "",
        ifelse(FX_value_YOY() > 0,
               "arrow-up",
               "arrow-down")
      )),
      color = ifelse(
        FX_value_YOY() == 0,
        "purple",
        ifelse(FX_value_YOY() > 0, "green", "red")
      )
    )
  })
  

  # Creating the chart for EUR/RUB FX rate
  Box_chart_FX <- reactive({
    ggplot(Master_chart()) +
      geom_line(stat = "identity",
                color = "#e6c300",
                size = 1.5,
                aes(x = Date_n,
                    y = RUB_EUR)) +
      Theme_chart +
      Theme_axes +
      scale_y_continuous(name = "RUB/EUR",
                         breaks = seq(
        round(min(Master_chart()$RUB_EUR), 1),
        round(max(Master_chart()$RUB_EUR), 1),
        (round(max(Master_chart()$RUB_EUR) - min(Master_chart()$RUB_EUR)))/2
      )
      )
  })
  
  # Creating the chart for EUR/RUB FX YOY
  Box_chart_FX_YOY <- reactive({
    ggplot(Master_chart(),
           aes(x = Date_n,
               y = RUB_EUR_YOY)) +
      geom_col(fill = ifelse(Master_chart()$RUB_EUR_YOY >= 0, 
                             "MediumSeaGreen", 
                             "#ff5050")
      ) + 
      Theme_chart + 
      Theme_YOY() +
      scale_y_continuous(name = "%%",
                         breaks = seq(
                           round(min(Master_chart()$RUB_EUR_YOY), 0),
                           round(max(Master_chart()$RUB_EUR_YOY), 0),
                           (round(max(Master_chart()$RUB_EUR_YOY) - min(Master_chart()$RUB_EUR_YOY)))
                         )
      )
  })
  
  
  # Creating ranked numbers' and countries' list
  Top_countries <- reactive({
    HTML(
      paste0(
        paste0(Top_10()$Sort, 
               ". ", 
               Top_10()$Country
               ),
        collapse = "<br/>"
      )
    )
  })
  
  Top_countries_in <- reactive({
    HTML(
      paste0(
        paste0(Top_10_in()$Sort, 
               ". ", 
               Top_10_in()$Country
        ),
        collapse = "<br/>"
      )
    )
  })
  
  Top_countries_out <- reactive({
    HTML(
      paste0(
        paste0(Top_10_out()$Sort, 
               ". ", 
               Top_10_out()$Country
        ),
        collapse = "<br/>"
      )
    )
  })
  
  
  List_value <- reactive ({
    paste0(format(Top_10()$Total, big.mark = ","))
  })
  
  List_value_in <- reactive ({
    paste0("<span style = color:#80b1dd>", format(Top_10_in()$Total, big.mark = ","), "</span>")
  })
  
  List_value_out <- reactive ({
    paste0("<span style = color:#ff5733>", format(-Top_10_out()$Total, big.mark = ","), "</span>")
  })
  
  # Creating values' list
  Top_numbers <- reactive({
    if(input$dir == "Into Russia - by populaiton") {
    HTML(
      paste0(
        paste0(round(Top_10()$Total, 2), " %"),
        collapse = "<br/>"
      )
    )
    } else {
      HTML(
        paste0(
          List_value(),
          collapse = "<br/>"
        )
      )
    }
  })
  
  Top_numbers_in <- reactive({
      HTML(
        paste0(
          List_value_in(),
          collapse = "<br/>"
        )
      )
  })
  
  Top_numbers_out <- reactive({
    HTML(
      paste0(
        List_value_out(),
        collapse = "<br/>"
      )
    )
  })

# Creating the top countries title
 Top_list_text <- reactive({
   HTML(paste0(h4(Text_box_countries(),
                  align = "center")
               )
        )
 })
 
 Top_list_text_in <- reactive({
   HTML(paste0(h5(Text_box_countries_in(),
                  align = "center")
   )
   )
 })
 
 Top_list_text_out <- reactive({
   HTML(paste0(h5(Text_box_countries_out(),
                  align = "center")
   )
   )
 })

 # Producing the outputs
 output$top_text <- renderUI(Top_list_text())
 output$top_text_in <- renderUI(Top_list_text_in())
 output$top_text_out <- renderUI(Top_list_text_out())
 output$top_10_cntr <- renderUI(Top_countries())
 output$top_10_nmbr <- renderUI(Top_numbers())
 output$top_10_cntr_in <- renderUI(Top_countries_in())
 output$top_10_nmbr_in <- renderUI(Top_numbers_in())
 output$top_10_cntr_out <- renderUI(Top_countries_out())
 output$top_10_nmbr_out <- renderUI(Top_numbers_out())
 output$Mnth_Year <- renderUI(Mon_Yr())
 output$Mnth_Year_mc <- renderUI(Mon_Yr_mc())
 output$total_value <- renderValueBox({Box_total()})
 output$total_trend <- renderValueBox({Box_YOY()})
 output$ch_total <- renderPlot({Box_chart_total()})
 output$ch_yoy <- renderPlot({Box_chart_YOY()})
 output$FX_value <- renderValueBox({Box_FX_total()})
 output$FX_YOY <- renderValueBox({Box_FX_YOY()})
 output$ch_fx <- renderPlot({Box_chart_FX()})
 output$ch_fx_yoy <- renderPlot({Box_chart_FX_YOY()})
 output$Source_out <- renderUI({tagList(Out_link)})
 output$Source_in <- renderUI({tagList(In_link)})
 
 # Defining the columns for great circles' lat/long pairs
 Col_choice_1 <-  reactive({
   Master_frame()[, c("lonmow", "latmow")]
 })
 
 Col_choice_2 <-  reactive({
     Master_frame()[, c("lon","lat")]
 })
 
  # Creating great circles
  curved.lines <- reactive({
    gcIntermediate(
      p1 = as.matrix(x =  Col_choice_1()),
      p2 = as.matrix(x =  Col_choice_2()),
      breakAtDateLine = TRUE,
      n = 100,
      addStartEnd = T,
      sp = TRUE,
    )
  })
  
  Balance_sign <- reactive ({
    if_else(Master_frame_geo()$Total > 0, 
            "Incoming surplus",
            "Outgoing surplus")
  })
  
  Bal_sign_rus <-reactive ({
    if_else(Master_frame_geo()$Total_Rus > 0, 
            "incoming surplus",
            "outgoing surplus")
  })
  
  Balance_dir <- reactive ({
    if (input$dir == "Crossing balance")
    {
      "Crossing balance"
    } else {
      if (input$dir == "Into Russia")
      {
        "incoming"
      } else {
        if (input$dir == "Out of Russia")
        {
          "outgoing"
        } else {""}
      }
    }
  })
  
  # Creating labels for countries' hover-ups
  labels <- reactive({
    if (input$dir == "Crossing balance") {
      sprintf(
        "Incoming from <strong>%s: </strong> %.1fK<br/>
        Outgoing into <strong>%s: </strong> %.1fK<br/>
        %s: %.1fK border crossings <br/>
        quarter ending: <br/>
        %s",
        Master_frame()$Country,
        Master_frame()$In / 1000,
        Master_frame()$Country,
        Master_frame()$Out / 1000,
        Balance_sign(),
        abs(Master_frame()$Total) / 1000,
        paste0(Master_frame()$Month, "-", Master_frame()$Year)
      ) %>%
        lapply(htmltools::HTML)
    } else {
      if (input$dir == "Into Russia - by populaiton") {
        sprintf(
        "<strong> %.2f </strong> percent of population of <br/>
        <strong> %s </strong> <br/>
        crossed a border with Russia <br/>
        at least once, quarter ending: <br/>
        %s",
          abs(Master_frame()$Total),
          Master_frame()$Country,
          paste0(Master_frame()$Month,  "-", Master_frame()$Year)
        ) %>%
          lapply(htmltools::HTML)
      } else {
        if (input$dir == "Into Russia") {
          sprintf(
            "Citizens of <strong>%s: </strong> <br/>
        %.1fK incoming border crossings <br/>
        quarter ending: <br/>
        %s",
            Master_frame()$Country,
            abs(Master_frame()$Total) / 1000,
            paste0(Master_frame()$Month, "-", Master_frame()$Year)
          ) %>%
            lapply(htmltools::HTML)
        } else {
          sprintf(
            "Russian citizens to <strong>%s: </strong> <br/>
        %.1fK outgoing border crossings <br/>
        quarter ending: <br/>
        %s",
            Master_frame()$Country,
            abs(Master_frame()$Total) / 1000,
            paste0(Master_frame()$Month, "-", Master_frame()$Year)
          ) %>%
            lapply(htmltools::HTML)
        }
      }
    }
  })
  
  
  label_rus <- reactive({
    if (input$dir == "Crossing balance") {
      sprintf(
        "<strong> Russian Federation: </strong> </br> 
        total %s of <br/> 
        <strong> %s </strong> border crossings, <br/>
        quarter ending: <br/>
        %s",
        Bal_sign_rus(),
        format(Master_frame()$Total_Rus, big.mark = ","),
        paste0(Master_frame()$Month, "-", Master_frame()$Year)
      ) %>%
        lapply(htmltools::HTML)
    } else {
      if (input$dir == "Into Russia" | input$dir == "Out of Russia"){
      sprintf(
        "<strong> Russian Federation: </strong> </br> 
        total of <strong> %s </strong> </br> 
        %s border crossings, </br>
        quarter ending: <br/>
        %s",
        format(Master_frame()$Total_Rus, big.mark = ","),
        Balance_dir(),
        paste0(Master_frame()$Month, "-", Master_frame()$Year)
      ) %>%
        lapply(htmltools::HTML)
      } else {}
    }
  })
  
  # Initialize map
  output$map <- renderLeaflet({
    basemap
  })
  
  # Creating a function for redrawing the maps
  Countries <- function(map) {
    map %>%
    addPolygons(
      data = Master_frame_geo(),
                group = "countries",
                smoothFactor = 0.3,
                fillOpacity = 0.7,
                dashArray = "1",
                color = "white",
                weight = 1,
                opacity = 1,
                fillColor = ~ Master_frame()$Color,
                highlight = highlightOptions(
                  weight = 2,
                  color = "#666",
                  dashArray = "",
                  fillOpacity = 0.9,
                  bringToFront = F
                ),
                label = labels(),
                labelOptions = labelOptions(
                  style = list(
                    "border" = 0,
                    "font-weight" = "lighter",
                    "color" = "#ffffff",
                    "background-color" = "#40367c",
                    "line-height" = "14px",
                    padding = "5px 5px"
                  ),
                  # style = list("font-weight" = "normal",
                  #              padding = "3px 8px"),
                  textsize = "14px",
                  direction = "auto"
                )
    )
  }
  
  # Creating a function for redrawing the lines
  Lines <- function(map) {
    map %>% 
    addPolylines(
      data = curved.lines(),
      group = "lines",
      color = Master_frame()$Color,
      weight = abs(Master_frame()$Total) * 30 / max(abs(Master_frame()$Total)),
      label = labels(),
      opacity = 1
    )
  }
  
  Rus_marker <- function(map) {
    map %>%
      addLabelOnlyMarkers(
        lng = 80,
        lat = 60,
        label = label_rus(),
        labelOptions = labelOptions(
          style = list(
            "color" = ifelse(
              input$dir == "Into Russia",
              "blue",
              ifelse(
                input$dir == "Out of Russia",
                "red",
                ifelse(
                  input$dir == "Into Russia - by populaiton",
                  "white",
                  ifelse(Bal_sign_rus() == "travel surplus",
                         "blue",
                         "red")
                )
              )
            ),
            "font-family" = "Segoe UI",
            # "border-color" = "white",
            "box-shadow" = "0px 0px #ffffff"
          ),
          noHide = T,
          textsize = "16px",
          opacity = 0.8,
          textOnly = F
        )
      )
  }
  
  # Observing the changes of direction
  observeEvent(input$dir, {
    leafletProxy("map", session) %>%
      clearGroup(group = c("countries", "lines")) %>%
      clearMarkers() %>% 
      Countries %>%
      Lines
  })
  
  # Observing the changes of dates
  observeEvent(input$date, {
    leafletProxy("map", session) %>%
      clearGroup(group = c("countries", "lines")) %>%
      clearMarkers() %>% 
      Countries %>% 
      Lines
  })
  
  # Observing the changes in No.of countries
  observeEvent(input$number, {
      leafletProxy("map", session) %>%
      clearGroup(group = c("countries", "lines")) %>%
      clearMarkers() %>% 
      Countries %>%
      Lines
  })
  
  # Observing the provider tiles selection
  observeEvent(input$prov, {
    leafletProxy("map", session) %>%
      clearGroup(group = c("countries", "lines")) %>%
      clearMarkers() %>% 
      addProviderTiles(input$prov) %>% 
      Countries %>%
      Lines
  })
 
### Great circles part end ####   

  
### Minichart part begin ####

  Travel_type <- reactive({
    if(input$dir_mc == "Into Russia") {
      names(In_clean)[4:8]
    } else {
      names(Out_clean)[4:8]
    }
  })
  
  Clean_selection <- reactive({
    if(input$dir_mc == "Into Russia") {
      In_clean
    } else {
      Out_clean
    }
  })
  
  
  data_select <- reactive ({
    if (input$view_mode == "Total") {
      Clean_selection()[, Travel_type()]
    } else {
      # req(input$trans)
      Clean_selection()[, input$trans]
    }
  })
  
  Totals <- reactive ({
    if(input$view_mode == "Total"){
      Clean_selection()$Total
    } else {
      if(length(input$trans) == 1) {
        Clean_selection()[, input$trans]
      } 
      else {
        rowSums(Clean_selection()[, input$trans])
      }
    }
  })

  
  Minicharts <- function(map) {
    map %>%
      addMinicharts(
        Clean_selection()$lat,
        Clean_selection()$lon,
        layerId = Clean_selection()$Country,
        chartdata = data_select(),
        time = Clean_selection()$Date,
        type = "pie",
        width = input$size * sqrt(Totals()) / sqrt(max(Totals())),
        height = input$size * sqrt(Totals()) / sqrt(max(Totals())),
        showLabels = T
      )
  }
  
  output$map_mc <- renderLeaflet({
    basemap_mc
  })
  
  output$graph_mc <- renderPlot({
    Graph_mc()
  })

  observeEvent(input$dir_mc, {
    leafletProxy("map_mc", session) %>%
      clearMinicharts() %>%
      Minicharts
  })
  
  observeEvent(input$view_mode, {
    if (input$view_mode == "By transport type") {
      updateSelectInput(session,
                        "trans",
                        choices = Travel_type(),
                        selected = "Air")
    } else {
      updateSelectInput(session, "trans",
                        choices = character(0))
    }
    leafletProxy("map_mc", session) %>%
      clearMinicharts() %>%
      Minicharts
  })
  
  observeEvent(input$trans, {
    leafletProxy("map_mc", session) %>%
      clearMinicharts() %>%
      Minicharts
  })
  
  observeEvent(input$size, {
    leafletProxy("map_mc", session) %>%
      clearMinicharts() %>%
      Minicharts
  })
  
  observeEvent(input$prov_mc, {
    leafletProxy("map_mc", session) %>%
      clearMinicharts() %>%
      addProviderTiles(input$prov_mc) %>%
      Minicharts
  })
  
  
### Minichart part end ####   
  
  observeEvent(input$Viz_type, {
    if(input$Viz_type == "Circles") {
    leafletProxy("map", session) %>%
      clearMinicharts() %>%
      clearGroup(group = c("countries", "lines")) %>%
      clearMarkers() %>% 
      Rus_marker %>%
      Countries %>% 
      Lines
    } else {
    leafletProxy("map_mc", session) %>%
      clearMinicharts() %>%
      Minicharts
    }
  }) 
  
  
}

shinyApp(ui, server)