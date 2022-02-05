###########################
# ui.R for CWR Shiny App  #
###########################

# Written by Jens Ulrich and Erika Luna Perez

###########################
# LIBRARIES               #
###########################

library(shiny)
library(sf) # the base package manipulating shapes
library(spdplyr) # the `dplyr` counterpart for shapes
library(dplyr) # data wrangling
library(tidyverse) # data wrangling
library(tigris) # for joining spatial data with data frame classes
library(leaflet)
library(htmltools)
library(shinydashboard)
library(DT)

########################################
# DATA WRANGLING AND SUPPORT FUNCTIONS #
########################################

# Load required data and shapefiles for  
# building reactive maps and data tables 

# canada_ecoregions_geojson defines ecoregions in Canada, clipped to the national border of Canada
canada_ecoregions_geojson <- st_read("data/canada_ecoregions_clipped.geojson", quiet = TRUE)
# canada_provinces_geojson defines province and territory boundaries
canada_provinces_geojson <- st_read("data/canada_provinces.geojson", quiet = TRUE)

# province_gap_table includes all garden accessions from our surveyed gardens
# with lat/long when applicable (needs to be formatted here or before uploading)
# The table has a row for each native province that a species is native to with garden = NA
# along with a row for each garden accession from each native province.
# ecoregion_gap_table has similar setup
ecoregion_gap_table_t <- as_tibble(read.csv("data/ecoregion_gap_table_by_taxon.csv"))

dbHeader <- dashboardHeader(title = "My Dashboard",
                            tags$li(a(href = 'http://shinyapps.company.com',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.company.com',
                                      img(src = 'ubc.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

###########
# LOAD UI #
###########

# ui structure: one navbar page with 4 tab panels

ui <- fluidPage(
  
  includeCSS("www/style.css"),
  
  dashboardPage(
    
    skin = "purple",
    
    dashboardHeader(title = "Conservation of Canadian Crop Wild Relatives (CWR)", titleWidth = 500),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("What are CWR?", tabName = "about", icon = icon("seedling")),
        menuItem("Find Native CWR", tabName = "find", icon = icon("thumbtack")),
        menuItem("CWR Conservation", tabName = "explore", icon = icon("map marked alt")),
        menuItem("Acknowledgements", tabName = "aknow", icon = icon("tasks"))
        
      ) # end sidebarMenu
      
    ), # end dashboardSidebar
    
    dashboardBody(
      tabItems(
        # First tabItem element
        tabItem(tabName = "home",
                includeMarkdown("www/home.md")
        ), # end first tabItem element
        
        # Second tab element
        tabItem(tabName = "about",
                includeMarkdown("www/about.md")
        ), # end second tabItem element
        
        # Third tab element
        tabItem(tabName = "find",
                
                includeMarkdown("www/find.md"),
                
                fluidRow(
                  box(#title = "Find Native CWR",
                    #solidHeader = T,
                    width = 4,
                    collapsible = T,
                    
                    # want to update this so it's dependent on users choice of provinces v. ecoregions
                    # user chooses a group of interest
                    selectInput("inSelectedGroup", "Select a CWR or WUS group of interest", 
                                choices = c("Food crop relatives", "Forest resources",
                                            "Forage and feed crops", "Wild-utilized plant species")
                                ), # end select input
                    # user may choose an ecoregion of interest (or click on one from the map)
                    selectInput("inRegion", "Filter CWR List by a Region:",
                                choices = ecoregion_gap_table_t$ECO_NAME
                                ), # end select input
                  ), # end box
                    
                  box(#title = "Range map", solidHeader = T,
                    width = 8, collapsible = T,
                    leafletOutput("choroplethPlot"),
                    dataTableOutput("nativeRangeTable")
                  ) # end box
                ) # end fluidRow
                  
        ), # end third tabItem element
        
        tabItem(tabName = "explore",
                
                includeMarkdown("www/explore.md"),
                
                fluidRow(
                  box(#title = "Find Native CWR",
                    #solidHeader = T,
                    width = 4,
                    collapsible = T,
                    
                    # want to update this so it's dependent on users choice of provinces v. ecoregions
                    # user chooses a group of interest
                    selectInput("inSelectedGroup2", "Select a CWR or WUS group of interest", 
                                choices = c("Food crop relatives", "Forest resources",
                                            "Forage and feed crops", "Wild-utilized plant species")
                                ), # end select input
                    # user chooses a crop of interest
                    selectInput("inSelectedUse", "Select a Crop Category", 
                                choices = "", selected = ""
                                ), # end select input
                    # user chooses a crop of interest
                    selectInput("inSelectedCrop", "Select a Crop", 
                                choices = "", selected = ""
                                ), # end select input
                    # user chooses a CWR (filtered to match the selected crop)
                    # update this so that user can choose a CWR without first selecting crop
                    selectInput("inSelectedCWR", "Select a Crop Wild Relative or WUS", 
                                choices = "", selected = ""
                                ) # end select input
                  ), # end box
                  
                  box(#title = "Range map", solidHeader = T,
                    width = 8, collapsible = T,
                    leafletOutput("choroplethPlot2"),
                    includeMarkdown("www/conservation_tab.md"),
                    # provide summary data for the CWR
                    dataTableOutput("gapTable")
                  ) # end box
                  
                ) # end fluidRow
                
        ), # end fourth tabItem element
        
        tabItem(tabName = "aknow", 
                
                includeMarkdown("www/aknow.md")
        ) # end fifth tabItem element
       
      ) # end tabItems
    ) # end dashboardBody
  ) # end dashboardPage
) # ui

