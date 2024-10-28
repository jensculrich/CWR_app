###########################
# ui.R for CWR Shiny App  #
###########################

# Written by Jens Ulrich and Erika Luna Perez
# February 2022

###########################
# LIBRARIES               #
###########################

library(shiny)
library(sf) # the base package manipulating shapes
library(dplyr) # data wrangling
library(tidyverse) # data wrangling
library(tigris) # for joining spatial data with data frame classes
library(raster)
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



###########
# LOAD UI #
###########

# ui structure: one navbar page with 5 tab panels

ui <- fluidPage(
  
  includeCSS("www/style.css"),
  
  dashboardPage(
    
    skin = "blue",
    
    dashboardHeader(title = "Crop Wild Relatives (CWR) and Wild-utilized Plants (WUS) in Canada", titleWidth = 1000),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("What Are CWR?", tabName = "about", icon = icon("seedling")),
        menuItem("Find Native CWR", tabName = "find", icon = icon("thumbtack")),
        menuItem("Identify CWR Conservation Gaps", tabName = "explore", icon = icon("table")),
        menuItem("CWR Species Distribution Models", tabName = "apple", icon = icon("apple")),
        menuItem("About", tabName = "aknow", icon = icon("tasks"))
      ), # end sidebarMenu
      
      width = 300         
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
                    # user chooses to view map with ecoregion or province boundaries displayed
                    selectInput("inNativeProvincesOrEcoregions", "Choose a Geographic Display",
                                choices = c("Provinces", "Ecoregions")),
                    # want to update this so it's dependnet on users choice of provinces v. ecoregions
                    selectInput("inRegion", "Filter CWR List by a Region:", 
                                choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland and Labrador",
                                            "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", 
                                            "Ontario", "Prince Edward Island", "Quebec", 
                                            "Saskatchewan", "Yukon")),
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
                    selectInput("inSelectedCrop", "Select a Crop or WUS", 
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
        
        # Fourth tab element
        tabItem(tabName = "apple",
                
                includeMarkdown("www/apple.md"),
                
                fluidRow(
                  box(
                    width = 4,
                    collapsible = T,
                    
                    # want to update this so it's dependent on users choice of provinces v. ecoregions
                    # user chooses a species of interest
                    selectInput("inAppleSpecies", "Select an apple CWR species", 
                                choices = c("Malus coronaria (sweet crabapple)", 
                                            "Malus fusca (Pacific crabapple)", 
                                            ""),
                                selected = ""
                    ), # end select input
                    # user chooses an emissions scenario of interest
                    selectInput("inSelectedEmissions", "Select an emissions scenario", 
                                choices = c("low (ssp245)", "high (ssp585)"),
                                selected = ""
                    ), # end select input
                    # user chooses a time projection of interest
                    selectInput("inSelectedProjection", "Select a time projection", 
                                choices = c("2030", "2050", "2070"),
                                selected = ""
                    ), # end select input
                    # user chooses a habitat suitability degree
                    # update this so that user can choose a CWR without first selecting crop
                    selectInput("inSelectedHabitatSuitability", "Select degree of habitat suitability", 
                                choices = c("",
                                            "low to high", 
                                            "moderate to high", 
                                            "high"),
                                selected = "moderate to high"
                    ) # end select input
                  ), # end box
                  
                  box(#title = "Range map", solidHeader = T,
                    width = 8, collapsible = T,
                    leafletOutput("choroplethPlot3")
                  ) # end box
                  
                ), # end fluidRow
                
                includeMarkdown("www/apple2.md")
                
        ), # end fifth tabItem element
        
        tabItem(tabName = "aknow", 
                
                includeMarkdown("www/aknow.md")
        ) # end fifth tabItem element
       
      ) # end tabItems
    ) # end dashboardBody
  ) # end dashboardPage
) # ui

