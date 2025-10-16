## Manage data refresh
## This script provides a console menu permitting the user
## to invoke functions to load the data from the web via
## the menu options or to see instructions for downloading
## manually where needed.

# Load Libraries ----
library(tidyverse)

# Source Data Load Functions ----
source("00_R_scripts/eg.v.2_download_and_prep_data.R")

data_refresh_main <- function(){
    choice <- select.list(choices = c("See document Emerald Gardens Data: Base Tables Collection and Preparation",
                                      "Refresh eg_plotting_tbl", 
                                      "Refresh merged_property_info_tbl",
                                      "CANCEL"),
                          title = "Data Refresh Options"
                          )
    if(choice == "See document Emerald Gardens Data: Base Tables Collection and Preparation"){
                browseURL(url = "04_documentation/emerald_gardens_tibble_refresh.html/", 
                  browser = getOption("browser"), 
                  encodeIfNeeded = FALSE)
            }else if(choice == "Refresh eg_plotting_tbl"){
                    reload_eg_plotting_tbl()
            }else if(choice == "Refresh merged_property_info_tbl"){
                    reload_merged_property_info_tbl()
            }else if(choice == "CANCEL"){
                    return("Action Cancelled. Run data_refresh_main() to resume.")
            }
}

reload_eg_plotting_tbl <- function(){
    print_instructions()
    
    choice <- select.list(choices = c("Load from local file.", "Read refreshing data document.", "CANCEL"))

    if(choice == "Load from local file."){
        make_eg_plotting_tbl()
    }else if(choice == "Read refreshing data document."){
        browseURL(url = "04_documentation/emerald_gardens_tibble_refresh.html/", 
                  browser = getOption("browser"), 
                  encodeIfNeeded = FALSE)
    }else{
        return()
    }
}

reload_merged_property_info_tbl <- function(){
    merge_property_cards()
}

print_instructions <- function() {
    instructions <- c("*******************************************", 
                      "Data for ParcelHosted.shp file will be loaded from",
                  "'01_raw_data/ParcelHosted/ParcelHosted.shp'",
                  "",
                  "If you need to source the data from the SCPA web site,",
                  "select option 2 to see instructions.",
                  "*******************************************")
    
    writeLines(instructions)
}
