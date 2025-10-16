### Functions to create interactive ggplotly() maps with custom tool tips.
library(tidyverse)
library(plotly)
library(sf)
library(RSelenium)

# Load map creation tibbles if not already present
if (!exists("property_info_pages_tbl", envir = .GlobalEnv)){
    property_info_pages_tbl <- read_rds("02_processed_data/property_info_pages_tbl.rds")
    }

if (!exists("eg_mapping_tbl", envir = .GlobalEnv)) {
    eg_mapping_tbl <- read_rds("02_processed_data/eg_mapping_tbl.rds")
}

if (!exists("streets_tbl", envir = .GlobalEnv)){
    streets_tbl <- read_rds("02_processed_data/streets_tbl.rds")
}

if (!exists("tool_tips_tbl", envir = .GlobalEnv)) {
    tool_tips_tbl <- read_rds("02_processed_data/tool_tips_tbl.rds")
}

###############################################################


 joined_tbl <- eg_mapping_tbl %>%
     left_join(tool_tips_tbl %>%
                   select(-geometry), by = "account") %>%
     mutate(lon = st_coordinates(eg_mapping_tbl$centroid)[,1],
            lat = st_coordinates(eg_mapping_tbl$centroid)[,2])


# Interactive maps ----

# Owner Occupied Map (updated) ----
 make_interactive_owner_occupied_map <- function(){
     # Add owner-occupied column to joined_tbl
     oo_tbl <- joined_tbl %>%
         mutate(
             owner_occupied = case_when(
                 str_extract(string = mailing_addr,
                             pattern = "^\\d{4}.*(CIR|CT)") == str_extract(string = property_full_address,
                                                                           pattern = "^\\d{4}.*(CIR|CT)") ~ TRUE,
                 # Exceptions list
                 mailing_addr == "PO BOX 52323" ~ TRUE,
                 # Presumed rental
                 TRUE ~ FALSE
             )
         )
     oo_plot <- ggplot() +
             # Add polygon shapes for "Owner Occupied"
             geom_sf(data = oo_tbl,
                     aes(fill = owner_occupied),  # Explicit aesthetic mapping
                     color = "black",
                     alpha = 0.6,
                     lwd = 0.5) +

         # Add street numbers as text at centroids
         geom_text(data = joined_tbl,
                   aes(x = st_coordinates(centroid)[, 1],  # Longitude of centroid
                       y = st_coordinates(centroid)[, 2],  # Latitude of centroid
                       label = street_num),  # Use street_num for labels
                   color = "black",  # Text color
                   size = 3,  # Adjust text size as needed
                   hjust = 0.5,  # Horizontally center the text
                   vjust = 0.5,  # Vertically center the text
                   check_overlap = TRUE) +  # Avoid overlap of text

         # Add invisible points at centroids for tooltips
         geom_sf(data = joined_tbl,
                 aes(geometry = centroid, text = ownership_tool_tip),  # Tooltip mapping
                 color = "transparent",  # Ensures invisibility
                 size = 0.001,
                 alpha = 0) +

         # Set custom colors for "Owner Occupied" and "Rental"
         scale_fill_manual(
             values = c("TRUE" = "green", "FALSE" = "orange"),
             labels = c("TRUE" = "Owner Occupied", "FALSE" = "Rental")
         ) +

         # Set axis labels
         labs(x = "Longitude", y = "Latitude") +

         # Minimal theme
         theme_minimal()

     # Convert to interactive map
     oo_plot <- ggplotly(oo_plot) %>%
         add_annotations(
             x = streets_tbl$lon,
             y = streets_tbl$lat,
             text = streets_tbl$street_name,
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default tooltips
     oo_plot$x$data[[2]]$hoverinfo <- "none"
     oo_plot$x$data[[3]]$hoverinfo <- "none"

     # Add account numbers to the layer with tooltips
     oo_plot$x$data[[5]]$customdata <- joined_tbl$account

     # Reduce line widths
     oo_plot$x$data[[2]]$line$width <- 1
     oo_plot$x$data[[3]]$line$width <- 1

     # Hide default legend
     oo_plot$x$data[[2]]$showlegend <- FALSE
     oo_plot$x$data[[3]]$showlegend <- FALSE

     # Substitute legend
     oo_plot <- oo_plot %>%
         layout(
             annotations = list(
                 # "Owner Occupied" legend item with text below the color block
                 list(
                     x = 0.86, y = 0.53,  # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "#A4F0B7",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "Owner Occupied" block
                 list(
                     x = 0.88, y = 0.48,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "Owner<br>Occupied",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 29
                 ),
                 # "Rental" legend item with text below the color block
                 list(
                     x = 0.86, y = 0.42,   # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "orange",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "Rental" block
                 list(
                     x = 0.872, y = 0.38,   # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "Rental",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 60, height = 20
                 )
             )
         )

     return(oo_plot)
 }

# Pool Map (updated) ----
 make_interactive_pool_map <- function(){

     pool_plot <- ggplot() +
         # Add polygon shapes for "Has Pool"
         geom_sf(data = joined_tbl,
                 aes(fill = pool),  # Explicit aesthetic mapping
                 color = "black",
                 alpha = 0.6,
                 lwd = 0.5) +


         # Add invisible points at centroids for tooltips ("Has Pool")
         geom_sf(data = joined_tbl,
                 aes(geometry = centroid, text = pool_tooltip),  # Tooltip mapping
                 color = "transparent",  # Ensures invisibility
                 size = 0.001,
                 alpha = 0) +

         # Add street numbers as text at centroids
         geom_text(data = joined_tbl,
                   aes(x = st_coordinates(centroid)[, 1],  # Longitude of centroid
                       y = st_coordinates(centroid)[, 2],  # Latitude of centroid
                       label = street_num),  # Use street_num for labels
                   color = "black",  # Text color
                   size = 3,  # Adjust text size as needed
                   hjust = 0.5,  # Horizontally center the text
                   vjust = 0.5,  # Vertically center the text
                   check_overlap = TRUE) +  # Avoid overlap of text

         # Set custom colors for "Has Pool" and "No Pool"
         scale_fill_manual(
             values = c("TRUE" = "lightblue", "FALSE" = "yellow"),
             labels = c("TRUE" = "Has Pool", "FALSE" = "No Pool")
         ) +

         # Set axis labels
         labs(x = "Longitude", y = "Latitude") +

         # Minimal theme
         theme_minimal()

     # Convert to interactive map
     pool_plot <- ggplotly(pool_plot) %>%
         add_annotations(
             x = streets_tbl$lon,
             y = streets_tbl$lat,
             text = streets_tbl$street_name,
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default tooltips
     pool_plot$x$data[[2]]$hoverinfo <- "none"
     pool_plot$x$data[[3]]$hoverinfo <- "none"

     # Add account numbers to the layer with tooltips
     pool_plot$x$data[[4]]$customdata <- joined_tbl$account

     # Reduce line widths
     pool_plot$x$data[[2]]$line$width <- 1
     pool_plot$x$data[[3]]$line$width <- 1

     # Hide default legend
     pool_plot$x$data[[2]]$showlegend <- FALSE
     pool_plot$x$data[[3]]$showlegend <- FALSE

     # Substitute legend
     pool_plot <- pool_plot %>%
         layout(
             annotations = list(
                 # "Has Pool" legend item with text below the color block
                 list(
                     x = 0.86, y = 0.50,  # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "lightblue",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "Has Pool" block
                 list(
                     x = 0.872, y = 0.46,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "Has Pool",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 60, height = 20
                 ),
                 # "No Pool" legend item with text below the color block
                 list(
                     x = 0.86, y = 0.42,   # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "yellow",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "No Pool" block
                 list(
                     x = 0.872, y = 0.38,   # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "No Pool",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 60, height = 20
                 )
             )
         )

     return(pool_plot)
 }

# Last Sale Map (updated) ----
 make_interactive_last_sale_map <- function(){

     last_sale_tbl <- joined_tbl %>%
         mutate(most_recent_sale_date = as.Date(most_recent_sale_date, "%m/%d/%Y")) %>%
         mutate(
             recent_sale_bin = case_when(
                 as.numeric(difftime(Sys.Date(), most_recent_sale_date, units = "days")) / 365.25 <= 1 ~ "Past Year",
                 as.numeric(difftime(Sys.Date(), most_recent_sale_date, units = "days")) / 365.25 > 1 &
                     as.numeric(difftime(Sys.Date(), most_recent_sale_date, units = "days")) / 365.25 <= 5 ~ "1 to 5 Years",
                 as.numeric(difftime(Sys.Date(), most_recent_sale_date, units = "days")) / 365.25 > 5 &
                     as.numeric(difftime(Sys.Date(), most_recent_sale_date, units = "days")) / 365.25 <= 10 ~ "6 to 10 Years",
                 TRUE ~ "More than 10 Years"
             )
         ) %>%
         mutate(recent_sale_bin = factor(recent_sale_bin, levels = c(
                                                                         "Past Year",
                                                                         "1 to 5 Years",
                                                                         "6 to 10 Years",
                                                                         "More than 10 Years"
                                                                     )
                                     )
         )

     recent_sale_plot <- ggplot() +
         # Add color
         geom_sf(data = last_sale_tbl, aes(fill = factor(recent_sale_bin))) +

         # Specify colors for legend
         scale_fill_manual(values = c("Past Year" = "lightgreen",
                                      "1 to 5 Years" = "lightblue",
                                      "6 to 10 Years" = "yellow",
                                      "More than 10 Years" = "lightgray"),
                           labels = c("Past Year" = "Past Year",
                                      "1 to 5 Years" = "1 to 5 Years",
                                      "6 to 10 Years" = "5 to 10 Years",
                                      "More than 10 Years" = "More than 10 Years")
         ) +

        labs(fill = "Last Sale") +

         # Add invisible points at centroids for tooltips
         geom_sf(data = last_sale_tbl,
                 aes(geometry = centroid, text = recent_sale_tooltip),  # Tooltip mapping
                 color = "transparent",  # Ensures invisibility
                 size = 0.001,
                 alpha = 0) +

         # Add street numbers as text at centroids
         geom_text(data = joined_tbl,
                   aes(x = st_coordinates(centroid)[, 1],  # Longitude of centroid
                       y = st_coordinates(centroid)[, 2],  # Latitude of centroid
                       label = street_num),  # Use street_num for labels
                   color = "black",  # Text color
                   size = 3,  # Adjust text size as needed
                   hjust = 0.5,  # Horizontally center the text
                   vjust = 0.5,  # Vertically center the text
                   check_overlap = TRUE) + # Avoid overlap of text
         # Set axis labels
         labs(x = "Longitude", y = "Latitude") +
         theme_minimal()

     # Convert to interactive map
     recent_sale_plot <- ggplotly(recent_sale_plot) %>%
         add_annotations(
             x = streets_tbl$lon,
             y = streets_tbl$lat,
             text = streets_tbl$street_name,
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default tooltips
     recent_sale_plot$x$data[[2]]$hoverinfo <- "none"
     recent_sale_plot$x$data[[3]]$hoverinfo <- "none"
     recent_sale_plot$x$data[[4]]$hoverinfo <- "none"
     recent_sale_plot$x$data[[5]]$hoverinfo <- "none"

     # Add account numbers to the layer with tooltips
     recent_sale_plot$x$data[[6]]$customdata <- joined_tbl$account

     # Set line widths for shapes to 1
     recent_sale_plot$x$data[[2]]$line$width <- 1
     recent_sale_plot$x$data[[3]]$line$width <- 1
     recent_sale_plot$x$data[[4]]$line$width <- 1
     recent_sale_plot$x$data[[5]]$line$width <- 1

     # Remove default legend
     recent_sale_plot$x$data[[2]]$showlegend <- FALSE
     recent_sale_plot$x$data[[3]]$showlegend <- FALSE
     recent_sale_plot$x$data[[4]]$showlegend <- FALSE
     recent_sale_plot$x$data[[5]]$showlegend <- FALSE

     # Substitute legend
     recent_sale_plot <- recent_sale_plot %>%
         layout(
             annotations = list(
                 # Legend name
                 list(
                     x = 0.88, y = 0.75,
                     xref = "paper", yref = "paper",
                     text = "Last Sale",
                     showarrow = FALSE,
                     font = list(size = 16, style = "bold"),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 75, height = 20
                 ),
                 list(
                     x = 0.86, y = 0.70,  # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "lightgreen",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "Past Year" block
                 list(
                     x = 0.872, y = 0.64,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "Past Year",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 60, height = 20
                 ),

                 # "1 to 5 Years" legend item with text below
                 list(
                     x = 0.86, y = 0.58,  # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "lightblue",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "1 to 5 Years" block
                 list(
                     x = 0.88, y = 0.54,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "1 to 5 Years",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 70, height = 20
                 ),

                 # "6 to 10 Years" legend item with text below
                 list(
                     x = 0.86, y = 0.48,  # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "yellow",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "6 to 10 Years" block
                 list(
                     x = 0.88, y = 0.44,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "6 to 10 Years",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 78, height = 20
                 ),

                 # "More than 10 Years" legend item with text below
                 list(
                     x = 0.86, y = 0.38,  # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "lightgray",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "More than 10 Years" block
                 list(
                     x = 0.89, y = 0.30,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "More than<br>10 Years",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 94, height = 30
                 )
             )
         )

     return(recent_sale_plot)
 }


# Effective Year Built map (updated) ----
 make_interactive_effective_year_built_map <- function(){
     # Prep data tables for construction this map.

     # Extracts the account and eff_yr_built columns from property_info_pages_tbl
     # and binds them into a new tibble (eff_yr_built_tbl).
     eff_yr_built_tbl <- bind_cols(property_info_pages_tbl %>%
                                       select(account), property_info_pages_tbl$buildings_info %>%
                                       bind_rows()) %>%
         select(account, eff_yr_built)

    # Bind eff_yr_built_tbl to eg_mapping_tbl in order to color lots by effective year built
     eyb_eg_mapping_tbl <- eg_mapping_tbl %>%
         left_join(eff_yr_built_tbl, by = c("account" = "account")) %>%
         mutate(eff_yr_built_bin =
                    case_when(
                        eff_yr_built <= 2000 ~ "Pre 2001",
                        eff_yr_built > 2000 & eff_yr_built <= 2005 ~ "2001 to 2005",
                        eff_yr_built > 2005 & eff_yr_built <= 2010 ~ "2006 to 2010",
                        eff_yr_built > 2010 & eff_yr_built <= 2015 ~ "2011 to 2015",
                        eff_yr_built > 2015 & eff_yr_built <= 2020 ~ "2016 to 2020",
                        TRUE ~ "After 2020"
                    )
         ) %>%
         mutate(eff_yr_built_bin = factor(eff_yr_built_bin,
                                          levels = c(
                                              "After 2020",
                                              "2016 to 2020",
                                              "2011 to 2015",
                                              "2006 to 2010",
                                              "2001 to 2005",
                                              "Pre 2001"
                                          )
                                   )
         )

    # Join eg_mapping_tbl to tool_tips_tbl
     eyb_eg_mapping_tbl <- eyb_eg_mapping_tbl %>%
         left_join(tool_tips_tbl, by = "account") %>%
         select(eff_yr_built_bin, everything())

    # End data prep section

     # Begin map creation section

     effective_year_built_plot <- ggplot() +
         # Add color
         geom_sf(data = eyb_eg_mapping_tbl, aes(fill = factor(eff_yr_built_bin))) +

         # Specify colors for legend
         scale_fill_manual(values = c(
             "After 2020" = "orangered",
             "2016 to 2020" = "#F0F8FF",
             "2011 to 2015" = "#FFC1C1",
             "2006 to 2010" = "violet",
             "2001 to 2005" = "lightblue",
             "Pre 2001" = "lightgray"
         ),
         labels = c("After 2020" = "After 2020",
                    "2016 to 2020" = "2016 to 2020",
                    "2011 to 2015" = "2011 to 2015",
                    "2006 to 2010" = "2011 to 2015",
                    "2001 to 2005" = "2001 to 2005",
                    "Pre 2001" = "Pre 2001")
         ) +
         labs(fill = "Effective Year Built") +

         # Add invisible points at centroids for tooltips
         geom_sf(data = eyb_eg_mapping_tbl,
                 aes(geometry = centroid, text = eff_yr_built_tool_tip),  # Tooltip mapping
                 color = "transparent",  # Ensures invisibility
                 size = 0.001,
                 alpha = 0) +

         # Add street numbers as text at centroids
         geom_text(data = eyb_eg_mapping_tbl,
                   aes(x = st_coordinates(centroid)[, 1],  # Longitude of centroid
                       y = st_coordinates(centroid)[, 2],  # Latitude of centroid
                       label = street_num),  # Use street_num for labels
                   color = "black",  # Text color
                   size = 3,  # Adjust text size as needed
                   hjust = 0.5,  # Horizontally center the text
                   vjust = 0.5,  # Vertically center the text
                   check_overlap = TRUE) + # Avoid overlap of text
         # Set axis labels
         labs(x = "Longitude", y = "Latitude") +
         theme_minimal()

     # Convert static plot to interactive plot
     effective_year_built_plot <- effective_year_built_plot %>%
         ggplotly() %>%
         add_annotations(
             x = streets_tbl$lon,
             y = streets_tbl$lat,
             text = streets_tbl$street_name,
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default tooltips
     effective_year_built_plot$x$data[[2]]$hoverinfo <- "none"
     effective_year_built_plot$x$data[[3]]$hoverinfo <- "none"
     effective_year_built_plot$x$data[[4]]$hoverinfo <- "none"
     effective_year_built_plot$x$data[[5]]$hoverinfo <- "none"
     effective_year_built_plot$x$data[[6]]$hoverinfo <- "none"

     # Add account numbers to the layer with tooltips
     effective_year_built_plot$x$data[[7]]$customdata <- joined_tbl$account

     # Remove default legend
     effective_year_built_plot$x$data[[2]]$showlegend <- FALSE
     effective_year_built_plot$x$data[[3]]$showlegend <- FALSE
     effective_year_built_plot$x$data[[4]]$showlegend <- FALSE
     effective_year_built_plot$x$data[[5]]$showlegend <- FALSE
     effective_year_built_plot$x$data[[6]]$showlegend <- FALSE

     # Reduce shape border widths
     effective_year_built_plot$x$data[[2]]$line$width <- 1
     effective_year_built_plot$x$data[[3]]$line$width <- 1
     effective_year_built_plot$x$data[[4]]$line$width <- 1
     effective_year_built_plot$x$data[[5]]$line$width <- 1
     effective_year_built_plot$x$data[[6]]$line$width <- 1

     # Substitute legend
     effective_year_built_plot <- effective_year_built_plot %>%
         layout(
             annotations = list(
                 # Legend name
                 list(
                     x = 0.91, y = 0.78,
                     xref = "paper", yref = "paper",
                     text = "Effective Year\nBuilt",
                     showarrow = FALSE,
                     font = list(size = 16, style = "bold"),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 110, height = 60
                 ),
                 # "After 2020" legend item with text below
                 list(
                     x = 0.88, y = 0.70,  # Color block position
                     xref = "paper", yref = "paper",
                     text = " ",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "orangered",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "After 2020" block
                 list(
                     x = 0.9, y = 0.64,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "After 2020",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 70, height = 20
                 ),
                 # "2016 to 2020" legend item with text below
                 list(
                     x = 0.88, y = 0.60,  # Color block position
                     xref = "paper", yref = "paper",
                     text = " ",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "#F0F8FF",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "2016 to 2020" block
                 list(
                     x = 0.9, y = 0.56,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "2016 to 2020",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 20
                 ),
                 # "2011 to 2015" legend item with text below
                 list(
                     x = 0.88, y = 0.52,  # Color block position
                     xref = "paper", yref = "paper",
                     text = " ",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "#FFC1C1",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "2011 to 2015" block
                 list(
                     x = 0.9, y = 0.48,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "2011 to 2015",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 20
                 ),
                 # "2006 to 2010" legend item with text below
                 list(
                     x = 0.88, y = 0.44,  # Color block position
                     xref = "paper", yref = "paper",
                     text = " ",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "violet",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "2006 to 2010" block
                 list(
                     x = 0.9, y = 0.40,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "2006 to 2010",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 20
                 ),
                 # "2001 to 2005" legend item with text below
                 list(
                     x = 0.88, y = 0.36,  # Color block position
                     xref = "paper", yref = "paper",
                     text = " ",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "lightblue",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "2001 to 2005" block
                 list(
                     x = 0.9, y = 0.31,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "2000 to 2005",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 20
                 ),
                 # "Pre 2001" legend item with text below
                 list(
                     x = 0.88, y = 0.25,  # Color block position
                     xref = "paper", yref = "paper",
                     text = " ",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = "lightgray",
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "Pre 2001" block
                 list(
                     x = 0.9, y = 0.22,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "Pre 2001",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 20
                 )
             )
         )

     return(effective_year_built_plot)
 }

# Assessed Per Square Foot map (distinct structure owing to continuous scale) ----
 make_interactive_assessed_per_sqft_map <- function() {
     # Calculate assessed value per square foot
     data_tbl <- joined_tbl %>%
         mutate(assessed_per_sqft = assessed_value_of_bldg / grnd_area)

     # Create base ggplot
     assessed_per_sqft_plot <- ggplot() +
         geom_sf(data = data_tbl, aes(fill = assessed_per_sqft), color = "black", size = 0.1) +
         scale_fill_gradient(
             low = "snow2",
             high = "steelblue2",
             name = "Assessed Value per Sq.Ft.\nBuilding Area",
             labels = scales::label_dollar()
         ) +
         # Add invisible points at centroids for tooltips
         geom_sf(
             data = joined_tbl,
             aes(geometry = centroid, text = assessed_value_pr_sqft_tooltip),
             color = "transparent",
             size = 0.001,
             alpha = 0
         ) +
         # Add street numbers as text
         geom_text(
             data = joined_tbl,
             aes(
                 x = st_coordinates(centroid)[, 1],
                 y = st_coordinates(centroid)[, 2],
                 label = street_num
             ),
             color = "black",
             size = 3,
             hjust = 0.5,
             vjust = 0.5,
             check_overlap = TRUE
         ) +
         labs(x = "Longitude", y = "Latitude") +
         theme_minimal()

     # Convert to interactive plotly
     assessed_per_sqft_plot <- assessed_per_sqft_plot %>%
         ggplotly() %>%
         layout(
             legend = list(
                 x = 1,
                 xanchor = "center",
                 title = list(text = "Legend Title", side = "top")
             )
         ) %>%
         add_annotations(
             x = streets_tbl$lon,
             y = streets_tbl$lat,
             text = streets_tbl$street_name,
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Adjust shape line widths
     assessed_per_sqft_plot$x$data <- map(
         assessed_per_sqft_plot$x$data,
         function(layer) {
             if (!is.null(layer$line$width)) {
                 layer$line$width <- 1
             }
             layer
         }
     )

     # Remove unwanted tooltips from shape layers, retain custom tooltip
     assessed_per_sqft_plot$x$data <- imap(
         assessed_per_sqft_plot$x$data,
         function(layer, i) {
             if (i >= 2 && i <= 128 && !is.null(layer$text)) {
                 layer$text <- NULL
             }
             if (i == 130 && !is.null(layer$hovertext)) {
                 layer$hovertext <- layer$hovertext  # Keep custom tooltip
             }
             layer
         }
     )

     # Add account numbers to the appropriate layer
     assessed_per_sqft_plot$x$data[[129]]$customdata <- joined_tbl$account

     return(assessed_per_sqft_plot)
 }

# Roof Age Map Functions
# Roofs and Building Permits map ----

 convert_to_date <- function(date_str) {
     tryCatch({
         as.Date(date_str, format = "%b %d, %Y")
     }, error = function(e) {
         message(paste("Error: Could not convert '", date_str, "' to date."))
         return(as.Date(NA))
     })
 }

 format_tooltip <- function(text, max_length = 40) {
     words <- str_split(text, "\\s+")[[1]] # Split into words
     lines <- c()
     current_line <- ""

     for (word in words) {
         if (nchar(current_line) + nchar(word) + 1 <= max_length) {
             current_line <- paste(current_line, word)
         } else {
             lines <- c(lines, current_line)
             current_line <- word
         }
     }
     lines <- c(lines, current_line) # Add the last line

     return(paste(lines, collapse = "<br>"))
 }

 # Insert contents of harvest_building_permit_data.R here.
 # The function there automatically scrapes the web site
 # and produces a consolidated building permits tibble.

 # This function is not called directly, but is used in a call to
 # map() in the create_building_permits_tbl() function.

 fetch_building_permit_data <- function(street, street_type){
     tryCatch({
         system("pkill -f geckodriver")
         # Start a Selenium server and browser
         rD <- rsDriver(browser = "firefox", port = 4545L, verbose = FALSE)
         remDr <- rD$client

         # Navigate to the search page
         remDr$navigate("https://building.scgov.net/PublicPortal/Sarasota/SearchPermits.jsp")

         # Populate the form fields
         street_name <- remDr$findElement(using = "id", value = "StreetName")

         # Construct the XPath expression using text()
         xpath_expression <- paste0("//select[@name='PropStreetType']/option[text()='", street_type, "']")
         street_type_element <- remDr$findElement(using = "xpath", value = xpath_expression)

         zip_code <- remDr$findElement(using = "id", value = "ZipCode")
         search_button <- remDr$findElement(using = "id", value = "btnSearch")

         street_name$sendKeysToElement(list(street))
         street_type_element$clickElement()
         zip_code$sendKeysToElement(list("34233"))

         # Click the search button
         search_button$clickElement()

         # Wait for the results to load (adjust as needed)
         Sys.sleep(5)

         # Get the page source
         page_source <- remDr$getPageSource()[[1]]

         # Parse the HTML
         parsed_results <- rvest::read_html(page_source)

         # Extract the table
         tables <- parsed_results %>% rvest::html_table()
         search_results_table <- tables[[10]]
         new_colnames <- as.character(unlist(search_results_table[1,]))
         search_results_table <- search_results_table[-1,]
         colnames(search_results_table) <- new_colnames

         # Print the table
         #    print(search_results_table)

         # Close the browser and server
         remDr$close()
         rD$server$stop()

     }, error = function(e) {
         message("An error occurred:", e$message)
     }, finally = {
         # Ensure the Java process is killed
         # kill_java_process(4545)
     })

     return(search_results_table)

 }

 # No need to call this function directly. It is used in
 # the write_rds() call below.
 create_building_permits_tbl <- function(){
     argument_pairs <- list(
         list("Diamond", "Circle"),
         list("Topaz", "Court"),
         list("Opal", "Court")
     )

     # Web scraping
     building_permits_tbl <- map(argument_pairs, function(pair){
         fetch_building_permit_data(pair[[1]], pair[[2]])
     })

     # Combine data from each street into one tibble
     building_permits_tbl <- bind_rows(building_permits_tbl)

     # Eliminate rows not containing records for construction permits
     building_permits_tbl <- building_permits_tbl %>%
         filter(!`Permit Type` %in% c(
             "Zoning Compliance",
             "Property Records Request",
             "Request for Service",
             "Request",
             "False Alarm Billing",
             "Building Compliance",
             "Real Property Compliance",
             "Tree Permit"
         )) %>%
         filter(!str_ends(Work, "Complaint")) %>%
         filter(Work != "Request")


     # Replace empty Work column values
     building_permits_tbl <- building_permits_tbl %>%
         mutate(Work = ifelse(Work == "" & `Permit Type` == "Minor Repairs", "Minor Repairs", Work),
                Work = ifelse(Work == "New (Complete)"|Work == "New", `Permit Type`, Work),
                Work = ifelse(Work == "Exterior", "Minor Repairs", Work)
         )

     # Eliminate columns not used for tooltip or pop-up table
     building_permits_tbl <- building_permits_tbl %>%
         select(c(-`Permit #`, -`Permit Type`, -OCC, -Status))


     # Rename columns
     building_permits_tbl <- building_permits_tbl %>%
         rename(
             account = `Parcel Number`,
             permit_date = `In Date`
         )
     # Convert permit_date column from charcter to date type
     building_permits_tbl$permit_date <- building_permits_tbl$permit_date %>% convert_to_date()

     # Trim leading and trailing white space from columns
     building_permits_tbl <- building_permits_tbl %>% mutate(across(where(is.character), str_trim))

     building_permits_tbl <- building_permits_tbl %>%
         mutate(re_roof = if_else(Work %in% c("Re-Roofing",
                                              "Reroof-Shingle Tear Off / Replace",
                                              "Reroof-Shingle over 1 Layer Shingle Only"), TRUE, FALSE
         )
         )


     return(building_permits_tbl)
 }

 # Testing - next statement is for testing purposes
 # building_permits_tbl <- create_building_permits_tbl()

 # Save building_permits_tbl to file
 # Uncomment next line to pull data for permits stored in older system
 # Not likely to be used as this will not change. New permits stored
 # in the Accela site.

 # write_rds(create_building_permits_tbl(), "02_processed_data/building_permits_tbl.rds")

 # Data returned by fetch_building_permit_data() is pre-2024
 # For more recent records its necessary to search  on
 # https://aca-prod.accela.com/SARASOTACO/Cap/CapHome.aspx?module=Building&TabName=Building
 # Having downloaded that data we must perform some transformations.
 #
 # Data entry on the Accela site is not controlled. After downloading each file,
 # inspect it to look for irregular content, edit as required before saving it
 # and running the function merge_recent_permits().

 # Merge Diamond Circle, Topaz Court, and Opal Court records
 merge_recent_permits <- function(){

     Diamond_Cir <- read_csv(list.files("01_raw_data/") %>%
                                 str_subset(., regex("accela_diamond", ignore_case = TRUE)) %>%
                                 paste0("01_raw_data/", .))
     Topaz_Ct <- read_csv(list.files("01_raw_data/") %>%
                              str_subset(., regex("accela_topaz", ignore_case = TRUE)) %>%
                              paste0("01_raw_data/", .))
     Opal_Ct <- read_csv(list.files("01_raw_data/") %>%
                             str_subset(., regex("accela_opal", ignore_case = TRUE)) %>%
                             paste0("01_raw_data/", .))

     recent_permits <- bind_rows(Diamond_Cir, Topaz_Ct, Opal_Ct) %>%
         select(-c(7,8)) %>%
         rename(Work = Description, permit_date = Date) %>%
         # This data does not include an account column
         # The following call to mutate() creates an Address column
         # That can be used to match a column of the same name and
         # structure in the tibble created by extracting the older
         # permit data found on https://building.scgov.net/PublicPortal/Sarasota/SearchPermits.jsp"
         mutate(Address = str_extract(`Project Name`, "^[^,]*"),
                Address = str_remove(Address, "\\*PP\\* "),
                street_number = str_extract(Address, "^\\d{4}"),
                street_name = str_extract(Address, "(?i)Diamond|Opal|Topaz"),
                join_column = str_to_title(paste(street_number, street_name)),
                Address = str_to_title(Address),
                permit_date = as.Date(permit_date, format = "%m/%d/%Y"),
                re_roof = if_else(`Record Type` == "Residential Roofing Permit", TRUE, FALSE)
         ) %>%
         filter(Status == "Closed - Complete")

     eg_mapping_tbl <- read_rds("02_processed_data/eg_mapping_tbl.rds") %>%
         select(account, street_num, street_name) %>%
         sf::st_drop_geometry() %>%
         mutate(join_column = str_to_title(paste(street_num, street_name)))

     recent_permits <- left_join(recent_permits, eg_mapping_tbl, by = "join_column") %>%
         select(-c(
             street_number,
             `Record Number`,
             `Record Type`,
             street_name.x,
             # join_column,
             street_num,
             street_name.y,
             `Project Name`,
             Status)
         ) %>%
         select(account, everything())

     return(recent_permits)
 }

 # Merge old data with Accela data
 create_building_permit_history_tbl <- function(){
     accela_permits_tbl <- read_rds("02_processed_data/accela_building_permits_tbl.rds")
     older_permits_tbl <- read_rds("02_processed_data/building_permits_tbl.rds")

     bind_rows(accela_permits_tbl, older_permits_tbl)
 }

 # Create re-roofing tibble with one row for each house with data for the most recent
 # re-roof.

 create_reroofed_houses_tbl <- function(){
     reroofed_houses_tbl <- create_building_permit_history_tbl() %>%
         select(-join_column) %>%
         mutate(Address = str_to_title(Address)) %>%
         filter(re_roof == TRUE) %>%
         group_by(account) %>%
         arrange(desc(permit_date)) %>%
         slice(1)

     # Add tooltip column
     reroofed_houses_tbl <- reroofed_houses_tbl %>%
         mutate(tooltip = paste("Permit Date:", as.character(permit_date), "<br>", format_tooltip(Work, max_length = 30)))

     return(reroofed_houses_tbl)
 }

 # Create a DT for each house with a row for each building permit, sort by date descending ----

 # Function to create a building permits DT::datatable for a single house
 make_property_permit_history_DT <- function(account_num){
     if(!exists("building_permit_history_tbl", envir = .GlobalEnv)){
         building_permit_history_tbl <- create_building_permit_history_tbl()
     }
     if(building_permit_history_tbl[building_permit_history_tbl$account == account_num,] %>% nrow() > 0){
         property_permit_history_DT <- building_permit_history_tbl %>%
             filter(account == account_num) %>%
             arrange(desc(permit_date)) %>%
             select(-c(Address, join_column, re_roof)) %>%
             DT::datatable(
                 .,
                 caption = htmltools::tags$caption(
                     style = 'caption-side: top; text-align: center; font-size: 18px; font-weight: bold;',
                     property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                             2][[1]][[1]]$situs_addr %>%
                         str_remove(" SARASOTA, FL, 34233")
                 ),
                 options = list(columnDefs = list(list(
                     targets = which(names(.) == "account"),
                     visible = FALSE
                 )))
             )

     }else{
         property_permit_history_DT <- DT::datatable(tibble("Permit History" = "No permits on record"))
     }
     return(property_permit_history_DT)
 }

 # Function to create a tibble of DT::datatable objects identified by account
 create_building_permits_datatables_tibble <- function(accounts) {

     datatables_tbl <- tibble(
         account = accounts,
         datatable = map(accounts, ~{
             dt <- make_property_permit_history_DT(.x)
             class(dt) <- c("datatables", class(dt)) # ENSURE it is of class datatables
             return(dt)
         })
     )

     return(datatables_tbl)
 }

 # Add datatble column to reroofed_houses_tbl
 add_DT_column <- function(left_tbl = create_reroofed_houses_tbl(),
                           right_tbl = NULL){
     if(!exists("eg_mapping_tbl", .GlobalEnv)){
         eg_mapping_tbl <- read_rds("02_processed_data/eg_mapping_tbl.rds")
     }
     right_tbl <- create_building_permits_datatables_tibble(eg_mapping_tbl$account)
     new_tbl <- left_join(left_tbl, right_tbl, by = "account")

     return(new_tbl)
 }

 # Join with eg_mapping_tbl to add geometry and centroid columns and covert to sf format.

 create_building_permits_mapping_tibble <- function(map_tbl = eg_mapping_tbl, building_permits_tbl = add_DT_column()){

     if(!exists("eg_mapping_tbl", .GlobalEnv)){
         eg_mapping_tbl <- read_rds("02_processed_data/eg_mapping_tbl.rds")
     }

     building_permits_mapping_tibble <- left_join(map_tbl,
                                                  building_permits_tbl,
                                                  by = "account") %>%
         select(account, street_num, permit_date, tooltip, datatable, centroid, geometry)

     # Create bin column for grouping roof replacement periods
     building_permits_mapping_tibble <- building_permits_mapping_tibble %>%
         mutate(re_roof_bin = case_when(
             .$permit_date <= Sys.Date() & .$permit_date >= Sys.Date() - 365.25 ~ "Past Year",
             .$permit_date <= Sys.Date() - 365.25 &
                 .$permit_date >= Sys.Date() - (365.25 * 5) ~ "1 to 5 Years",
             .$permit_date <= Sys.Date() - (365.25 * 5) &
                 .$permit_date >= Sys.Date() - (365.25 * 10) ~ "6 to 10 Years",
             .$permit_date <= Sys.Date() - (365.25 * 10) &
                 .$permit_date >= Sys.Date() - (365.25 * 15) ~ "11 to 15 Years",
             .$permit_date <= Sys.Date() - (365.25 * 15) &
                 .$permit_date >= Sys.Date() - (365.25 * 20) ~ "16 to 20 Years",
             .$permit_date <= Sys.Date() - (365.25 * 20) ~ "More than 20 Years"
         ),
         re_roof_bin =  factor(re_roof_bin, levels = c(
             "Past Year",
             "1 to 5 Years",
             "6 to 10 Years",
             "11 to 15 Years",
             "16 to 20 Years",
             "More than 20 Years")
         )
         )

     return(building_permits_mapping_tibble)
 }

 # Create interactive roof map
 make_interactive_roof_map <- function(){
     if(!exists("building_permits_mapping_tbl", envir = .GlobalEnv)){
         building_permits_mapping_tbl <- read_rds("02_processed_data/building_permits_mapping_tbl.rds")
     }

     if (!exists("streets_tbl", envir = .GlobalEnv)){
         streets_tbl <- read_rds("02_processed_data/streets_tbl.rds")
     }

     roof_map_plot <- ggplot() +
         geom_sf(
             data = building_permits_mapping_tbl,
             aes(fill = re_roof_bin),
             alpha = 0.6,
             lwd = 0.5
         ) +
         scale_fill_manual(
             name = "Re-Roofing Period",
             values = c(
                 "Past Year" = "#FC8D62",
                 "1 to 5 Years" = "#FFFF33",
                 "6 to 10 Years" = "#E78AC3",
                 "11 to 15 Years" = "#4DAF4A",
                 "16 to 20 Years" = "#377EB8",
                 "More than 20 Years" = "#B0B0B0"

             )
         ) +
         # Add street numbers as text at centroids
         geom_text(data = building_permits_mapping_tbl,
                   aes(x = st_coordinates(centroid)[, 1],  # Longitude of centroid
                       y = st_coordinates(centroid)[, 2],  # Latitude of centroid
                       label = street_num),  # Use street_num for labels
                   color = "black",  # Text color
                   size = 3,  # Adjust text size as needed
                   hjust = 0.5,  # Horizontally center the text
                   vjust = 0.5,  # Vertically center the text
                   check_overlap = TRUE) +  # Avoid overlap of text
         # Add invisible points at centroids for tooltips
         geom_sf(data = building_permits_mapping_tbl,
                 aes(geometry = centroid, text = tooltip),  # Tooltip mapping
                 color = "transparent",  # Ensures invisibility
                 size = 0.001,
                 alpha = 0) +

         # Set axis labels
         labs(x = "Longitude", y = "Latitude") +

         # Minimal theme
         theme_minimal()

     # Convert to interactive map
     roof_map_plot <- ggplotly(roof_map_plot) %>%
         add_annotations(
             x = streets_tbl$lon,
             y = streets_tbl$lat,
             text = streets_tbl$street_name,
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default legend
     roof_map_plot$x$data[[2]]$showlegend <- FALSE
     roof_map_plot$x$data[[3]]$showlegend <- FALSE
     roof_map_plot$x$data[[4]]$showlegend <- FALSE
     roof_map_plot$x$data[[5]]$showlegend <- FALSE
     roof_map_plot$x$data[[6]]$showlegend <- FALSE
     roof_map_plot$x$data[[7]]$showlegend <- FALSE

     # Remove default tooltips
     roof_map_plot$x$data[[2]]$text <- ""
     roof_map_plot$x$data[[3]]$text <- ""
     roof_map_plot$x$data[[4]]$text <- ""
     roof_map_plot$x$data[[5]]$text <- ""
     roof_map_plot$x$data[[6]]$text <- ""
     roof_map_plot$x$data[[7]]$text <- ""

     # Add account numbers to customdata
     roof_map_plot$x$data[[9]]$customdata <- building_permits_mapping_tbl$account

     # Substitute legend
     roof_map_plot <- roof_map_plot %>%
         layout(
             annotations = list(
                 # Legend title
                 list(
                     x = 0.915, y = 0.88,  # Color block position
                     xref = "paper", yref = "paper",
                     text = "Most Recent<br>Roof Replacement",
                     showarrow = FALSE,
                     font = list(size = 14),
                     align = "center",
                     bgcolor = "",
                     bordercolor = "#ffffff",
                     borderwidth = 0,
                     width = 150, height = 30
                 ),
                 # "Past Year" legend item with text below the color block
                 list(
                     x = 0.88, y = 0.83,  # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = scales::alpha("#FC8D62", alpha = 0.6),
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "Past Year" block
                 list(
                     x = 0.895, y = 0.795,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "Past Year",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 29
                 ),
                 # "1 - 5 Years" legend item with text below the color block
                 list(
                     x = 0.88, y = 0.76,   # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = scales::alpha("#FFFF33", alpha = 0.6),
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "1 - 5 Years" block
                 list(
                     x = 0.895, y = 0.72,   # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "1 - 5 Years",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 73, height = 20
                 ),
                 # "6 - 10 Years" legend item with text below the color block
                 list(
                     x = 0.88, y = 0.68,   # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = scales::alpha("#E78AC3", alpha = 0.6),
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "6 - 10 Years" block
                 list(
                     x = 0.895, y = 0.625,   # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "6 - 10 Years",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 20
                 ),
                 # 11 to 15 Years" legend item with text below the color block
                 list(
                     x = 0.88, y = 0.58,   # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = scales::alpha("#4DAF4A", alpha = 0.6),
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "11 to 15 Years" block
                 list(
                     x = 0.91, y = 0.54,   # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "11 to 15 Years",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 115, height = 20
                 ),
                 # "16 to 20 Years" legend item with text below the color block
                 list(
                     x = 0.88, y = 0.50,   # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = scales::alpha("#377EB8", alpha = 0.6),
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "16 to 20 Years" block
                 list(
                     x = 0.90, y = 0.46,   # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "16 to 20 Years",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 100, height = 50
                 ),
                 # "More than 20 Years" legend item with text below the color block
                 list(
                     x = 0.88, y = 0.42,   # Color block position
                     xref = "paper", yref = "paper",
                     text = "",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "left",
                     bgcolor = scales::alpha("#B0B0B0", alpha = 0.6),
                     bordercolor = "black",
                     borderwidth = 1,
                     width = 30, height = 30
                 ),
                 # Text below "More than 20 Years" block
                 list(
                     x = 0.905, y = 0.37,   # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "More than \n20 Years",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 100, height = 50
                 )
             )
         )

     return(roof_map_plot)
 }

 # Retrieve DT:datatable for an account ----
 fetch_building_permit_DT <- function(account_num){
     building_permits_mapping_tbl <- create_building_permits_mapping_tibble()
     building_permit_DT <- building_permits_mapping_tbl[building_permits_mapping_tbl$account == account_num, ]
     display_table <- building_permit_DT$datatable[[1]]
     return(display_table)
 }


############################ Leaflet Map ##############################
create_context_leaflet_map <- function(zoom_level = 16){

    ## Uses the leaflet and sf libraries
    # Insure the eg_mapping_tbl is available
    if (!exists("eg_mapping_tbl", envir = .GlobalEnv)) {
        eg_mapping_tbl <- read_rds("02_processed_data/eg_mapping_tbl.rds")
    }

    # Calculate centroid
    centroid <- sf::st_centroid(sf::st_union(eg_mapping_tbl))
    centroid_coords <- sf::st_coordinates(centroid)

    # Create leaflet map with a lower zoom level
    leaflet_map <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::setView(lng = centroid_coords[1],
                         lat = centroid_coords[2],
                         zoom = zoom_level) # Adjust zoom level (lower = zoomed out)
    return(leaflet_map)
}

# Example: Fly to a specific point (longitude, latitude)
zoom_to_map_coords <- function(lng = -82.46923, lat = 27.29021, zoom_level = 16){
    target_lng <- lng
    target_lat <- lat
    target_zoom <- zoom_level

    animap <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::flyTo(lng = target_lng, lat = target_lat, zoom = target_zoom)
    return(animap)
}

# zoom_to_map_coords()


############################ Streets Shape File ##############################
which(is.na(sf::read_sf("01_raw_data/Street/Street.shp")[["lifecycles"]])) %>% length()

street_data <- sf::read_sf("01_raw_data/Street/Street.shp")

if(sf::st_crs(street_data) != 4326){
    street_data <- sf::st_transform(street_data, crs = 4326)
}

