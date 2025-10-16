### Functions to create interactive ggplotly() maps with custom tool tips.
library(tidyverse)
library(plotly)
library(sf)

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

         # Set custom colors for "Owner Occupied" and "Rental"
         scale_fill_manual(
             values = c("TRUE" = "green", "FALSE" = "orange"),
             labels = c("TRUE" = "Owner Occupied", "FALSE" = "Rental")
         ) +

         # Add invisible points at centroids for tooltips
         geom_sf(data = joined_tbl,
                 aes(geometry = centroid, text = ownership_tool_tip),  # Tooltip mapping
                 color = "transparent",  # Ensures invisibility
                 size = 0.001,
                 alpha = 0) +

         # Set axis labels
         labs(x = "Longitude", y = "Latitude") +

         # Minimal theme
         theme_minimal()

     # Convert to interactive map
     oo_plot <- ggplotly(oo_plot) %>%
         add_annotations(
             x = streets_tbl$lon,
             y = streets_tbl$lat,
             text = paste("<i>", streets_tbl$street_name, "</i>"),
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default tooltips
     oo_plot$x$data[[2]]$hoverinfo <- "none"
     oo_plot$x$data[[3]]$hoverinfo <- "none"

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
                     x = 0.83, y = 0.53,  # Color block position
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
                     x = 0.85, y = 0.48,  # Position the text below the color block
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
                     x = 0.83, y = 0.42,   # Color block position
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
                     x = 0.84, y = 0.38,   # Position the text below the color block
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
     pool_layer_data_tbl <- joined_tbl %>% filter(pool == TRUE)
     no_pool_layer_data_tbl <- joined_tbl %>% filter(pool == FALSE)

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
             text = paste("<i>", streets_tbl$street_name, "</i>"),
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default tooltips
     pool_plot$x$data[[2]]$hoverinfo <- "none"
     pool_plot$x$data[[3]]$hoverinfo <- "none"

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
                     x = 0.83, y = 0.50,  # Color block position
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
                     x = 0.845, y = 0.46,  # Position the text below the color block
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
                     x = 0.83, y = 0.42,   # Color block position
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
                     x = 0.845, y = 0.38,   # Position the text below the color block
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
# Begin map creation section

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
             text = paste("<i>", streets_tbl$street_name, "</i>"),
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default tooltips
     recent_sale_plot$x$data[[2]]$hoverinfo <- "none"
     recent_sale_plot$x$data[[3]]$hoverinfo <- "none"
     recent_sale_plot$x$data[[4]]$hoverinfo <- "none"
     recent_sale_plot$x$data[[5]]$hoverinfo <- "none"

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
                     x = 0.85, y = 0.75,
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
                     x = 0.83, y = 0.70,  # Color block position
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
                     x = 0.845, y = 0.64,  # Position the text below the color block
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
                     x = 0.83, y = 0.58,  # Color block position
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
                     x = 0.851, y = 0.54,  # Position the text below the color block
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
                     x = 0.83, y = 0.48,  # Color block position
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
                     x = 0.853, y = 0.44,  # Position the text below the color block
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
                     x = 0.83, y = 0.38,  # Color block position
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
                     x = 0.855, y = 0.30,  # Position the text below the color block
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
                        eff_yr_built <= 2000 ~ "Pre 2000",
                        eff_yr_built > 2000 & eff_yr_built <= 2005 ~ "2000 to 2005",
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
                                              "2000 to 2005",
                                              "Pre 2000"
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
             "2000 to 2005" = "lightblue",
             "Pre 2000" = "lightgray"
         ),
         labels = c("After 2020" = "After 2020",
                    "2016 to 2020" = "2016 to 2020",
                    "2011 to 2015" = "2011 to 2015",
                    "2006 to 2010" = "2011 to 2015",
                    "2000 to 2005" = "2000 to 2005",
                    "Pre 2000" = "Pre 2000")
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
             text = paste("<i>", streets_tbl$street_name, "</i>"),
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )

     # Remove default tooltips
     effective_year_built_plot$x$data[[2]]$hoverinfo <- "none"
     effective_year_built_plot$x$data[[3]]$hoverinfo <- "none"
     effective_year_built_plot$x$data[[4]]$hoverinfo <- "none"
     effective_year_built_plot$x$data[[5]]$hoverinfo <- "none"
     effective_year_built_plot$x$data[[6]]$hoverinfo <- "none"


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
                     x = 0.86, y = 0.78,
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
                     x = 0.83, y = 0.70,  # Color block position
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
                     x = 0.848, y = 0.64,  # Position the text below the color block
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
                     x = 0.83, y = 0.60,  # Color block position
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
                     x = 0.848, y = 0.56,  # Position the text below the color block
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
                     x = 0.83, y = 0.52,  # Color block position
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
                     x = 0.848, y = 0.48,  # Position the text below the color block
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
                     x = 0.83, y = 0.44,  # Color block position
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
                     x = 0.848, y = 0.40,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "2006 to 2010",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 20
                 ),
                 # "2000 to 2005" legend item with text below
                 list(
                     x = 0.83, y = 0.36,  # Color block position
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
                 # Text below "2000 to 2005" block
                 list(
                     x = 0.848, y = 0.31,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "2000 to 2005",
                     showarrow = FALSE,
                     font = list(size = 12),
                     align = "center",
                     bgcolor = "transparent",
                     bordercolor = "transparent",
                     width = 80, height = 20
                 ),
                 # "Pre 2000" legend item with text below
                 list(
                     x = 0.83, y = 0.25,  # Color block position
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
                 # Text below "Pre 2000" block
                 list(
                     x = 0.848, y = 0.22,  # Position the text below the color block
                     xref = "paper", yref = "paper",
                     text = "Pre 2000",
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
 make_interactive_assessed_per_sqft_map <- function(){
     data_tbl <- joined_tbl %>%
         mutate(assessed_per_sqft = assessed_value_of_bldg/grnd_area)

     # Create base map
     assessed_per_sqft_plot <- ggplot() +
         # Add color
         geom_sf(data = data_tbl, aes(fill = assessed_per_sqft), color = "black", size = 0.1) +
         scale_fill_gradient(
             low = "snow2",
             high = "steelblue2",
             name = "Assessed Value per Sq.Ft.\nBuilding Area",
             labels = scales::label_dollar()
         ) +
         # Add invisible points at centroids for tooltips
         geom_sf(data = joined_tbl,
                 aes(geometry = centroid, text = assessed_value_pr_sqft_tooltip),  # Tooltip mapping
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

     # Convert static plot to interactive plot
     assessed_per_sqft_plot <- assessed_per_sqft_plot %>%
         ggplotly() %>%

         add_annotations(
             x = streets_tbl$lon,
             y = streets_tbl$lat,
             text = paste("<i>", streets_tbl$street_name, "</i>"),
             showarrow = FALSE,
             textangle = streets_tbl$angle
         )  %>%

     # Set shape line widths to 1
         # This plot is different from the others in that the legend and shapes
         # are colored on a continuous scale rather than on a discrete one.
         # As a consequence each shape has its own layer. Rather than include
         # 127 assessed_per_sqft_plot$x$data[[n]]$line$width <- 1 statements,
         # I've used this code block to change them all at once.
         {
             .$x$data <- map(.$x$data, function(layer) {
                 if (!is.null(layer$line$width)) {
                     layer$line$width <- 1
                 }
                 layer
             })
             .
         } %>%

         # Similar construct to remove unwanted tooltips. What is different
         # in this plot is the continuous scale. This caused the tooltip
         # values to be stored in the $hovertext property of layer 130 and
         # this code chunk removes the default tooltips while retaining the custom tooltips.
         {
             .$x$data <- imap(.$x$data, function(layer, i) {
                 # Remove unwanted tooltips from shape layers (2 to 128)
                 if (i >= 2 && i <= 128 && !is.null(layer$text)) {
                     layer$text <- NULL  # Clear the unwanted tooltips
                 }
                 # Ensure hovertext remains in layer 130
                 if (i == 130 && !is.null(layer$hovertext)) {
                     layer$hovertext <- layer$hovertext  # Keep the custom hovertext
                 }
                 layer
             })
             .
         }

     return(assessed_per_sqft_plot)
 }

