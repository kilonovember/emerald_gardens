# Script to create the tables for display in the Property Details tab

# NOTES:
# Load these via a source() statment and include the calls to the
# functions in the server section of the shiny app

library(dplyr)
library(gt)
library(htmltools)

return_property_description_table <- function(account_number){
    table <- property_info_pages_tbl[property_info_pages_tbl$account == "0069020067",] %>%
        select(property_description_block) %>%
        pluck(1) %>%
        pluck(1) %>%
        select(c(parcel_description, lot_size, zoning, sec_twp_rng, census)) %>%
        rename(
            .data = .,
            `Parcel Description` = parcel_description,
            `Lot Size` = lot_size,
            Zoning = zoning,
            `Section-Township-Range` = sec_twp_rng,
            `Census Tract`= census) %>%
        pivot_longer(everything())
    return(table)
}

return_buildings_info_table <- function(account_number){
    table <- property_info_pages_tbl[property_info_pages_tbl$account == account_number,] %>%
        select(buildings_info) %>%
        pluck(1) %>%
        pluck(1) %>%
        select(c(situs_addr, gross_area, living_area, bedrooms, bathrooms, half_baths, eff_yr_built, year_built)) %>%
        mutate(across(where(is.integer), as.character)) %>%
        mutate(living_area = paste(living_area,"sq.ft.")) %>%
        mutate(gross_area = paste(gross_area, "sq.ft.")) %>%
        rename(
            .data = .,
            `Address` = situs_addr,
            `Gross Building Area` = gross_area,
            `Living Area` = living_area,
            Bedrooms = bedrooms,
            Bathrooms = bathrooms,
            `Half Baths` = half_baths,
            `Effective Year Built` = eff_yr_built,
            `Year Built` = year_built
        ) %>%
        pivot_longer(everything())
    return(table)
}

return_extra_features_table <- function(account_number){
    table <- property_info_pages_tbl[property_info_pages_tbl$account == account_number,] %>%
        select(extra_features) %>%
        pluck(1) %>%
        pluck(1) %>%
        select(-c(row_id, bldg_num, units_measure)) %>%
        mutate(across(where(is.integer), as.character)) %>%
        mutate(units = paste(units, "sq.ft.")) %>%
        rename(
            Description = description,
            Size = units,
            `Year Built` = year
        ) %>%
        gt() %>%
        tab_header(data = ., title = "Extra Features") %>%
        tab_options(table.font.size = px(12))
    return(table)
}

return_assessed_value_history_table <- function(account_number){
    table <- property_info_pages_tbl[property_info_pages_tbl$account == account_number,] %>%
    select(assessed_value_history) %>%
    pluck(1) %>%
    pluck(1) %>%
    rename(
        Year = year,
        Land = land,
        Building = building,
        `Extra Features` = extra_features,
        `Total Estimated Value` = just,
        `Assessed Value` = assessed,
        Exemptions = exemptions,
        `Taxable Value`= taxable,
        `Capped Value` = cap
    ) %>%
        gt() %>%
        tab_header(data = ., title = "Assessment History") %>%
        tab_options(table.font.size = px(12))

    return(table)
}

# Florida real property transfer codes document URL: https://floridarevenue.com/property/Documents/salequalcodes_bef01012019.pdf

# return_transfers_history_table <- function(account_number){
#     table <- property_info_pages_tbl[property_info_pages_tbl$account == account_number,] %>%
#         select(transfers) %>%
#         pluck(1) %>%
#         pluck(1) %>%
#         rename(
#             `Transfer Date` = xfer_date,
#             `Recorded Consideration` = recorded_consideration,
#             `Instrument Number` = instrument_num,
#             `Instrument Type` = instrument_type,
#             `Transfer Type` = qual_code,
#             Seller = seller,
#             `Transfer Document URL` = xfer_docs_url
#         ) %>%
#         gt() %>%
#         tab_header(data = ., title = "Transfers History") %>%
#         tab_style(style = cell_text(align = "center"), locations = cells_column_labels()) %>%
#         tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c("Transfer Type", "Instrument Type"))) %>%
#         text_transform(
#             locations = cells_body(columns = "Instrument Number"),
#             fn = function(x) {
#                 urls <- return_transfers_history_table(account_number = account_number)$`Transfer Document URL`
#                 mapply(function(text, url) {
#                     as.character(htmltools::a(href = url, text, target = "_blank")) # Creates an HTML hyperlink
#                 }, x, urls)
#             }
#         )
#     return(table)
# }

# Transfers History GT
return_transfers_history_table <- function(account_number) {
    # Extract the data for the specified account
    table_data <- property_info_pages_tbl[property_info_pages_tbl$account == account_number,] %>%
        select(transfers) %>%
        pluck(1) %>%
        pluck(1) %>%
        rename(
            `Transfer Date` = xfer_date,
            `Recorded Consideration` = recorded_consideration,
            `Instrument Number` = instrument_num,
            `Instrument Type` = instrument_type,
            `Transfer Type` = qual_code,
            Seller = seller,
            `Transfer Document URL` = xfer_docs_url
        )

    # Extract URLs once and store
    urls <- table_data$`Transfer Document URL`

    # Create the gt table
    table <- table_data %>%
        gt() %>%
        tab_header(title = "Transfers History") %>%
        tab_style(style = cell_text(align = "center"), locations = cells_column_labels()) %>%
        tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c("Transfer Type", "Instrument Type"))) %>%
        text_transform(
            locations = cells_body(columns = "Instrument Number"),
            fn = function(x) {
                # Map each instrument number to its corresponding URL as a hyperlink
                mapply(function(text, url) {
                    as.character(htmltools::a(href = url, text, target = "_blank"))
                }, x, urls)
            }
        ) %>%
        cols_hide(columns = "Transfer Document URL") %>% # Hide the Transfer Document URL column
        tab_options(table.font.size = px(12))
    return(table)
}

# Combined Buildings Information and Property Information GT
combined_buildings_property_info <- function(account_number){
    table <- bind_rows(
    return_buildings_info_table(account_number),
    return_property_description_table(account_number)
    ) %>%
        gt() %>%
        tab_header(data = ., title = "Property Description") %>%
        cols_label(name = "", value = "") %>%
        tab_options(table.font.size = px(12))
    return(table)
}

# Tests
# return_assessed_value_history_table("0069020036")
# combined_buildings_property_info("0069020036")
# return_transfers_history_table("0069020032")
# return_extra_features_table("0069020036")

