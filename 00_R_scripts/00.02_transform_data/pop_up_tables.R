# Functions to create Emerald Gardens maps pop-up modal windows

# Load libraries (if not already loaded) ----
if(!"tidyverse" %in% (.packages())){library(tidyverse)}
if(!"gt" %in% (.packages())){library(gt)}
if(!"DT" %in% (.packages())){library(DT)}

# Load property_info_pages_tbl, a tibble of tibbles with each column holding
# a tibble containing the information from a section of the SCPA Property Record Information Page
# for an Emerald Gardens property. Each row contains all the tibbles for a single property
# while the property_info_pages_tbl's rows encompass all properties.
#
# This file is created when the execute_data_downloads.R script is executed.
# The execute_data_downloads.R script takes minutes to execute and should be
# part of the site maintenance process and not included in the Shiny app.
# That is why we read the property_info_pages_tbl from a file and not by
# executing the call to the script that harvests the data from the SCPA web site.

# Load data
property_info_pages_tbl <- read_rds("02_processed_data/property_info_pages_tbl.rds")
page_sections_xpaths_list <- read_rds("02_processed_data/page_sections_xpaths_list.rds")

## Retrieve all account numbers
all_account_nums <- read_rds("02_processed_data/eg_mapping_tbl.rds")$account

# Function to create a gt table.
# (Without sorting and searching, makes for cleaner presentation when sorting and searching aren't useful.)
# This table contains the sale/transfer history for the property.

# Function to create a transfer history table ----
make_gt_xfer_history_tbl <- function(
        account_num,
        property_info_pages = property_info_pages_tbl,
        page_sections_xpaths = page_sections_xpaths_list){

        property_xfers_tbl <- property_info_pages[property_info_pages$account == account_num, "transfers"][[1]][[1]]
        gt_property_xfers_tbl <- property_xfers_tbl %>%
               rename(
                   `Transfer Date` = xfer_date,
                   `Recorded Consideration` = recorded_consideration,
                   `Instrument Num` = instrument_num,
                   `Qual Code` = qual_code,
                   `Seller` = seller,
                   `Instrument Type` = instrument_type
               ) %>%
               mutate(`Instrument Num` = sprintf(
                   '<a href="%s" target="_blank">%s</a>',
                   xfer_docs_url,
                   `Instrument Num`
                )
               ) %>%
               select(-xfer_docs_url) %>%
               gt::gt() %>%
               gt::tab_header(property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                                      2][[1]][[1]]$situs_addr %>%
                                  str_remove(" SARASOTA, FL, 34233")) %>%
               gt::fmt_markdown(columns = `Instrument Num`) %>%

               gt::tab_style(
                   style = gt::cell_text(align = "center"),
                   locations = gt::cells_column_labels(
                       columns = everything()
                   )
               ) %>%
               gt::cols_align(
                   align = "center",
                   columns = c(`Qual Code`, `Instrument Num`, `Instrument Type`)
               ) %>%
               gt::cols_align(
                   align = "right",
                   columns = c(`Recorded Consideration`, `Instrument Num`)
               ) %>%
                # Set the font size for the table
               gt::tab_style(
                   style = list(
                                gt::cell_text(size = px(12))  # Set the font size to 14px (adjust as needed)
                            ),
                            locations = gt::cells_body()  # Apply to all table cells
                )

    return(gt_property_xfers_tbl)
}

# Function to create a valuation history table ----
make_gt_valuation_history_tbl <- function(
        account_num,
        property_info_pages = property_info_pages_tbl){
    valuation_history_tbl <- property_info_pages[property_info_pages$account == account_num, "assessed_value_history"][[1]][[1]]

    gt_valuation_history_tbl <- valuation_history_tbl %>%
        rename(
            Year = year,
            Land = land,
            Building = building,
            `Extra Features` = extra_features,
            Just = just,
            Assessed = assessed,
            Exemptions = exemptions,
            Taxable = taxable,
            Cap = cap
        ) %>%
        gt::gt() %>%
        gt::tab_header(property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                               2][[1]][[1]]$situs_addr %>%
                           str_remove(" SARASOTA, FL, 34233")) %>%
        gt::tab_style(
            style = gt::cell_text(align = "center"),
            locations = gt::cells_column_labels(
                columns = everything()
            )
        ) %>%
        # Set the font size for the table
        gt::tab_style(
            style = list(
                gt::cell_text(size = px(12))  # Set the font size to 14px (adjust as needed)
            ),
            locations = gt::cells_body()  # Apply to all table cells
        )

    return(gt_valuation_history_tbl)
}

# Function to create a buildings details table ----
make_buildings_details_tbl <- function(
        account_num,
        property_info_pages = property_info_pages_tbl) {
    buildings_details <- property_info_pages[property_info_pages$account == account_num, "buildings_info"][[1]][[1]]

    gt_buildings_details_tbl <- buildings_details %>%
        rename(
            `Street Address` = situs_addr,
            `Building Num` = bldg_num,
            Bedrooms = bedrooms,
            Bathrooms = bathrooms,
            `Half Baths` = half_baths,
            `Year Built` = year_built,
            `Effective Year Built` = eff_yr_built,
            `Gross Area` = gross_area,
            `Living Area` = living_area,
            Stories = stories
        ) %>%
        mutate(across(where(is.numeric), as.character)) %>%
        # pivot_longer() transposes columns to rows.
        # As some of the values in the tibble are numbers
        # while others are characters, it's necessary to
        # first transform the numeric values to character values.
        pivot_longer(., cols = everything()) %>%
        gt::gt() %>%
        gt::tab_header(property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                               2][[1]][[1]]$situs_addr %>%
                           str_remove(" SARASOTA, FL, 34233")) %>%
        # Remove column headings as they add no value here.
        gt::cols_label(
            name = "",
            value = ""
        ) %>%
        # Right align numeric columns
        gt::tab_style(
            style = gt::cell_text(align = "right"),
            locations = gt::cells_body(
                columns = 2
            )
        ) %>%
        # Set the font size for the table
        gt::tab_style(
            style = list(
                gt::cell_text(size = px(12))  # Set the font size to 14px (adjust as needed)
            ),
            locations = gt::cells_body()  # Apply to all table cells
        )


    return(gt_buildings_details_tbl)
}

# Function to create an ownership table ----
make_ownership_table <- function(
        account_num,
        property_info_pages = property_info_pages_tbl){

        ownership_table <- property_info_pages[property_info_pages$account == account_num, "ownership_block"][[1]][[1]]

    if(is_logical(ownership_table$owner_2)){
        ownership_table <- ownership_table %>%
            mutate(owner_2 = as.character(owner_2))
    }

    if(is_logical(ownership_table$owner_3)){
        ownership_table <- ownership_table %>%
            mutate(owner_3 = as.character(owner_3))
    }

    ownership_table <- ownership_table %>%
        rename(
            `Owner 1` = owner_1,
            `Owner 2` = owner_2,
            `Owner 3` = owner_3,
            `Mailing Address` = mailing_addr,
            `Property Address` = situs_addr
        )

    gt_ownership_table <- ownership_table %>%
        pivot_longer(., cols = everything()) %>%
        filter(value != "NA") %>%
        gt::gt() %>%
        gt::tab_header(property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                               2][[1]][[1]]$situs_addr %>%
                           str_remove(" SARASOTA, FL, 34233")) %>%
        # Remove column headings as they add no value here.
        gt::cols_label(
            name = "",
            value = ""
        ) %>%
        # Set the font size for the table
        gt::tab_style(
            style = list(
                gt::cell_text(size = px(12))  # Set the font size to 14px (adjust as needed)
            ),
            locations = gt::cells_body()  # Apply to all table cells
        )

    return(gt_ownership_table)
}

# Function to create an extra features table ----
make_extra_features_table <- function(account_num,
                                      property_info_pages = property_info_pages_tbl
                                      ){
    extra_features_table <- property_info_pages_tbl[property_info_pages_tbl$account == account_num, "extra_features"][[1]][[1]]
    if(extra_features_table$row_id[1] == "There are no extra features associated with this parcel"){
        extra_features_table <- extra_features_table %>%
            rename(" " = row_id) %>%
            gt::gt() %>%
            gt::tab_header(property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                                   2][[1]][[1]]$situs_addr %>%
                               str_remove(" SARASOTA, FL, 34233")) %>%
        # Set the font size for the table
        gt::tab_style(
            style = list(
                gt::cell_text(size = px(12))  # Set the font size to 14px (adjust as needed)
            ),
            locations = gt::cells_body()  # Apply to all table cells
        )

        return(extra_features_table)
    }else{
        extra_features_table <- extra_features_table %>%
            select(c(description, units, year)) %>%
            mutate(units = paste(as.character(units), "sq.ft.")) %>%
            rename(
                Feature = description,
                Size = units,
                `Year built` = year
            ) %>%
            gt::gt() %>%
            gt::tab_header(property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                                   2][[1]][[1]]$situs_addr %>%
                               str_remove(" SARASOTA, FL, 34233")) %>%
            # Set the font size for the table
            gt::tab_style(
                style = list(
                    gt::cell_text(size = px(12))  # Set the font size to 14px (adjust as needed)
                ),
                locations = gt::cells_body()  # Apply to all table cells
            )

        gt_extra_features_table <- extra_features_table

        return(gt_extra_features_table)
    }
}

# Function to create building permits pop-up table ----
make_building_permits_tbl <- function(
        account_num,
        .building_permits_tbl = building_permits_tbl){

    .building_permits_tbl <- .building_permits_tbl %>%
        rename(`Permit Date` = permit_date)

        if(
        .building_permits_tbl[.building_permits_tbl$account == account_num,] %>% nrow() == 0){
            message_tbl <- tibble(message = "No permits on file for this address.") %>%
                gt() %>%
                gt::tab_header(property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                                       2][[1]][[1]]$situs_addr %>%
                                   str_remove(" SARASOTA, FL, 34233")) %>%
                cols_label(message = "")
        }else{
        message_tbl <- .building_permits_tbl[.building_permits_tbl$account == account_num,] %>%
            select(-account, -Address, -Status) %>%
            arrange(desc(`Permit Date`)) %>%
            gt() %>%
            gt::tab_header(property_info_pages_tbl[property_info_pages_tbl$account == account_num,
                                                   2][[1]][[1]]$situs_addr %>%
                               str_remove(" SARASOTA, FL, 34233"))
        }
    return(message_tbl)
}
