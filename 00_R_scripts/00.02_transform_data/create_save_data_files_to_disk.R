# Create data files from functions
# NOTES:


# Create and save list of XPaths for SCPA Property Record Information page sections ----
page_sections_xpaths_list <- list(
    whole_page = x_path_property_info_page <- "//span[starts-with(text(), 'Property Record Information for ')]",
    ownership_block = x_path_ownership_block <- "//body//ul[@class='resultl spaced']//li/text()",
    property_description_block = x_path_property_description_block <-"//body//ul[@class='resultr spaced']//li/text()",
    buildings_block = x_path_buildings_block <- "//body//table[@id = 'Buildings']",
    extra_features_block = x_path_extra_features_block <-  "//body//table[@id = 'Buildings']/following-sibling::table[@class = 'grid'][1]",
    valuation_history_block = x_path_valuation_history_block <- "//body//span[text()='Values']/following-sibling::table[1]",
    exemptions_block = x_path_exemptions_block <- "//body//div[@id = 'exemptions']",
    sales_block = x_path_sales_block <- "//body//table[@id = 'Buildings']/following-sibling::table[@class = 'grid'][2]",
    FEMA_flood_zone_block = x_path_FEMA_flood_zone_block <- "//body//div[@class = 'flood']",
    transfer_documents_link = x_path_transfer_documents_link <- "//body//div[@id = 'container']//table[@class = 'grid']//tbody//tr[contains(@class, 'gridrow') or contains(@class, 'gridrow_alternate')]/td/a[contains(@title, 'Sarasota Clerk of the Court')]/@href"
)
write_rds(page_sections_xpaths_list, "00_R_scripts/00.02_transform_data/page_sections_xpaths_list.rds")

# Create eg_mapping_tbl.rds----
# from harvest_scpa_property_info_files.R
eg_mapping_tbl <- retrieve_and_format_eg_mapping_data()
write_rds(x = eg_mapping_tbl, file = "02_processed_data/eg_mapping_tbl.rds")

# Create property_info_pages_tbl.rds ----
# from harvest_scpa_property_info.R
property_info_pages_tbl <- make_eg_all_accounts_tbl()
write_rds(x = property_info_pages_tbl, file = "02_processed_data/property_info_pages_tbl.rds")

# Create tool_tips_tbl.rds ----
# from tooltip_creation_functions.R
tool_tips_tbl <- make_combined_tool_tip_tbl() %>% relocate(geometry, .after = everything())
write_rds(x = tool_tips_tbl, file = "02_processed_data/tool_tips_tbl.rds")

# Create tibble for display in Property Details tab
gt_table_tbl <- prepare_tibble_for_GT_Table()
write_rds(gt_table_tbl, "02_processed_data/eg_gt_ssf.rds")
