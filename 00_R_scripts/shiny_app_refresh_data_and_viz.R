# SHINY APP DATA AND VISUALIZATIONS REFRESH SCRIPT
# This script will gather and transform the data, saving to to files,
# and create maps and plot visualizations used in the Shiny app.

# Download data from SCPA site and write to files
# Imports: 00_R_scripts/00.02_transform_data/harvest_scpa_property_info_files.R
source("00_R_scripts/00.01_download_data/execute_data_downloads.R")
# Creates:
#   1) 02_processed_data/property_info_pages_tbl.rds
#   2) 02_processed_data/eg_mapping_tbl.rds

# Download data from building permits site and create tibbles
# Imports: no imports
source("00_R_scripts/00.01_download_data/harvest_building_permit_data.R")


# Refresh DT table used in Property Details tab
# Imports: 02_processed_data/property_info_pages_tbl.rds
source("00_R_scripts/00.02_transform_data/prep_tibble_for_GT_Table.R")
write_rds(prepare_tibble_for_GT_Table(), "02_processed_data/eg_DT_ssf.rds")
# Creates: 02_processed_data/eg_DT_ssf.rds

# Refresh tooltips.
# Imports:
#   1) 02_processed_data/property_info_pages_tbl.rds
#   2) 02_processed_data/eg_mapping_tbl.rds
source("00_R_scripts/00.02_transform_data/tooltip_creation_functions.R")
write_rds(make_combined_tool_tip_tbl(), "02_processed_data/tool_tips_tbl.rds")
# Creates: 02_processed_data/tool_tips_tbl.rds

# Refresh maps
# Imports:
#   1) 00_R_scripts/00.03_map_creation_functions/map_creation_functions.R
#   2) 02_processed_data/property_info_pages_tbl.rds
#   3) 02_processed_data/eg_mapping_tbl.rds
#   4) 02_processed_data/streets_tbl.rds
#   5) 02_processed_data/tool_tips_tbl.rds
source("00_R_scripts/00.03_map_creation_functions/create_and_save_maps.R")
# Creates:
#   1) 03_plot_files/interactive_owner_occupied_map.rds
#   2) 03_plot_files/interactive_pool_map.rds
#   3) 03_plot_files/interactive_last_sale_map.rds
#   4) 03_plot_files/interactive_effective_year_built_map.rds
#   5) 03_plot_files/interactive_assessed_per_sqft_map.rds
#   6) 03_plot_files/interactive_sales_by_year_plot.rds

# Refresh pop-up tables
# Pop-up tables are displayed in modal windows when a lot shape in one of the maps is clicked.
# This script creates one tibble of pop-up table content for each map.
# Each tibble creates one row of pop-up table content for lot shape.

# Imports:
#   1) 02_processed_data/property_info_pages_tbl.rds
#   2) 02_processed_data/page_sections_xpaths_list.rds
#   3) 02_processed_data/eg_mapping_tbl.rds
# source("00_R_scripts/00.02_transform_data/pop_up_tables.R")
# Creates: This script writes no files to disk. Pop-up tables are created on-demand
# in the Shiny app when a user clicks on a shape in a map.
# While it doesn't refresh any files, it's included here for completeness.

# Refresh data plots
# Imports:
#   1) 02_processed_data/property_info_pages_tbl.rds
#   2) 00_R_scripts/00.03_map_creation_functions/misc_data_plots.R
source("00_R_scripts/00.03_map_creation_functions/create_and_save_plots.R")
# Creates:
#   1) 03_plot_files/model_dot_plot.rds
#   2) 03_plot_files/q-q_residuals_plot.rds
#   3) 03_plot_files/residuals_scatter_plot.rds
#   4) 03_plot_files/interactive_sales_by_year_plot.rds
