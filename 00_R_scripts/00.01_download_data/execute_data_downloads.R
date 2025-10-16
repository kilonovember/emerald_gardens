# Script to download Emerald Gardens data from SCPA

source("00_R_scripts/00.02_transform_data/harvest_scpa_property_info_files.R")

# From harvest_scpa_property_info_files.R
#
# The file retrieved holds information for every real estate parcel in
# Sarasota County. Must be filtered by subdivision ID to extract records
# concerning Emerald Gardens.
#
# This function returns no object. It is called only for its side-effect.
# A closer examination of its contents is needed to determine if it
# contains any data not captured elsewhere. This may be deleted if not.
# fetch_scpa_public_xlsx()
# 2025/03/05 - Mark for removal
#   A search of all R scripts found no reference to the output file, 01_raw_data/SCPA_Public.xlsx"

# From harvest_scpa_property_info_files.R
#
# Create a tibble of tibbles holding the information for each
# page of Emerald Gardens lots from the SCPA site
## Save tibble object to file
write_rds(make_eg_all_accounts_tbl(
                base_url = base_url_str,
                page_sections_xpaths = page_sections_xpaths_list,
                scpa_eg_json_data_url = scpa_eg_json_data_url_str
            ),
          "02_processed_data/property_info_pages_tbl.rds"
         )


# From harvest_scpa_property_info_files.R
#
write_rds(retrieve_and_format_eg_mapping_data(), "02_processed_data/eg_mapping_tbl.rds")

