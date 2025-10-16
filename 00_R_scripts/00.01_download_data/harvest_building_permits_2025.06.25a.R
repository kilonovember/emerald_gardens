# Harvest Building Permits 2025.06.25a

# Sarasota Building Permits
# P8)LmB?Jxurp%s
# cknell

library(tidyverse)
library(RSelenium)
library(rvest)
library(httr)

# --- Global Variables and Configuration ---
# 'remDr' will be assigned globally after starting the Selenium session.
remDr <- NULL

# URLs
login_page_url <- "https://aca-prod.accela.com/SARASOTACO/Login.aspx"
search_page_url <- "https://aca-prod.accela.com/SARASOTACO/Cap/CapHome.aspx?module=Building&TabName=Home"

# XPaths for all elements
XP_user_name_input <- "//*[@id='username']"
XP_password_input <- "//*[@id='passwordRequired']"
XP_login_button <- "//button[@class='p-element ACA_Button p-button p-component']"
XP_download_button <- "//a[@id='ctl00_PlaceHolderMain_dgvPermitList_gdvPermitList_gdvPermitListtop4btnExport']"
XP_street_num_low_input <- "//input[@id='ctl00_PlaceHolderMain_generalSearchForm_txtGSNumber_ChildControl0']"
XP_street_num_high_input <- "//input[@id='ctl00_PlaceHolderMain_generalSearchForm_txtGSNumber_ChildControl1']"
XP_street_name_input <- "//input[@id='ctl00_PlaceHolderMain_generalSearchForm_txtGSStreetName']"
XP_street_type_select <- "//select[@id='ctl00_PlaceHolderMain_generalSearchForm_ddlGSStreetSuffix']"
XP_ZIP_input <- "//input[@id='ctl00_PlaceHolderMain_generalSearchForm_txtGSAppZipSearchPermit']"
XP_clear_button <- "//a[@id='ctl00_PlaceHolderMain_btnResetSearch']"
XP_lookup_button <- "//a[@id='ctl00_PlaceHolderMain_btnNewSearch']"
XP_select_all_checkbox <- "//input[@id='ctl00_PlaceHolderMain_dgvPermitList_gdvPermitList_HeaderButton_0']"

# Credentials
username <- "cknell"
password <- "P8)LmB?Jxurp%s"

# --- Helper Functions ---

# `safe_find_element` attempts to find an element, retrying if necessary.
# 'verbose = FALSE' by default to suppress "Attempt X" messages during normal operation.
safe_find_element <- function(client, xpath, element_name, attempts = 10, wait_time = 1, verbose = FALSE) {
    element <- NULL
    found <- FALSE
    for (i in 1:attempts) {
        elements <- client$findElements(using = "xpath", value = xpath)
        if (length(elements) > 0) {
            element <- elements[[1]]
            found <- TRUE
            break
        }
        if (verbose) { # Only print retry messages if verbose is TRUE
            cat(paste0("Attempt ", i, ": ", element_name, " element not found. Waiting ",
                       wait_time, " second...\n"))
        }
        Sys.sleep(wait_time)
    }

    if (found) {
        cat(paste0(element_name, " element found successfully after ", i, " attempts.\n"))
        return(element)
    } else {
        stop(paste("Error:", element_name, "element", xpath, "not found after", attempts, "attempts. Aborting."))
    }
}

# `wait_for_element_ready` waits until an element is both displayed and enabled.
# 'verbose = FALSE' by default to suppress "Debug" messages during normal operation.
wait_for_element_ready <- function(client, xpath_value, timeout = 15, poll_freq = 0.5, verbose = FALSE) {
    start_time <- Sys.time()
    while (Sys.time() - start_time < timeout) {
        elements <- client$findElements(using = "xpath", value = xpath_value)
        if (length(elements) > 0) {
            element <- elements[[1]]
            tryCatch({
                if (element$isDisplayed()[[1]] && element$isEnabled()[[1]]) {
                    return(element)
                }
            }, error = function(e) {
                if (verbose) { # Only print debug messages if verbose is TRUE
                    message(paste("Debug: isDisplayed/isEnabled check failed for element. Retrying. Error:",
                                  e$message))
                }
            })
        }
        Sys.sleep(poll_freq) # Wait before the next retry
    }
    stop(paste("Element not visible/enabled within timeout:", xpath_value))
}

# `fill_and_verify_input` fills a text input, with a fallback to JavaScript.
fill_and_verify_input <- function(element, value, element_name, use_js_fallback = TRUE) {
    # Get the tag name of the element
    tag_name <- element$getElementTagName()[[1]]

    # Only attempt to clear the element if it's not a 'select' (dropdown)
    # or if it's an editable text input type.
    if (tag_name != "select") {
        element$clearElement()
    }

    element$sendKeysToElement(list(value))

    # Verification logic: For select elements, sendKeysToElement usually works
    # by sending the visible text of the option.
    # For other inputs, we check the actual value.
    current_value <- element$getElementAttribute("value")[[1]]

    # The verification for 'select' elements is less straightforward via 'value' attribute
    # alone as 'value' might be different from the displayed text.
    # For simplicity, we'll assume sendKeysToElement is reliable for select.
    if (tag_name == "select" || current_value == value) {
        cat(paste0(element_name, " successfully entered/selected.\n"))
    } else if (use_js_fallback) {
        cat(paste0(element_name, " entry failed. Trying JavaScript method...\n"))
        element_id <- element$getElementAttribute("id")[[1]]
        if (!is.null(element_id) && nzchar(element_id)) {
            remDr$executeScript(
                paste0("document.getElementById('", element_id, "').value = arguments[0];"), list(value)
            )
            cat(paste0(element_name, " entered successfully via JavaScript.\n"))
        } else {
            warning(paste0("Could not use JavaScript fallback for ", element_name, " (no ID found)."))
        }
    } else {
        warning(paste0(element_name, " entry failed and no JavaScript fallback attempted."))
    }
}

# --- Functions for Page Interaction ---

# Locates all primary search input fields and buttons.
# Uses 'verbose = FALSE' for safe_find_element by default.
locate_search_fields <- function(){
    found_elements_list <- list()

    found_elements_list$clear_button <- safe_find_element(remDr, XP_clear_button, "clear_button", verbose = FALSE)
    found_elements_list$lookup_button <- safe_find_element(remDr, XP_lookup_button,
                                                           "lookup_button", verbose = FALSE)
    found_elements_list$street_num_low_input <- safe_find_element(remDr, XP_street_num_low_input,
                                                                  "street_num_low_input", verbose = FALSE)
    found_elements_list$street_num_high_input <- safe_find_element(remDr, XP_street_num_high_input,
                                                                   "street_num_high_input", verbose = FALSE)
    found_elements_list$street_name_input <- safe_find_element(remDr, XP_street_name_input, "street_name_input",
                                                               verbose = FALSE)
    found_elements_list$street_type_input <- safe_find_element(remDr, XP_street_type_select,
                                                               "street_suffix_select", verbose = FALSE)
    found_elements_list$ZIP_input <- safe_find_element(remDr, XP_ZIP_input, "ZIP_input", verbose = FALSE)

    return(found_elements_list)
}

# Fills in the street-related search fields based on a key.
fill_in_search_fields <- function(street_name_key){
    search_fields <- locate_search_fields()

    # Define search data as a list of lists for cleaner access
    search_data <- list(
        diamond = list(
            street_name = "Diamond",
            street_num_low = "4401",
            street_num_high = "4513",
            street_type = "Cir",
            zip_code = "34233"
        ),
        topaz = list(
            street_name = "Topaz",
            street_num_low = "4512",
            street_num_high = "4591",
            street_type = "Ct",
            zip_code = "34233"
        ),
        opal = list(
            street_name = "Opal",
            street_num_low = "4415",
            street_num_high = "4497",
            street_type = "Ct",
            zip_code = "34233"
        )
    )

    fields_to_fill <- search_data[[tolower(street_name_key)]]

    if (is.null(fields_to_fill)) {
        warning(paste("No search data found for street name:", street_name_key))
        return(FALSE)
    }

    fill_and_verify_input(search_fields$street_name_input, fields_to_fill$street_name, "Street Name")
    fill_and_verify_input(search_fields$street_num_low_input, fields_to_fill$street_num_low, "Street Number Low")
    fill_and_verify_input(search_fields$street_num_high_input, fields_to_fill$street_num_high,
                          "Street Number High")
    fill_and_verify_input(search_fields$street_type_input, fields_to_fill$street_type, "Street Type")

    # Return the zip_code needed for the next step, as it's filled separately
    return(fields_to_fill$zip_code)
}

# Performs a full search and data download for a given street name.
perform_search_and_download <- function(street_name_key){
    cat(paste0("\n--- Starting search for ", street_name_key, " ---\n"))

    # Clear fields if this isn't the first search in the session
    if(street_name_key != "diamond") {
        clear_button <- safe_find_element(remDr, XP_clear_button, "clear_button_for_clear", verbose = FALSE)
        clear_button$clickElement()
        Sys.sleep(2) # Give time for clear to register and DOM to reset
    }

    # Fill in street-related fields and get the ZIP code
    zip_code_to_fill <- fill_in_search_fields(street_name_key)

    if (!is.character(zip_code_to_fill)) { # Check if fill_in_search_fields failed
        return(FALSE) # Abort this specific search
    }

    # --- Handle the ZIP input DOM reset work-around ---
    cat("Filling Street Type input appears to reset the DOM for ZIP. Re-locating and filling ZIP input...\n")
    tryCatch({
        # Use the robust wait_for_element_ready helper for the ZIP input
        # Set verbose = FALSE here to suppress the repeating debug messages
        zip_input_fresh <- wait_for_element_ready(remDr, XP_ZIP_input, timeout = 20, verbose = FALSE)
        zip_input_fresh$clearElement() # Clear any existing mask/placeholder
        Sys.sleep(0.1) # Small pause for element state
        remDr$executeScript("arguments[0].value = arguments[1];",
                            args = list(zip_input_fresh, zip_code_to_fill))
        cat(paste0("ZIP_input filled successfully via JavaScript with ", zip_code_to_fill, ".\n"))
    }, error = function(e) {
        warning(paste("Could not re-locate or fill ZIP_input:", e$message))
        return(FALSE) # Abort this specific search
    })

    # Click the lookup button
    lookup_button <- safe_find_element(remDr, XP_lookup_button, "lookup_button_after_fill", verbose = FALSE)
    lookup_button$clickElement()
    Sys.sleep(5) # Wait for search results to load

    # Locate and click select_all_checkbox
    select_all_checkbox <- safe_find_element(remDr, XP_select_all_checkbox, "select_all_checkbox", verbose = FALSE)
    select_all_checkbox$clickElement()
    Sys.sleep(1) # Small pause after click

    # Locate and click download button
    download_button <- safe_find_element(remDr, XP_download_button, "download_button", verbose = FALSE)
    download_button$clickElement()
    Sys.sleep(5) # Wait for download to initiate

    cat(paste0("--- Successfully completed search and download for ", street_name_key, " ---\n"))
    return(TRUE)
}


# --- Main Script Execution ---

# Kill any existing RSelenium session (frees port 4567L)
cat("Checking for existing Selenium processes...\n")
RSeleniumPID <- system("ps aux | grep selenium-server-standalone | grep -v grep | awk '{print $2}'", intern = TRUE)

if (length(RSeleniumPID) > 0 && RSeleniumPID != "") {
    system(paste("kill -9", paste(RSeleniumPID, collapse = " ")))
    cat("Selenium process terminated.\n")
} else {
    cat("No Selenium process found.\n")
}

# Start RSelenium session
cat("Starting RSelenium session...\n")
rD <- rsDriver(browser = "firefox", port = 4567L, phantomver = NULL)
remDr <<- rD$client # Assign to global remDr variable for functions to access
cat("RSelenium session started.\n")

# Navigate to login page
cat("Navigating to login page...\n")
remDr$navigate(login_page_url)
Sys.sleep(3) # Wait for full page load

# Handle iframe (if necessary)
iframes <- remDr$findElements(using = "tag name", value = "iframe")
if (length(iframes) > 0) {
    remDr$switchToFrame(iframes[[1]])
    cat("Switched to iframe.\n")
}

# Locate and fill login credentials
username_field <- safe_find_element(remDr, XP_user_name_input, "username_field", verbose = FALSE)
password_field <- safe_find_element(remDr, XP_password_input, "password_field", verbose = FALSE)

fill_and_verify_input(username_field, username, "Username", use_js_fallback = TRUE)
fill_and_verify_input(password_field, password, "Password", use_js_fallback = TRUE)

# Locate and click login button
login_button <- safe_find_element(remDr, XP_login_button, "login_button", verbose = FALSE)
tryCatch({
    login_button$clickElement()
    cat("Login button clicked successfully.\n")
}, error = function(e) {
    cat("Failed to click login button directly. Trying JavaScript method...\n")
    # Use the stored XPath for JavaScript click
    remDr$executeScript(
        "document.evaluate(arguments[0], document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click();",
        list(XP_login_button)
    )
    cat("Login button clicked via JavaScript.\n")
})
Sys.sleep(5) # Wait for login redirect/page load

# Navigate to search page (if not automatically redirected)
cat("Navigating to search page...\n")
remDr$navigate(search_page_url)
Sys.sleep(5) # Wait for search page to load

# --- Perform searches and downloads for each street ---
# You can add more street names to this vector as needed.
street_names_to_search <- c("diamond", "topaz", "opal")

for (street in street_names_to_search) {
    success <- perform_search_and_download(street)
    if (!success) {
        warning(paste0("Search and download failed for ", street, ". Continuing with next street."))
        # You might want to add more robust error handling or logging here.
    }
}

# --- End Session ---
cat("\nAll specified searches and downloads completed. Closing Selenium session.\n")
remDr$close()
rD$server$stop() # Stop the RSelenium server


# --- Move harvested files ---
# Identify this session's downloads

building_permit_records_lst <- list.files(path = "/Users/cknell/Downloads/",
                                          pattern = paste0("^", paste0("RecordList", Sys.Date() %>%
                                                                           str_replace_all(., "-", ""))))

# Rename files (based on the order downloaded)
file.rename(paste0("/Users/cknell/Downloads/",
                   building_permit_records_lst[1]),
            "/Users/cknell/Downloads/building_permits_history_accela_Diamond.csv")

file.rename(paste0("/Users/cknell/Downloads/",
                   building_permit_records_lst[2]),
            "/Users/cknell/Downloads/building_permits_history_accela_Topaz.csv")

file.rename(paste0("/Users/cknell/Downloads/",
                   building_permit_records_lst[3]),
            "/Users/cknell/Downloads/building_permits_history_accela_Opal.csv")

# Move the files to the raw data directory
system("mv /Users/cknell/downloads/building_permits_history*.* /Users/cknell/Documents/emerald_gardens/eg.v.5/01_raw_data/")
