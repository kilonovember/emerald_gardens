# Template function for downloading Accela site building permits data.
# Login credentials stored in Firefox.

# Sarasota Building Permits
# P8)LmB?Jxurp%s
# cknell

library(tidyverse)
library(RSelenium)
library(rvest)
library(httr)

######################
# Function to automate login

wait_for_element <- function(remDr, using, value, timeout = 10) {
    for (i in 1:timeout) {
        elements <- remDr$findElements(using = using, value = value)
        if (length(elements) > 0) {
            return(elements[[1]])
        }
        Sys.sleep(1)
    }
    # If the element isn't found, clean up Selenium and stop execution
    cat("Element not found:", value, "\nClosing Selenium session...\n")
    remDr$close()
    stop(paste("Element not found:", value))
}


# Use RSeleniumPID to kill the Selenium process.
# This will be useful when called if a data retrieval operation fails before cleaning up or
# as the last call in a function which has completed data scraping.

kill_RSelenium_instance <- function(){
    # Get the process ID of the Selenium server
    RSeleniumPID <- system("ps aux | grep selenium-server-standalone | grep -v grep | awk '{print $2}'", intern = TRUE)

    # Check if PID is found before attempting to kill
    if (length(RSeleniumPID) > 0 && RSeleniumPID != "") {
        system(paste("kill -9", paste(RSeleniumPID, collapse = " ")))  # Kill all found Selenium processes
        cat("Selenium process terminated.\n")
    } else {
        cat("No Selenium process found.\n")
    }
}

# Function to log in with given username and password
login_sarasota <- function(username, password) {
    # Stop any existing Selenium session first
    kill_RSelenium_instance()

    # Start RSelenium session
    rD <- rsDriver(browser = "firefox", port = 4567L)
    remDr <- rD$client

    remDr$open()

    # Navigate to login page
    remDr$navigate("https://aca-prod.accela.com/SARASOTACO/Login.aspx")

    # Wait for full page load
    Sys.sleep(3)

    # Handle iframe (if necessary)
    iframes <- remDr$findElements(using = "tag name", value = "iframe")
    if (length(iframes) > 0) {
        remDr$switchToFrame(iframes[[1]])
        cat("Switched to iframe.\n")
    }

    # Locate username field
    username_field <- tryCatch(
        wait_for_element(remDr, "xpath", "//*[@id='username']"),
        error = function(e) {
            cat("Error encountered: ", e$message, "\n")
            stop("Login aborted.")
        }
    )
    username_field$clearElement()
    username_field$sendKeysToElement(list(username))

    # Verify username entered
    current_username <- username_field$getElementAttribute("value")[[1]]
    if (current_username == username) {
        cat("Username successfully entered.\n")
    } else {
        cat("Username entry failed. Trying JavaScript method...\n")
        remDr$executeScript(
            "document.getElementById('username').value = arguments[0];", list(username)
        )
    }

    # Locate password field
    password_field <- tryCatch(
        wait_for_element(remDr, "xpath", "//*[@id='passwordRequired']"),
        error = function(e) {
            cat("Error encountered: ", e$message, "\n")
            stop("Login aborted.")
        }
    )
    password_field$clearElement()
    password_field$sendKeysToElement(list(password))

    # Verify password entered
    current_password <- password_field$getElementAttribute("value")[[1]]
    if (!nzchar(current_password)) {
        cat("Password entry failed. Trying JavaScript method...\n")
        remDr$executeScript(
            "document.getElementById('passwordRequired').value = arguments[0];", list(password)
        )
    }

    cat("Username and password entered successfully.\n")

    # Locate the login button using updated XPath
    login_button <- wait_for_element(remDr, "xpath", "//accela-button-primary//button[@class='p-element ACA_Button p-button p-component']")

    # Attempt to click the login button
    tryCatch({
        login_button$clickElement()
        cat("Login button clicked successfully.\n")
    }, error = function(e) {
        cat("Failed to click login button. Trying JavaScript method...\n")
        # Using JavaScript to click the login button if the direct click fails
        remDr$executeScript(
            "document.evaluate(arguments[0], document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click();",
            list("//accela-button-primary//button[@class='p-element ACA_Button p-button p-component']")
        )
    })

    cat("Login process initiated.\n")

    return(remDr)  # Return the Selenium driver object
}


######################

navigate_to_accela_search <- function(RSelenium_instance) {
    # Check if RSelenium instance is active
    if (is.null(RSelenium_instance)) {
        stop("RSelenium instance is NULL. Ensure login_sarasota() was successful.")
    }

    # Get the current URL
    current_url <- RSelenium_instance$getCurrentUrl()[[1]]
    if (current_url == "https://aca-prod.accela.com/SARASOTACO/Dashboard.aspx#") {
        cat("Currently on Dashboard page. Navigating to Accela search page...\n")
    } else {
        cat("Unexpected current URL:", current_url, "\n")
        cat("Proceeding to navigate anyway...\n")
    }

    # Navigate to the desired page
    RSelenium_instance$navigate("https://aca-prod.accela.com/SARASOTACO/Cap/CapHome.aspx?module=Building&TabName=Home")

    # Wait to ensure the page loads properly
    Sys.sleep(3)

    # Confirm navigation
    new_url <- RSelenium_instance$getCurrentUrl()[[1]]
    if (new_url == "https://aca-prod.accela.com/SARASOTACO/Cap/CapHome.aspx?module=Building&TabName=Home") {
        cat("Successfully navigated to Default page.\n")
    } else {
        cat("Navigation may have failed. Current URL:", new_url, "\n")
    }
}

# Function to collect data given an active Selenium object

# collect_data <- function(RSObject, street){
#     # Click the 'Clear' button
#     RSObject$findElement(
#         using = "id",
#         value = "ctl00_PlaceHolderMain_btnResetSearch"
#     )$clickElement()
#
#     Sys.sleep(2)  # Allow clearing
#
#     GSCity <- "Sarasota"
#     GSState <- "FL"
#     GSZip <- "34232"
#
#     if(street == "diamond"){
#         GSFromNo <- "4401"
#         GSToNo  <- "4513"
#         GSStreetName <- "Diamond"
#         GSStreetSuffix <- "Cir"
#     }else if(street == "topaz"){
#         GSFromNo <- "4512"
#         GSToNo <- "4591"
#         GSStreetName <- "Topaz"
#         GSStreetSuffix <- "Ct"
#     }else if(street == "opal"){
#         GSFromNo <- "4415"
#         GSToNo <- "4497"
#         GSStreetName <- "Opal"
#         GSStreetSuffix <- "Ct"
#     }else{
#
#     }
#     # Fill in the address fields
# RSObject$findElement(
#         using = "id",
#         "ctl00_PlaceHolderMain_generalSearchForm_txtGSNumber_ChildControl0")$sendKeysToElement(list(GSFromNo))
#
# # RSObject$executeScript(
# #     paste0(
# #         "var low_address_num = document.getElementById('ctl00_PlaceHolderMain_generalSearchForm_txtGSNumber_ChildControl0');",
# #         "low_address_num.value = '", GSFromNo, "';",
# #         "low_address_num.dispatchEvent(new Event('input',  { bubbles: true }));",
# #         "low_address_num.dispatchEvent(new Event('change',  { bubbles: true }));",
# #         "low_address_num.dispatchEvent(new Event('blur',  { bubbles: true }));"
# #     )
# # )
#
# RSObject$findElement(
#     using = "id",
#     value = "ctl00_PlaceHolderMain_generalSearchForm_txtGSNumber_ChildControl1")$sendKeysToElement(list(GSToNo))  # Adjust as needed
#
# # RSObject$executeScript(
# #     paste0(
# #         "var high_address_num = document.getElementById('ctl00_PlaceHolderMain_generalSearchForm_txtGSNumber_ChildControl1');",
# #         "high_address_num.value = '", GSToNo, "';",
# #         "high_address_num.dispatchEvent(new Event('input',  { bubbles: true }));",
# #         "high_address_num.dispatchEvent(new Event('change',  { bubbles: true }));",
# #         "high_address_num.dispatchEvent(new Event('blur',  { bubbles: true }));"
# #     )
# # )
#
# # RSObject$findElement(
# #         using = "id",
# #         "ctl00_PlaceHolderMain_generalSearchForm_txtGSStreetName")$sendKeysToElement(list(GSStreetName))
#
# RSObject$executeScript(
#     paste0(
#         "var street_name = document.getElementById('ctl00_PlaceHolderMain_generalSearchForm_txtGSStreetName');",
#         "street_name.value = '", GSStreetName, "';",
#         "street_name.dispatchEvent(new Event('input',  { bubbles: true }));",
#         "street_name.dispatchEvent(new Event('change',  { bubbles: true }));",
#         "street_name.dispatchEvent(new Event('blur',  { bubbles: true }));"
#     )
# )
#
# # RSObject$findElement(
# #         using = "id",
# #         value = "ctl00_PlaceHolderMain_generalSearchForm_ddlGSStreetSuffix")$sendKeysToElement(list(GSStreetSuffix))
#
# RSObject$executeScript(
#     paste0(
#         "var street_suffix = document.getElementById('ctl00_PlaceHolderMain_generalSearchForm_ddlGSStreetSuffix');",
#         "street_suffix.value = '", GSStreetSuffix, "';",
#         "street_suffix.dispatchEvent(new Event('input',  { bubbles: true }));",
#         "street_suffix.dispatchEvent(new Event('change',  { bubbles: true }));",
#         "street_suffix.dispatchEvent(new Event('blur',  { bubbles: true }));"
#     )
# )
#
# # RSObject$findElement(
# #     using = "id",
# #     value = "ctl00_PlaceHolderMain_generalSearchForm_txtGSCity")$sendKeysToElement(list(GSCity))
#
# RSObject$executeScript(
#     paste0(
#         "var city = document.getElementById('ctl00_PlaceHolderMain_generalSearchForm_txtGSCity');",
#         "city.value = '", GSCity, "';",
#         "city.dispatchEvent(new Event('input',  { bubbles: true }));",
#         "city.dispatchEvent(new Event('change',  { bubbles: true }));",
#         "city.dispatchEvent(new Event('blur',  { bubbles: true }));"
#     )
# )
#
# # RSObject$findElement(
# #     using = "id",
# #     "ctl00_PlaceHolderMain_generalSearchForm_ddlGSState_State1")$sendKeysToElement(list(GSState))
#
# RSObject$executeScript(
#     paste0(
#         "var state = document.getElementById('ctl00_PlaceHolderMain_generalSearchForm_ddlGSState_State1');",
#         "state.value = '", GSState, "';",
#         "state.dispatchEvent(new Event('input',  { bubbles: true }));",
#         "state.dispatchEvent(new Event('change',  { bubbles: true }));",
#         "state.dispatchEvent(new Event('blur',  { bubbles: true }));"
#     )
# )
#
# # zip_js <- paste0(
# #     "var zipInput = document.getElementById('ctl00_PlaceHolderMain_generalSearchForm_txtGSAppZipSearchPermit');",
# #     "zipInput.value = '", GSZip, "';",
# #     "zipInput.blur();"
# # )
# #
# # RSObject$executeScript(zip_js)
#
# RSObject$executeScript(
#     paste0(
#         "var zip = document.getElementById('ctl00_PlaceHolderMain_generalSearchForm_txtGSAppZipSearchPermit');",
#         "zip.value = '", GSZip, "';",
#         "zip.dispatchEvent(new Event('input',  { bubbles: true }));",
#         "zip.dispatchEvent(new Event('change',  { bubbles: true }));",
#         "zip.dispatchEvent(new Event('blur',  { bubbles: true }));"
#     )
# )
#
# # Click the 'Search' button
# RSObject$findElement(
#     using = "id",
#     value = "ctl00_PlaceHolderMain_btnNewSearch"
# )$clickElement()
#
#     Sys.sleep(5)  # Wait for results to load
# #
# #     # Click 'Download' (Export to Excel)
# #     download_button <- RSObject$findElement(using = "id", "ctl00_PlaceHolderMain_btnExport")
# #     download_button$click()
# #
# #     Sys.sleep(10)  # Allow time for file to download
#
# # }

# Call the login function
RSelenium_instance <- login_sarasota("cknell", "P8)LmB?Jxurp%s")

navigate_to_accela_search(RSelenium_instance)

# Collect data
# collect_data(RSelenium_instance, "diamond")

# Shut down Selenium instance if active
kill_RSelenium_instance()


# DataEditR::data_edit("01_raw_data/building_permits_history_accela_Diamond.csv", viewer = "browser")
