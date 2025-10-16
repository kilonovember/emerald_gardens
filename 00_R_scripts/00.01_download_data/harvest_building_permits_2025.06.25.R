# Harvest Building Permits 2025.06.25

# Sarasota Building Permits
# P8)LmB?Jxurp%s
# cknell

library(tidyverse)
library(RSelenium)
library(rvest)
library(httr)

# Functions ----
## Locate search fields ----
locate_search_fields <- function(){
    # Locate inputs and other controls on search page----
    # Create a list to store all the found elements
    found_elements_list <- list()

    ## Locate the Clear button ----
    clear_button <- NULL # Initialize outside the loop
    found_element <- FALSE
    for (i in 1:10) {
        elements <- remDr$findElements(using = "xpath", value = XP_clear_button)
        if (length(elements) > 0) {
            clear_button <- elements[[1]] # Assign the found element
            found_element <- TRUE
            break # Element found, exit the loop immediately
        }
        cat(paste0("Attempt ", i, ": clear_button element not found. Waiting 1 second...\n"))
        Sys.sleep(1) # Wait before the next retry
    }

    if (found_element) {
        cat("clear_button element found successfully after", i, "attempts.\n")
        found_elements_list$clear_button <- clear_button
    } else {
        # If the loop finishes without finding the element
        cat(paste("Error: clear_button element ", XP_clear_button, " not found after 10 attempts.\n"))
        cat("Closing Selenium session due to element not found.\n")
        remDr$close()
        stop("Search aborted: clear_button element not found.")
    }


    ## Locate the Look Up button ----
    lookup_button <- NULL # Initialize outside the loop
    found_element <- FALSE
    for (i in 1:10) {
        elements <- remDr$findElements(using = "xpath", value = XP_lookup_button)
        if (length(elements) > 0) {
            lookup_button <- elements[[1]] # Assign the found element
            found_element <- TRUE
            break # Element found, exit the loop immediately
        }
        cat(paste0("Attempt ", i, ": lookup_button element not found. Waiting 1 second...\n"))
        Sys.sleep(1) # Wait before the next retry
    }

    if (found_element) {
        cat("lookup_button element found successfully after", i, "attempts.\n")
        found_elements_list$lookup_button <- lookup_button
    } else {
        # If the loop finishes without finding the element
        cat(paste("Error: lookup_button element ", XP_street_num_low_input, " not found after 10 attempts.\n"))
        cat("Closing Selenium session due to element not found.\n")
        remDr$close()
        stop("Search aborted: lookup_button element not found.")
    }

    ## Locate the street number inputs ----
    ### Street number low ----
    street_num_low_input <- NULL # Initialize outside the loop
    found_element <- FALSE
    for (i in 1:10) {
        elements <- remDr$findElements(using = "xpath", value = XP_street_num_low_input)
        if (length(elements) > 0) {
            street_num_low_input <- elements[[1]] # Assign the found element
            found_element <- TRUE
            break # Element found, exit the loop immediately
        }
        cat(paste0("Attempt ", i, ": street_num_low_input element not found. Waiting 1 second...\n"))
        Sys.sleep(1) # Wait before the next retry
    }

    if (found_element) {
        cat("street_num_low_input element found successfully after", i, "attempts.\n")
        found_elements_list$street_num_low_input <- street_num_low_input
    } else {
        # If the loop finishes without finding the element
        cat(paste("Error: street_num_low_input element ", XP_street_num_low_input, " not found after 10 attempts.\n"))
        cat("Closing Selenium session due to element not found.\n")
        remDr$close()
        stop("Search aborted: street_num_low_input element not found.")
    }

    ### Street number high ----
    street_num_high_input <- NULL # Initialize outside the loop
    found_element <- FALSE
    for (i in 1:10) {
        elements <- remDr$findElements(using = "xpath", value = XP_street_num_high_input)
        if (length(elements) > 0) {
            street_num_high_input <- elements[[1]] # Assign the found element
            found_element <- TRUE
            break # Element found, exit the loop immediately
        }
        cat(paste0("Attempt ", i, ": street_num_high_input element not found. Waiting 1 second...\n"))
        Sys.sleep(1) # Wait before the next retry
    }

    if (found_element) {
        cat("street_num_high_input element found successfully after", i, "attempts.\n")
        found_elements_list$street_num_high_input <- street_num_high_input
    } else {
        # If the loop finishes without finding the element
        cat(paste("Error: street_num_high_input element ", XP_street_num_high_input,
                  " not found after 10 attempts.\n"))
        cat("Closing Selenium session due to element not found.\n")
        remDr$close()
        stop("Search aborted: street_num_high_input element not found.")
    }

    ### Street name ----
    street_name_input <- NULL # Initialize outside the loop
    found_element <- FALSE
    for (i in 1:10) {
        elements <- remDr$findElements(using = "xpath", value = XP_street_name_input)
        if (length(elements) > 0) {
            street_name_input <- elements[[1]] # Assign the found element
            found_element <- TRUE
            break # Element found, exit the loop immediately
        }
        cat(paste0("Attempt ", i, ": street_name_input element not found. Waiting 1 second...\n"))
        Sys.sleep(1) # Wait before the next retry
    }

    if (found_element) {
        cat("street_name_input element found successfully after", i, "attempts.\n")
        found_elements_list$street_name_input <- street_name_input
    } else {
        # If the loop finishes without finding the element
        cat(paste("Error: street_name_input element ", XP_street_name_input, " not found after 10 attempts.\n"))
        cat("Closing Selenium session due to element not found.\n")
        remDr$close()
        stop("Search aborted: street_name_input element not found.")
    }

    ### Street type ----
    street_type_input <- NULL # Initialize outside the loop
    found_element <- FALSE
    for (i in 1:10) {
        elements <- remDr$findElements(using = "xpath", value = XP_street_type_select)
        if (length(elements) > 0) {
            street_type_input <- elements[[1]] # Assign the found element
            found_element <- TRUE
            break # Element found, exit the loop immediately
        }
        cat(paste0("Attempt ", i, ": street_type_select element not found. Waiting 1 second...\n"))
        Sys.sleep(1) # Wait before the next retry
    }

    if (found_element) {
        cat("street_suffix_select element found successfully after", i, "attempts.\n")
        found_elements_list$street_type_input <- street_type_input
    } else {
        # If the loop finishes without finding the element
        cat(paste("Error: street_suffix_select element ", XP_street_type_select,
                  " not found after 10 attempts.\n"))
        cat("Closing Selenium session due to element not found.\n")
        remDr$close()
        stop("Search aborted: street_type_select element not found.")
    }

    ### ZIP code ----
    ZIP_input <- NULL # Initialize outside the loop
    found_element <- FALSE
    for (i in 1:10) {
        elements <- remDr$findElements(using = "xpath", value = XP_ZIP_input)
        if (length(elements) > 0) {
            ZIP_input <- elements[[1]] # Assign the found element
            found_element <- TRUE
            break # Element found, exit the loop immediately
        }
        cat(paste0("Attempt ", i, ": ZIP_input element not found. Waiting 1 second...\n"))
        Sys.sleep(1) # Wait before the next retry
    }

    if (found_element) {
        cat("ZIP_input element found successfully after", i, "attempts.\n")
        found_elements_list$ZIP_input <- ZIP_input
    } else {
        # If the loop finishes without finding the element
        cat(paste("Error: ZIP_input element ", XP_ZIP_input, " not found after 10 attempts.\n"))
        cat("Closing Selenium session due to element not found.\n")
        remDr$close()
        stop("Search aborted: ZIP_input element not found.")
    }
    return(found_elements_list)
}

## fill_in_search_fields() ----
fill_in_search_fields <- function(street_name){
    search_fields <- locate_search_fields()
    if(street_name == "diamond"){
        search_fields$street_name_input$sendKeysToElement(list("Diamond"))
        search_fields$street_num_low_input$sendKeysToElement(list("4401"))
        search_fields$street_num_high_input$sendKeysToElement(list("4513"))
        search_fields$street_type_input$sendKeysToElement(list("Cir"))
        search_fields$ZIP_input$sendKeysToElement(list("34233"))
        return(TRUE)
    }else if(street_name == "topaz"){
        search_fields$street_name_input$sendKeysToElement(list("Topaz"))
        search_fields$street_num_low_input$sendKeysToElement(list("4512"))
        search_fields$street_num_high_input$sendKeysToElement(list("4591"))
        search_fields$street_type_input$sendKeysToElement(list("Ct"))
        search_fields$ZIP_input$sendKeysToElement(list("34233"))
        return(TRUE)
    }else if(street_name == "opal"){
        search_fields$street_name_input$sendKeysToElement(list("Opal"))
        search_fields$street_num_low_input$sendKeysToElement(list("4415"))
        search_fields$street_num_high_input$sendKeysToElement(list("4497"))
        search_fields$street_type_input$sendKeysToElement(list("Ct"))
        search_fields$ZIP_input$sendKeysToElement(list("34233"))
        return(TRUE)
    }else{
        return(FALSE)
    }
}

# URLs ----
## Login Page ----
login_page_url <- "https://aca-prod.accela.com/SARASOTACO/Login.aspx"

## Search Page ----
search_page_url <- "https://aca-prod.accela.com/SARASOTACO/Cap/CapHome.aspx?module=Building&TabName=Home"

# XPaths ----
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

# Other static values ----
username <- "cknell"
password <- "P8)LmB?Jxurp%s"

# Kill any existing RSelenium session (frees port 4567L) ----

# Get the process ID of the Selenium server
RSeleniumPID <- system("ps aux | grep selenium-server-standalone | grep -v grep | awk '{print $2}'", intern = TRUE)

# Check if PID is found before attempting to kill
if (length(RSeleniumPID) > 0 && RSeleniumPID != "") {
    system(paste("kill -9", paste(RSeleniumPID, collapse = " ")))  # Kill all found Selenium processes
    cat("Selenium process terminated.\n")
} else {
    cat("No Selenium process found.\n")
}

# Start RSelenium session ----
rD <- rsDriver(browser = "firefox", port = 4567L, phantomver = NULL)
remDr <- rD$client

# Navigate to login page ----
remDr$navigate(login_page_url)

## Wait for full page load----
Sys.sleep(3)

## Handle iframe (if necessary) ----
iframes <- remDr$findElements(using = "tag name", value = "iframe")
if (length(iframes) > 0) {
    remDr$switchToFrame(iframes[[1]])
    cat("Switched to iframe.\n")
}

# Locate inputs and other controls on login page----
## Locate username field ----
username_field <- NULL # Initialize outside the loop
found_element <- FALSE

for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_user_name_input)
    if (length(elements) > 0) {
        username_field <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": Username element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("Username element found successfully after", i, "attempts.\n")
    # Now you can proceed to use username_field, e.g.:
    # username_field$sendKeysToElement(list("your_username"))
} else {
    # If the loop finishes without finding the element
    cat(paste("Error: Username element ", XP_user_name_input," not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Login aborted: Username element not found.")
}

## Locate password field ----
password_field <- NULL # Initialize outside the loop
found_element <- FALSE

for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_password_input)
    if (length(elements) > 0) {
        password_field <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": Password element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("Password element found successfully after", i, "attempts.\n")
} else {
    # If the loop finishes without finding the element
    cat(paste("Error: Password element ", XP_password_input, " not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Login aborted: Password element not found.")
}

## Fill in user name and password fields and submit ----
username_field$clearElement()
username_field$sendKeysToElement(list(username))

## Verify username entered ----
current_username <- username_field$getElementAttribute("value")[[1]]
if (current_username == username) {
    cat("Username successfully entered.\n")
} else {
    cat("Username entry failed. Trying JavaScript method...\n")
    remDr$executeScript(
        "document.getElementById('username').value = arguments[0];", list(username)
    )
}

password_field$clearElement()
password_field$sendKeysToElement(list(password))

# Verify password entered ----
current_password <- password_field$getElementAttribute("value")[[1]]
if (!nzchar(current_password)) {
    cat("Password entry failed. Trying JavaScript method...\n")
    remDr$executeScript(
        "document.getElementById('passwordRequired').value = arguments[0];", list(password)
    )
}

cat("Password entered successfully.\n")

# Locate the login button using updated XPath ----
login_button <- NULL # Initialize outside the loop
found_element <- FALSE
for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_login_button)
    if (length(elements) > 0) {
        login_button <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": login_button element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("login_button element found successfully after", i, "attempts.\n")
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

} else {
    # If the loop finishes without finding the element
    cat(paste("Error: login_button element ", XP_login_button, " not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Login aborted: login_button element not found.")
}
Sys.sleep(5)

# Navigate to search page ----
remDr$navigate(search_page_url)
Sys.sleep(5)

# Search for Diamond Circle
fill_in_search_fields("diamond")

# Filling Street Type input then moving to the ZIP input
# appears to reset the DOM, invalidating some input objects.
#
# This is a kluge work-around
remDr$findElements(using = "xpath", value = XP_ZIP_input)[[1]]$sendKeysToElement(list("34233"))
remDr$findElements(using = "xpath", value = XP_lookup_button)[[1]]$clickElement()

# Wait for results to load
Sys.sleep(5)


# Locate the select_all_checkbox ----
select_all_checkbox <- NULL
found_element <- FALSE
for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_select_all_checkbox)
    if (length(elements) > 0) {
        select_all_checkbox <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": select_all_checkbox element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("select_all_checkbox element found successfully after", i, "attempts.\n")
} else {
    # If the loop finishes without finding the element
    cat(paste("Error: lookup_button element ", XP_select_all_checkbox, " not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Search aborted: lookup_button element not found.")
}

# Locate the download button ----
download_button <- NULL # Initialize outside the loop
found_element <- FALSE
for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_download_button)
    if (length(elements) > 0) {
        download_button <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": download_button element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("download_button element found successfully after", i, "attempts.\n")
} else {
    # If the loop finishes without finding the element
    cat(paste("Error: download_button element ", XP_download_button, " not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Search aborted: download_button element not found.")
}

# Download Diamond Circle data
select_all_checkbox$clickElement()
download_button$clickElement()
# Wait for results to load
Sys.sleep(5)

# Search for Topaz Court
remDr$findElements(using = "xpath", value = XP_clear_button)[[1]]$clickElement()
fill_in_search_fields("topaz")

# Filling Street Type input then moving to the ZIP input
# appears to reset the DOM, invalidating some input objects.
#
# This is a kluge work-around
remDr$findElements(using = "xpath", value = XP_ZIP_input)[[1]]$sendKeysToElement(list("34233"))
remDr$findElements(using = "xpath", value = XP_lookup_button)[[1]]$clickElement()

# Wait for results to load
Sys.sleep(5)


# Locate the select_all_checkbox ----
select_all_checkbox <- NULL
found_element <- FALSE
for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_select_all_checkbox)
    if (length(elements) > 0) {
        select_all_checkbox <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": select_all_checkbox element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("select_all_checkbox element found successfully after", i, "attempts.\n")
} else {
    # If the loop finishes without finding the element
    cat(paste("Error: lookup_button element ", XP_select_all_checkbox, " not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Search aborted: lookup_button element not found.")
}

# Locate the download button ----
download_button <- NULL # Initialize outside the loop
found_element <- FALSE
for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_download_button)
    if (length(elements) > 0) {
        download_button <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": download_button element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("download_button element found successfully after", i, "attempts.\n")
} else {
    # If the loop finishes without finding the element
    cat(paste("Error: download_button element ", XP_download_button, " not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Search aborted: download_button element not found.")
}

# Download Topaz Court data
select_all_checkbox$clickElement()
download_button$clickElement()

# Wait for results to load
Sys.sleep(5)

# Search for Opal Court
remDr$findElements(using = "xpath", value = XP_clear_button)[[1]]$clickElement()
fields_lst <- fill_in_search_fields("opal")

# Filling Street Type input then moving to the ZIP input
# appears to reset the DOM, invalidating some input objects.
#
# This is a kluge work-around
remDr$findElements(using = "xpath", value = XP_ZIP_input)[[1]]$sendKeysToElement(list("34233"))
remDr$findElements(using = "xpath", value = XP_lookup_button)[[1]]$clickElement()

# Wait for results to load
Sys.sleep(5)


# Locate the select_all_checkbox ----
select_all_checkbox <- NULL
found_element <- FALSE
for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_select_all_checkbox)
    if (length(elements) > 0) {
        select_all_checkbox <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": select_all_checkbox element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("select_all_checkbox element found successfully after", i, "attempts.\n")
} else {
    # If the loop finishes without finding the element
    cat(paste("Error: lookup_button element ", XP_select_all_checkbox, " not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Search aborted: lookup_button element not found.")
}

# Locate the download button ----
download_button <- NULL # Initialize outside the loop
found_element <- FALSE
for (i in 1:10) {
    elements <- remDr$findElements(using = "xpath", value = XP_download_button)
    if (length(elements) > 0) {
        download_button <- elements[[1]] # Assign the found element
        found_element <- TRUE
        break # Element found, exit the loop immediately
    }
    cat(paste0("Attempt ", i, ": download_button element not found. Waiting 1 second...\n"))
    Sys.sleep(1) # Wait before the next retry
}

if (found_element) {
    cat("download_button element found successfully after", i, "attempts.\n")
} else {
    # If the loop finishes without finding the element
    cat(paste("Error: download_button element ", XP_download_button, " not found after 10 attempts.\n"))
    cat("Closing Selenium session due to element not found.\n")
    remDr$close()
    stop("Search aborted: download_button element not found.")
}

# Download Topaz Court data
select_all_checkbox$clickElement()
download_button$clickElement()

# Wait for results to load
Sys.sleep(5)

