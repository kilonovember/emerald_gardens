# Script to harvest builidng permit data from Satasota County.

# NOTES:
#   Convert to a function and add the function to the
#   script that parses the data into the building permits tibble.

library(RSelenium)
library(rvest)
library(tidyverse)

kill_java_process <- function(port) {
    tryCatch({
        pid_output <- system(paste("lsof -i :", port, " | grep LISTEN | awk '{print $2}'"), intern = TRUE)
        if (length(pid_output) > 0) {
            pid <- as.integer(pid_output[1])
            system(paste("kill -9", pid))
            message(paste("Java process (PID:", pid, ") killed."))
        } else {
            message("No Java process found on port", port)
        }
    }, error = function(e) {
        message("Error killing Java process:", e$message)
    })
}

fetch_building_permit_data <- function(street, street_type){
    tryCatch({
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
        parsed_results <- read_html(page_source)

        # Extract the table
        tables <- parsed_results %>% html_table()
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


# argument_pairs <- list(
#     list("Diamond", "Circle"),
#     list("Topaz", "Court"),
#     list("Opal", "Court")
# )
#
# building_permits_tbl <- map(argument_pairs, function(pair){
#     fetch_building_permit_data(pair[[1]], pair[[2]])
# })
#
# building_permits_tbl <- bind_rows(building_permits_tbl)
