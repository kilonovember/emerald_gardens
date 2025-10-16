# R Utility scripts -- Not part of the app, but used for troubleshooting

search_r_files_recursive <- function(directory_path, search_string) {
    r_files <- list.files(directory_path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
    results <- list()

    for (file in r_files) {
#        print(file)
        lines <- readLines(file, warn = FALSE)
        matches <- which(grepl(search_string, lines, fixed = TRUE))

        if (length(matches) > 0) {
            results[[file]] <- list(
                lines = lines[matches],
                line_numbers = matches
            )
        }
    }

    if (length(results) == 0) {
        cat("No matches found in directory:", directory_path, "and its subdirectories.\n")
    } else {
        cat("Results for directory:", directory_path, "\n")
        for (file_name in names(results)) {
            cat("  File:", file_name, "\n")
            file_results <- results[[file_name]]
            for (i in seq_along(file_results$lines)) {
                cat("    Line", file_results$line_numbers[i], ":", file_results$lines[i], "\n")
            }
        }
    }
}

# Example usage:
# directory_to_search <- "your_directory_path" # Replace with your directory path
# string_to_find <- "your_search_string"     # Replace with your search string
#
# search_r_files_recursive(directory_to_search, string_to_find)


# Create TOC for functions in R Scripts

process_r_scripts <- function(directory = "00_R_scripts") {
    r_files <- list.files(directory, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
    all_results <- list()

    for (file_path in r_files) {
        lines <- read_lines(file_path)

        result_lst <- imap(lines, function(line, index) {
            if (str_detect(line, "<- function\\(.*\\)")) { # Modified regex
                list(
                    Script = file_path,
                    Line = index,
                    function_def = line
                )
            } else {
                NULL
            }
        })

        result_lst <- compact(result_lst)
        all_results <- c(all_results, result_lst)
    }

    if (length(all_results) > 0) {
        bind_rows(all_results)
    } else {
        tibble()
    }
}

# Call the function to process all R scripts
# results_df <- process_r_scripts()


# Exploratory Data Analysis Tools:

# Examine each column in a data frame and return the column name and data class.

library(tidyverse)

get_column_info <- function(df) {
    # Use purrr::map to iterate over the columns of the data frame
    column_info <- map(names(df), ~{
        # Get the class of the current column
        col_class <- class(df[[.x]])

        # Return a named list containing the column name and its class
        list(name = .x, class = col_class)
    })

    # Convert the list of lists to a data frame for a more readable output
    column_info_df <- data.frame(
        name = map_chr(column_info, "name"),
        class = map_chr(column_info, "class")
    )

    return(column_info_df)
}

# Example Usage
my_df <- data.frame(
    A = 1:5,
    B = c("a", "b", "c", "d", "e"),
    C = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
    D = c(TRUE, FALSE, TRUE, FALSE, TRUE)
)

# Get the column information
df_column_info <- get_column_info(my_df)

# Print the result
print(df_column_info)
