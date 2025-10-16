library(rsconnect)
library(tidyverse)

# Specify the path to your RMarkdown file
directory_contents <- list.files()

rmarkdown_file <- directory_contents[5]

if (!file.exists(rmarkdown_file)) {
    stop("The specified RMarkdown file does not exist.")
} else {

# Deploy the RMarkdown file
rsconnect::deployDoc(
#    appFiles = rmarkdown_file,
    rmarkdown_file,
    appName = "emerald_gardens",       # Provide a name for your app
    account = "kilonovember",    # Use your shinyapps.io account name
    logLevel = "verbose"
)
}
