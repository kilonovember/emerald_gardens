# Miscellaneous visualization creation functions. Select from these
# for display on the Data Visualizations tab

# NOTES:

# BASIC TEST PROCEDURE - Clear global environment and source this file.

# 1) Review all lm plots for correct use of lm formula
# 2) Review all create_lm_x functions for correctness and consistency
# 3) Review all extract_lm_x_rediduals functions for correctness and consistency
# 4) Review all residuals plots for correctness and consistency
# 5) ?

# Function to create annual sales turnovers of house ownership.
# Those transfers which do not change the beneficial ownership
# are omitted as they don't signify an actual change of owner.
# Such are those where title is passed to a trust to avoid
# probate in the event of the death of the owner. Thise can
# be identified as those having a recorded consideration of
# $0 or $100,

# Load libraries
library(tidyverse)
library(plotly)
library(scales)

# Source custom ggplot themes ----
source("00_R_scripts/00.04_custom_ggplot_themes/custom_economist_themes.R")

# Load data file if it isn't already present ----
if(!exists("property_info_pages_tbl", envir = .GlobalEnv)){
    property_info_pages_tbl <- read_rds("02_processed_data/property_info_pages_tbl.rds")
}

# Data prep functions----

# Extract the latest record of each property and combined them into a single tibble. ----
make_latest_assessments_tbl <- function(){
    latest_assessments_tbl <- property_info_pages_tbl %>%
        mutate(first_assessment = map(assessed_value_history, ~ .x[1, ])) %>%
        select(first_assessment) %>%
        unnest(first_assessment) %>%
        bind_cols(property_info_pages_tbl$account, .) %>%
        rename(account = `...1`)
    return(latest_assessments_tbl)
}

# Extract effective year built for each property ----
make_effective_year_built_tbl <- function(){
    effective_year_built_tbl <- property_info_pages_tbl$buildings_info %>%
        bind_rows() %>%
        bind_cols(property_info_pages_tbl$account, .) %>%
        rename(account = `...1`) %>%
        select(account, eff_yr_built)
    return(effective_year_built_tbl)
}

# Make table for linear models with several independent variables
make_lm_data_tbl <- function(){
    # Test for availability of property_info_pages_tbl
    if(!exists("property_info_pages_tbl")){
        property_info_pages_tbl <- read_rds("02_processed_data/property_info_pages_tbl.rds")
    }
    year_built_tbl <- property_info_pages_tbl$ownership_block %>%
        bind_rows() %>%
        # Extract street address
        select(situs_addr) %>%
        mutate(addr = str_remove(situs_addr, " SARASOTA, FL, 34233")) %>%
        select(-situs_addr) %>%
        # Add account column
        bind_cols(property_info_pages_tbl$account, .) %>%
        rename(account = `...1`)

    # Add building information
    buildings_info_tbl <- property_info_pages_tbl$buildings_info %>%
        bind_rows() %>%
        # Extract columns of interest
        select(-c(situs_addr, bldg_num, year_built, stories))

    # Add lot size information
    lot_size_tbl <- property_info_pages_tbl$property_description_block %>%
        bind_rows() %>%
        bind_cols(property_info_pages_tbl$account, .) %>%
        rename(account = `...1`) %>%
        select(account, lot_size) %>%
        mutate(
            lot_size = str_remove(lot_size, ",") %>%
                str_remove(., " Sq.Ft.") %>%
                parse_number(.)
        )

    # Combine
    lm_data_tbl <- bind_cols(year_built_tbl, buildings_info_tbl)

    # Adjust extra_features_tbls so that in cases where there are no extra feature
    # the tibble move the value in the "row_id" column to a "description" column
    # and the row_id column will be assigned an integer value of 1.
    extra_features_tbl <- map(
        property_info_pages_tbl$extra_features,
        ~ if (is_tibble(.x) && nrow(.x) == 1 && ncol(.x) == 1 && "row_id" %in% names(.x)) {
            .x <- .x %>%
                mutate(description = .x$row_id, row_id = 1)
        } else {
            .x
        }
    )

    # Restructure those extra_features so that all
    # have the same structure
    extra_features_tbl <- map(
        extra_features_tbl,
        ~ if (is_tibble(.x) && nrow(.x) == 1  && !("bldg_num") %in% names(.x)){
            .x <- .x %>%
                add_column("bldg_num" = 1) %>%
                select(row_id, bldg_num, description)
        }else{
            .x
        }
    )

    # Add account column to each tibble
    extra_features_tbl <- map2(
        extra_features_tbl,
        property_info_pages_tbl$account,
        ~ if(is_tibble(.x)) {
            .x %>% add_column(account = .y)
        } else {
            .x
        }
    ) %>%
        # Collapse all tibbles into a single tibble
        bind_rows()

    # Add has_pool column
    extra_features_tbl <- extra_features_tbl %>%
        mutate(has_pool = ifelse(description == "Swimming Pool", TRUE, FALSE)) %>%
        # Remove unwanted columns from extra_features_tbl
        select(-c(row_id, bldg_num, units, units_measure, year)) %>%
        select(account, everything())

    # Select accounts with pool
    pools <- extra_features_tbl %>%
        filter(description == "Swimming Pool")

    # Select accounts where there is no pool
    no_pools <- extra_features_tbl %>%
        group_by(account) %>%
        filter(!any(description == "Swimming Pool")) %>%
        slice(1) %>%
        ungroup()

    # Combine pools and no_pools
    pool_tbl <- bind_rows(pools, no_pools) %>%
        select(-description)

    # Add pool column to lm_data_tbl
    lm_data_tbl <- lm_data_tbl %>%
        left_join(pool_tbl, by = "account")

    # Convert gross_area and living_area columns to integer
    lm_data_tbl <- lm_data_tbl %>%
        mutate(
            gross_area = parse_number(gross_area),
            living_area = parse_number(living_area)
        )

    # Add lot size
    lm_data_tbl <- lm_data_tbl %>%
        left_join(lot_size_tbl, by = "account")

    # Add latest just value
    just_value_tbl <- map2(
        property_info_pages_tbl$assessed_value_history,
        property_info_pages_tbl$account,
        ~ if(is_tibble(.x)) {
            .x %>% add_column(account = .y)
        } else {
            .x
        }
    ) %>%
        bind_rows() %>%
        filter(year == max(year)) %>%
        select(account, just) %>%
        # transform just to number
        mutate(
            just = str_remove(just, ",") %>%
                str_remove(., "\\$") %>%
                parse_number(.)
        )

    # Add column to lm_data_tbl
    lm_data_tbl <- lm_data_tbl %>%
        left_join(just_value_tbl, by = "account")

    return(lm_data_tbl)
}

# Mean premium for each year
compute_mean_price_premium_pct <- function(){
    data  <-  make_sale_price_and_just_value_table()
    data <- data %>%
        group_by(xfer_year) %>%
        summarize(mean_premium = mean(pct_delta)) %>%
        mutate(mean_premium = scales::percent(mean_premium, accuracy = 1)) %>%
        ungroup()
    return(data)
}

# Function to replace log_ values with original dollar values ----
log_to_dollar <- function(string){
    replacement <- str_extract_all(string, "\\d+.\\d+") %>%
        pluck(1) %>%
        as.numeric(.) %>%
        exp() %>%
        round(., digits = 0) %>%
        scales::dollar()
    return(replacement)
}

# Model creation functions ----

# Model with gross_area removed because of high co-linearity with lot_size and
# bedroom count removed due to high p-value (not a significant contributor to just value)
create_lm_4a <- function(){
    return(
        lm(
            data = make_lm_data_tbl(),
            just ~ lot_size + living_area + has_pool +
                bathrooms + half_baths + eff_yr_built
        )
    )
}

# Use glm() instead of lm()
create_glm_4a <- function(){
    return(
        glm(
            data = make_lm_data_tbl(),
            just ~ lot_size + living_area + has_pool +
                bathrooms + half_baths + eff_yr_built
        )
    )
}

# glm with log transformations
create_glm_4a_log <- function() {
    return(
        glm(
            data = make_lm_data_tbl() %>%
                mutate(
                    log_just = log(just),
                    log_lot_size = log(lot_size),
                    log_living_area = log(living_area)
                ),
            log_just ~ log_lot_size + log_living_area + has_pool +
                bathrooms + half_baths + eff_yr_built
        )
    )
}

# robust mode
create_robust_model <- function(){
    robust_model <- MASS::rlm(log(just) ~ log(lot_size) + log(living_area )+ has_pool +
                            bathrooms + half_baths + eff_yr_built,
                        data = make_lm_data_tbl())
    return(robust_model)
}

# Check for multicolinearity
car::vif(create_lm_4a())
car::vif(create_glm_4a_log())
car::vif(create_robust_model())

# Non-statistical plots ----

# Sale and transfer history
create_sale_history_plot <- function(account_num){
    # Test for availability of property_info_pages_tbl
    if(!exists("property_info_pages_tbl")){
        property_info_pages_tbl <- read_rds("02_processed_data/property_info_pages_tbl.rds")
    }

    # Construct chain of ownership via transfers
    xfer_history_tbl <- property_info_pages_tbl %>%
        filter(account == account_num) %>%
        pull(transfers) %>%
        pluck(1) %>%
        mutate(account = account_num) %>%
        select(account, xfer_date, recorded_consideration, seller) %>%
        mutate(buyer = lag(str_to_title(seller))) %>%
        mutate(seller = str_to_title(seller)) %>%
        mutate(xfer_date = as.Date(xfer_date, "%m/%d/%Y")) %>%
        mutate(numeric_consideration = parse_number(recorded_consideration))

    current_ownership <- property_info_pages_tbl %>%
        filter(account == account_num) %>%
        pull(ownership_block) %>%
        pluck(1) %>%
        select(owner_1, owner_2, owner_3)

    # Format current ownership
    current_ownership <-if (is.na(current_ownership$owner_3[1])) { # Check the first element of owner_3
        if (is.na(current_ownership$owner_2[1])) {
            str_to_title(current_ownership$owner_1[1])
        } else {
            paste0(str_to_title(current_ownership$owner_1[1]),
                                 "<br />",
                                 str_to_title(current_ownership$owner_2[1]))
        }
    } else {
        paste0(str_to_title(current_ownership$owner_1[1]),
                             "<br />",
                             str_to_title(current_ownership$owner_2[1]),
                             "<br />",
                             str_to_title(current_ownership$owner_3[1]))
    }

    xfer_history_tbl$buyer[1] <- current_ownership

    # Create tooltip column
    xfer_history_tbl <- xfer_history_tbl %>%
        mutate(tooltip = paste("Buyer: ", buyer, "<br />",
                               "Seller: ", seller, "<br /><br />",
                               "Sale date: ", xfer_date, "<br />",
                               "Price: ", recorded_consideration)
        )

    # Capture the address for use in title
    title_txt <- property_info_pages_tbl %>%
        filter(.$account == account_num) %>%
        pull(ownership_block) %>%
        pluck(1) %>%
        pull(situs_addr) %>%
        str_remove(., " SARASOTA, FL, \\d{5}$") %>%
        str_to_title(.)

    plot <- ggplot(data = xfer_history_tbl, aes(x = xfer_date, y = numeric_consideration)) +
        geom_line(aes(group = 1), color = "red") +
        geom_point(color = "blue", size = 5) +
        xlab("\nTransfer Date") +
        ylab("Consideration\n") +
        labs(title = title_txt) +
        scale_y_continuous(labels = scales::dollar_format()) +  # Format y-axis labels as dollars
        theme_economist_wheat_bkgrnd()

    plot <- ggplotly(plot)
    # Replace tooltip with custom values
    plot$x$data[[2]]$text <- xfer_history_tbl$tooltip
    plot$x$data[[1]]$text <- ""

    return(plot)
}

# Find all sales in which beneficial ownership changed,
# group them by year and sum them.
create_sales_by_year_plot <- function(){
    # Test for availability of property_info_pages_tbl
    if(!exists("property_info_pages_tbl")){
        property_info_pages_tbl <- read_rds("02_processed_data/property_info_pages_tbl.rds")
    }
    sales_grouped_by_year_plot <-
        # Flatten list of tibbles into a single tibble
        map(
            property_info_pages_tbl$transfers,
            ~ .x %>%
                mutate(qual_code = as.character(qual_code))
        ) %>%
        bind_rows() %>%
        # Convert xfer_date from character to date
        mutate(xfer_date = as.Date(xfer_date, "%m/%d/%Y")) %>%
        # Filter out rows where no beneficial ownership change took place
        filter(!recorded_consideration %in% c("$0", "$100")) %>%
        # Count the filtered sales by year
        count(year(xfer_date), name = "count") %>%
        rename(year = `year(xfer_date)`) %>%
        ggplot(data = ., aes(x = year, y = count)) +
        geom_point() +
        geom_line() +
        xlab("Year of Sale") +
        ylab("Count of Sales") +
        labs(title = "Count of Sales by Year") +
        ggthemes::theme_economist() +
        theme(legend.position = "none")
    ggp <- ggplotly(sales_grouped_by_year_plot)
    replacement_tooltips <- ggp$x$data[[1]]$text %>%
        str_remove_all("year: ") %>%
        str_replace_all("count", "Count of Sales")
    ggp$x$data[[1]]$text <- replacement_tooltips
    ggp$x$data[[1]]$line$color <- "blue"  # Ensure line is blue
    ggp$x$data[[1]]$marker$color <- "red"  # Ensure points are red
    ggp$x$data[[1]]$marker$line$color <- "red"  # Ensure points are outlined in red
    ggp$x$data[[1]]$marker$size <- 8 # Adjust point size

    return(ggp)
}

# Compare sale price to the SCPA just value for each property
make_sale_price_and_just_value_table <- function(){
    # Test for availability of property_info_pages_tbl
    if(!exists("property_info_pages_tbl")){
        property_info_pages_tbl <- read_rds("02_processed_data/property_info_pages_tbl.rds")
    }
    # make a tibble of the most recent arms-length sale for each property
    sales_tbl <- property_info_pages_tbl %>%
        mutate(transfers = map2(account, transfers, ~ .y %>%
                                    mutate(
                                        account = .x,
                                        qual_code = as.character(qual_code)  # Ensures qual_code is always a character
                                    )
        )) %>%
        select(transfers) %>%
        unnest(transfers) %>%
        select(account, everything()) %>%
        # remove rows representing transfers which were not "arms-length"
        filter(!recorded_consideration %in% c("$0", "$100")) %>%
        # transform xfer_date to date type
        mutate(xfer_date = as.Date(xfer_date, "%m/%d/%Y")) %>%
        # sort by account and by xfer_date descending so that the most recent sale is first
        arrange(account, desc(xfer_date)) %>%
        mutate(xfer_year = year(xfer_date) %>% as.integer()) %>%
        select(account, xfer_year, recorded_consideration)
    # make a tibble of the just value for each property corresponding to the
    # year of the most recent sale
    just_value_for_sale_yr_tbl <- property_info_pages_tbl %>%
        select(account, assessed_value_history) %>%
        unnest(assessed_value_history) %>%
        select(account, year, just)
    # join most_recent_sale_tbl and just_value_for_sale_yr_tbl
    sales_tbl <- sales_tbl %>%
        left_join(just_value_for_sale_yr_tbl, by = c("account"= "account", "xfer_year" = "year")) %>%
        filter(!is.na(just)) %>%
        mutate(delta = parse_number(recorded_consideration) - parse_number(just)) %>%
        mutate(pct_delta = delta / parse_number(recorded_consideration)) %>%
        mutate(pct_delta_fmt = scales::percent(pct_delta, accuracy = 1))
    return(sales_tbl)
}

# Plot of mean premium for each year ----
create_premium_plot_point <- function(){
    plot <- ggplot() +
        geom_point(data = compute_mean_price_premium_pct(),
                   aes(x = as.factor(xfer_year),
                       y = mean_premium),
                   color = "red",
                   size = 6) +
        labs(
            title = "Mean Sale Price Premium Over Just Value",
        ) +
        ylab("\nMean Premium\n") +
        xlab("Year of Sale\n") +
        theme_economist_wheat_bkgrnd()
    plot <- ggplotly(plot)
    # Adjust tooltips
    plot$x$data[[1]]$text <- plot$x$data[[1]]$text %>%
        str_replace_all("as.factor\\(xfer_year\\)", "Year of sale") %>%
        str_replace_all("mean_premium", "Mean premium")

    return(plot)
}

create_premium_plot_column <- function(){
    # Compute data
    data <- compute_mean_price_premium_pct() %>%
        mutate(mean_premium_numeric = as.numeric(str_remove(mean_premium, "%")) / 100)
    # Create plot
    plot <- ggplot(data) +
        geom_col(aes(x = as.factor(xfer_year), y = mean_premium_numeric, fill = mean_premium >= 0)) +
        geom_point(
            data = data %>% filter(xfer_year == 2020),
            aes(x = as.factor(xfer_year), y = mean_premium_numeric),
            color = "springgreen",
            size = 6,
            alpha = 0.8
        ) +
        scale_fill_manual(values = c("FALSE" = "tomato2", "TRUE" = "steelblue")) +
        labs(
            title = "Mean Sale Price Premium Over Just Value",
            y = "\nMean Premium\n",
            x = "Year of Sale\n"
        ) +
        scale_y_continuous(labels = scales::percent_format()) +
        theme_economist_wheat_bkgrnd() +
        theme(legend.position = "none")

    # Convert to plotly
    plot <- ggplotly(plot)

    # Adjust tooltips

    if (!is.null(plot$x$data[[1]]$text)) {
        formatted_percent <- scales::percent_format()(plot$x$data[[1]]$y)
        for (i in seq_along(formatted_percent)) {
            plot$x$data[[1]]$text[i] <- str_replace(plot$x$data[[1]]$text[i], "0\\.\\d{2}", formatted_percent[i])
        }
        plot$x$data[[1]]$text <- plot$x$data[[1]]$text %>%
            str_remove_all("as.factor\\(xfer_year\\): \\d{4}<br />") %>%
            str_replace_all("mean_premium >= 0: (TRUE|FALSE)", "") %>%
            str_replace_all("mean_premium_numeric: ", "Mean premium: ")
    }

    if (!is.null(plot$x$data[[2]]$text)) {
        formatted_percent <- scales::percent_format()(plot$x$data[[2]]$y)
        for (i in seq_along(formatted_percent)) {
            plot$x$data[[2]]$text[i] <- str_replace(plot$x$data[[2]]$text[i], "0\\.\\d{2}", formatted_percent[i])
        }
        plot$x$data[[2]]$text <- plot$x$data[[2]]$text %>%
            str_remove_all("as.factor\\(xfer_year\\): \\d{4}<br />") %>%
            str_replace_all("mean_premium >= 0:  (TRUE|FALSE)", "") %>%
            str_replace_all("mean_premium_numeric: ", "Mean premium: ")
    }

    plot$x$data[[3]]$text <- paste0("Mean premium", str_remove(plot$x$data[[3]]$text,
                                                               "as.factor\\(xfer_year\\): 2020<br />mean_premium_numeric:"), "%")
    return(plot)
}

# Infer the just value of a property based on its characteristics
generate_expected_just_range <- function(lot_size, living_area, has_pool, bathrooms, half_baths, eff_yr_built, model) {
    log_lot_size <- log(lot_size)
    log_living_area <- log(living_area)

    new_data <- data.frame(
        log_lot_size = log_lot_size,
        log_living_area = log_living_area,
        has_pool = has_pool,
        bathrooms = bathrooms,
        half_baths = half_baths,
        eff_yr_built = eff_yr_built
    )

    # Predict with standard errors
    predictions <- predict(model, newdata = new_data, se.fit = TRUE)

    # Calculate 90% confidence interval
    z_critical <- qnorm(0.95) # 90% CI, so 95th percentile
    lower_bound_log <- predictions$fit - z_critical * predictions$se.fit
    upper_bound_log <- predictions$fit + z_critical * predictions$se.fit

    # Convert back to original scale
    mean_value <- exp(predictions$fit)
    lower_bound <- exp(lower_bound_log)
    upper_bound <- exp(upper_bound_log)

    # Format as dollar values
    mean_dollar <- paste0("$", format(round(mean_value, 2), big.mark = ",", scientific = FALSE))
    lower_dollar <- paste0("$", format(round(lower_bound, 2), big.mark = ",", scientific = FALSE))
    upper_dollar <- paste0("$", format(round(upper_bound, 2), big.mark = ",", scientific = FALSE))

    # Return as a list
    return(list(mean = mean_dollar, low = lower_dollar, high = upper_dollar))
}

# Statistical Plots ----
# Create dot plot per model argument ----
# Retain this one as it is general and
# can work with any model.
create_model_dot_plot <- function(model){
    # Create a dataframe with actual and predicted values
    augmented_data <- broom::augment(model) %>%
        bind_cols(make_lm_data_tbl() %>% select(addr), .)

    # Exponentiate fitted and log_just values
    augmented_data <- augmented_data %>%
        mutate(
            fitted_exp = exp(fitted(model)),
            log_just_exp = exp(log_just)
        )

    # Calculate residuals and standard deviation based on model class
    if (inherits(model, "lm")) {
        sigma_val <- sigma(model)
        augmented_data <- augmented_data %>%
            mutate(
                upper_2sigma = fitted(model) + 2 * sigma_val,
                lower_2sigma = fitted(model) - 2 * sigma_val
            )
    } else if (inherits(model, "glm")) {
        deviance_residuals <- residuals(model, type = "deviance")
        sigma_val <- sd(deviance_residuals)
        augmented_data <- augmented_data %>%
            mutate(
                upper_2sigma = fitted(model) + 2 * sigma_val,
                lower_2sigma = fitted(model) - 2 * sigma_val
            )
    }

    # Transform 2-sigma bounds back to the original scale (if necessary)
    augmented_data <- augmented_data %>%
        mutate(
            upper_2sigma_exp = exp(upper_2sigma),
            lower_2sigma_exp = exp(lower_2sigma)
        )

    # Calculate linear model for swapped axes
    swapped_model <- lm(fitted_exp ~ log_just_exp, data = augmented_data)

    # Calculate residuals and standard deviation
    swapped_residuals <- residuals(swapped_model)
    sigma_val_swapped <- sd(swapped_residuals)

    # Calculate 2-sigma bounds
    augmented_data <- augmented_data %>%
        mutate(
            swapped_fitted = fitted(swapped_model),
            upper_2sigma_swapped = swapped_fitted + 2 * sigma_val_swapped,
            lower_2sigma_swapped = swapped_fitted - 2 * sigma_val_swapped
        )


    # Plot actual vs. predicted values
    plot <- ggplot(augmented_data, aes(x = log_just_exp, y = fitted_exp)) +
        geom_point(color = "royalblue", alpha = 0.9) +
        geom_smooth(method = "lm", color = "tomato2", se = TRUE) +
        geom_line(aes(y = upper_2sigma_swapped), color = "steelblue", linetype = "dashed", linewidth = 0.8) +
        geom_line(aes(y = lower_2sigma_swapped), color = "steelblue", linetype = "dashed", linewidth = 0.8) +
        scale_x_continuous(labels = scales::label_dollar()) +
        scale_y_continuous(labels = scales::label_dollar()) +
        labs(
            title = "Predicted vs. Actual House Valuations with ±2σ Bounds",
            subtitle = "Points above and below the dashed line are outliers.",
            y = "Predicted Value (Fitted)\n",
            x = "'Just' Value"
        ) +
        theme_economist_wheat_bkgrnd()


    # Convert to interactive
    lm_model <- create_glm_4a()
    lm_formula <- as.character(formula(lm_model))
    # Adjust title and subtitle
    plot <- ggplotly(plot) %>%
        layout(
            title = list(
                text = paste0(
                    plot$labels$title,
                    '<br><sup>', plot$labels$subtitle, '</sup>'
                )
            ),
            annotations = list(
                x = 0.05,
                y = 0.95,
                text = paste("Model: ", lm_formula[2], "~", lm_formula[3]),
                showarrow = FALSE,
                xref = "paper", yref = "paper",
                font = list(size = 12, color = "gray30")
            )
        )

    # Tooltip Adjustments
    plot$x$data[[1]]$text <- paste0(
        str_to_title(augmented_data$addr), "<br />",
        str_replace(plot$x$data[[1]]$text, "log_just_exp", "Just Value") %>%
            str_replace(., "fitted_exp", "Predicted value") %>%
            str_replace_all(., "(\\d+\\.\\d+|\\d+)", function(match) {
                scales::dollar(as.numeric(match))
            })
    )

    plot$x$data[[2]]$text <- str_replace(plot$x$data[[2]]$text,"exp\\(.fitted\\)", "Fitted value") %>%
        str_remove(., "<br />.*") %>%
        str_replace_all(., "(\\d+\\.\\d+|\\d+)", function(match) {
            scales::dollar(as.numeric(match))
        })

    # Remove tooltip for shaded smooth boundary
    plot$x$data[[3]]$hoverinfo <- "skip"
    plot$x$data[[3]]$text <- ""

    # Remove tooltips from 2-sigma lines
    plot$x$data[[4]]$hoverinfo <- "skip"
    plot$x$data[[4]]$text <- ""
    plot$x$data[[5]]$hoverinfo <- "skip"
    plot$x$data[[5]]$text <- ""

    return(plot)
}

# Create HTML coefficients table from summary() output
# Example: create_coefficients_table_html(summary(create_glm_4a_log()))
create_coefficients_table_html <- function(summary_output) {
    coef_data <- summary_output$coefficients

    # Convert the coefficient matrix to a data frame
    coef_df <- as.data.frame(coef_data)

    # Capture significance codes (asterisks)
    signif_codes <- character(nrow(coef_df)) # Initialize an empty vector
    p_values <- coef_df$`Pr(>|t|)` # Extract p-values

    signif_codes[p_values < 0.001] <- "***"
    signif_codes[p_values >= 0.001 & p_values < 0.01] <- "**"
    signif_codes[p_values >= 0.01 & p_values < 0.05] <- "*"
    signif_codes[p_values >= 0.05 & p_values < 0.1] <- "."
    signif_codes[p_values >= 0.1] <- " "

    coef_df$Signif <- signif_codes # Add the significance codes as a new column

    html <- "<table style='font-family: Courier New; font-size:14; border-collapse: collapse;'>"

    # Header row
    html <- paste0(html, "<tr>")
    for (col_name in colnames(coef_df)) {
        html <- paste0(html, "<th style='padding: 5px; border: 1px solid #ddd;'>", col_name, "</th>")
    }
    html <- paste0(html, "</tr>")

    # Data rows
    for (i in 1:nrow(coef_df)) {
        html <- paste0(html, "<tr>")
        for (j in 1:ncol(coef_df)) {
            cell_value <- coef_df[i, j]
            # Format numeric values for consistent alignment (right-aligned)
            if (is.numeric(cell_value)) {
                formatted_value <- sprintf("%.6g", cell_value) # Adjust formatting as needed
                html <- paste0(html, "<td style='padding: 5px; border: 1px solid #ddd; text-align: right;'>",
                               formatted_value, "</td>")
            } else { # For non-numeric values (like row names)
                html <- paste0(html, "<td style='padding: 5px; border: 1px solid #ddd;'>", cell_value, "</td>")
            }

        }
        html <- paste0(html, "</tr>")
    }
    html <- paste0(html, "</table>")
    return(html)
}


# Create summary table in HTML format for inclusion in shiny --- FIX <table> BOUNDARIES

create_summary_table_HTML <- function(model){
    # Create model_summary object
    model_summary <- summary(model)
    coefficients_table <- create_coefficients_table_html(model_summary)
    # Capture the formula portion of summary() to a text block
    model_formula_text <- model_summary$call %>% deparse()

    html_formatted_model_formula <- paste(
        paste("<div style='font-family:Courier New; font-size:14;'><div>", model_formula_text[1], "</div>"),
        paste("<div>&nbsp;&nbsp;&nbsp;&nbsp;", model_formula_text[2], "</div>"),
        paste("<div>&nbsp;&nbsp;&nbsp;&nbsp;", model_formula_text[3], "</div>"),
        paste("<div>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;", model_formula_text[4], "</div></div><br />")
    )


    # p-values line for table footer
    signif_codes_line <- paste0("<div style='background-color:#E5E5E5; width:550px;'>",
                                "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1", "</div>")

    # dispersion line for table footer
    dispersion_line <- paste0("<div>", "(Dispersion parameter for gaussian family taken to be ",
                              model_summary$dispersion %>% round(9), ")", "</div>")

    # Null deviance line for table footer
    null_deviance_line <- paste0("<div style='background-color:#E5E5E5; width:550px;'>",
                                 "Null deviance: ",
                                 model_summary$null.deviance %>% round(5),
                                 " on ",
                                 model_summary$df.null,
                                 " degrees of freedom",
                                 "</div>")

    # Residual deviance line for table footer
    residual_deviance_line <- paste0("<div>",
                                     "Residual deviance: ",
                                     model_summary$deviance %>% round(5),
                                     " on ",
                                     model_summary$df.residual,
                                     " degrees of freedom",
                                     "</div>")

    # AIC line for table footer
    AIC_line <- paste0("<div style='background-color:#E5E5E5; width:550px;'>","AIC: ", model_summary$aic %>% round(2), "</div>")

    # Fisher Scoring Iterations line
    fisher_scoring_line <- paste0("<div>Number of Fisher Scoring iterations: ", model_summary$iter,  "</div>")

    # Assemble the table
    summary_table <- paste0(
        coefficients_table,
        "<div>---</div>",
        "<div style='font-family:Courier New; font-size:14;'>",
        signif_codes_line,
        dispersion_line,
        null_deviance_line,
        residual_deviance_line,
        AIC_line,
        fisher_scoring_line,
        "</div><br />"
    )

    return(paste(html_formatted_model_formula, summary_table))
}

# Q-Q Plot of residuals to check for gaussian distribution ----
create_q_q_plot_of_residuals <- function(model){

    # Use residuals for rlm (rstandard doesn't work for rlm)
    residuals <- if ("rlm" %in% class(model)) {
        residuals(model) / model$s
    } else {
        rstandard(model)
    }

    Q_Q_plot <- ggplot() +
        geom_qq(
            aes(sample = residuals),
            geom = "point",
            shape = 16,
            size = 3,
            color = "black",
            alpha = 0.5) +
        geom_abline(color = "red") +  # Adds reference line for normality
        labs(
            title = "Q-Q Plot",
            subtitle = "Comparing Model Residuals to a Normal Distribution",
            x = "\nTheoretical Quantiles",
            y = "Sample Quantiles\n") +
        scale_y_continuous(labels = scales::label_dollar()) +
        ggthemes::theme_economist() +
        theme(plot.title = element_text(margin = margin(b = 5))
        )

    return(Q_Q_plot)
}

# Extract and plot residuals
extract_and_plot_resids <- function(model){
    residuals_vec <-  residuals(model)
    # Select data
    data_tbl <- make_lm_data_tbl()
    resid_plot <- ggplot(data = data_tbl, aes(x = fitted(model), y = resid(model))) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(x = "Fitted Values",
             y = "Residuals",
             title = "Residual Plot",
             subtitle = paste(as.character(formula(model))[2], as.character(formula(model))[1], as.character(formula(model))[3])
             ) +
        scale_x_continuous(labels = scales::label_dollar()) +
        scale_y_continuous(labels = scales::label_dollar()) +
        ylab("Residuals\n") +
        xlab("\nFitted Values") +
        ggthemes::theme_economist() +
        theme(plot.title = element_text(margin = margin(b = 5)))

    return(resid_plot)
}

 # Statistical tests of models
 # After running these tests the most useful model is glm_4a_log
 #

 # Visualize Cook's Distance

create_cooks_distance_plot <- function(model){
    .data <- tibble(
        observation = 1:nrow(model$model),
        cooks_distance = cooks.distance(model)
    )

    # Calculate the threshold for influential points
    threshold <-quantile(cooks.distance(model), 0.95) # 95th percentile


    # Identify influential points
    influential_points <- .data %>%
        filter(cooks_distance > threshold)

    plot <- ggplot(.data, aes(x = observation, y = cooks_distance)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        labs(title = "Cook's Distance Plot", x = "Observation", y = "Cook's Distance") +
        theme_minimal() +
        geom_hline(yintercept = threshold, linetype = "dashed", color = "red") +
        geom_text(
            data = influential_points,
            aes(label = observation),
            vjust = -0.5, # Adjust vertical position of labels
            hjust = 0.5, # Adjust horizontal position of labels
            size = 3.5 # Adjust label size
        )

    return(plot)
}

# Commenting out tests for production

# # Test for heteroscedasticity
#  extract_and_plot_resids(create_lm_4a())
#  extract_and_plot_resids(create_glm_4a())
#  extract_and_plot_resids(create_glm_4a_log())
#
# # Breusch-Pagan Test for Heteroscedasticity
#  lmtest::bptest(create_lm_4a())
#  lmtest::bptest(create_glm_4a())
#  lmtest::bptest(create_glm_4a_log())
#
#  # Non-Constant Variance Score Test - lm() only
#  car:: ncvTest(create_lm_4a())
#
#  # Box-Cox to find the best transformation
#  MASS::boxcox(create_lm_4a(), lambda = seq(-2, 2, 0.1))
#  MASS::boxcox(create_glm_4a(), lambda = seq(-2, 2, 0.1))
#  MASS::boxcox(create_glm_4a_log(), lambda = seq(-2, 2, 0.1))
#
#  plot(residuals(create_glm_4a_log(), type = "deviance") ~ fitted(create_glm_4a_log()))
#  plot(residuals(create_glm_4a(), type = "deviance") ~ fitted(create_glm_4a()))
#
#  # Examining outliers - part 1
#  robust_model <- create_robust_model() # Your robust model
#  hat_values <- hatvalues(robust_model)
#  extracted_rows <- make_lm_data_tbl() %>% slice(c(108, 68, 5))
#  extracted_rows$hat_value <- hat_values[c(108, 68, 5)]
#  print(extracted_rows)
#
#  # Examining outliers - part 2
#  mean_lot_size <- mean(make_lm_data_tbl()$lot_size)
#  sd_lot_size <- sd(make_lm_data_tbl()$lot_size)
#  extracted_rows$lot_size_zscore <- (extracted_rows$lot_size - mean_lot_size) / sd_lot_size
#
#  # Examining outliers - part 3
#  max_lot_size <- make_lm_data_tbl()$lot_size %>% max()
#  min_lot_size <- make_lm_data_tbl()$lot_size %>% min()
#  range_lot_size <- max_lot_size - min_lot_size
#
#  # Alternate models
#
#  # Model with interactive terms
#  robust_model_interact <- MASS::rlm(formula = log(just) ~ log(lot_size) * has_pool + log(lot_size) * bathrooms + log(lot_size) * eff_yr_built + log(living_area) + has_pool + bathrooms + half_baths + eff_yr_built, data = make_lm_data_tbl())
#
#  extract_and_plot_resids(robust_model_interact)
#  create_q_q_plot_of_residuals(robust_model_interact)
#
#  # Model with quadratic term
#  robust_model_poly <- MASS::rlm(formula = log(just) ~ log(lot_size) + I(log(lot_size)^2) + log(living_area) + has_pool + bathrooms + half_baths + eff_yr_built, data = make_lm_data_tbl())
#
#  extract_and_plot_resids(robust_model_poly)
#  create_q_q_plot_of_residuals(robust_model_poly)
#
#  # Comparing models' performance
#
#  weights(create_glm_4a_log())[c(108, 68, 5)]
#  weights(create_robust_model())[c(108, 68, 5)]
#  weights(robust_model_interact)[c(108, 68, 5)]
#  weights(robust_model_poly)[c(108, 68, 5)]
#
#  residuals(create_glm_4a_log())[c(108, 68, 5)]
#  residuals(create_robust_model())[c(108, 68, 5)]
#  residuals(robust_model_interact)[c(108, 68, 5)]
#  residuals(robust_model_poly)[c(108, 68, 5)]



