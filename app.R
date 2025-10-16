# Emerald Gardens v.4.0
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)
library(tidyverse)
library(gt)
library(DT)
library(scales)
library(htmltools)
library(plotly)
library(shinyWidgets)
library(leaflet)


# Load maps ----
assessed_pr_sqft_map <- read_rds("03_plot_files/interactive_assessed_per_sqft_map.rds") %>% event_register("plotly_click")
effective_year_built_map <- read_rds("03_plot_files/interactive_effective_year_built_map.rds") %>% event_register("plotly_click")
last_sale_map <- read_rds("03_plot_files/interactive_last_sale_map.rds") %>% event_register("plotly_click")
owner_occupied_map <- read_rds("03_plot_files/interactive_owner_occupied_map.rds") %>% event_register("plotly_click")
pool_map <- read_rds("03_plot_files/interactive_pool_map.rds") %>% event_register("plotly_click")
roof_map <- read_rds("03_plot_files/interactive_roof_map.rds") %>% event_register("plotly_click")

# Load static plots (no on-demand creation of plot) ----
just_valuation_dot_plot <- read_rds("03_plot_files/model_dot_plot.rds")
just_valuation_qq_plot <- read_rds("03_plot_files/q-q_residuals_plot.rds")
just_valuation_residuals_scatter_plot <- read_rds("03_plot_files/residuals_scatter_plot.rds")
interactive_sales_by_year_plot <- read_rds("03_plot_files/interactive_sales_by_year_plot.rds")


# Load data plots

# Load data
property_info_pages_tbl <- read_rds("02_processed_data/property_info_pages_tbl.rds")
building_permits_mapping_tbl <- read_rds("02_processed_data/building_permits_mapping_tbl.rds")

# Load the property details DT table ----

eg_DT_ssf <- read_rds("02_processed_data/eg_gt_ssf.rds") %>%
    mutate(RowID = row_number(), .before = everything())

# Load pop-up window functions
source("00_R_scripts/00.02_transform_data/pop_up_tables.R")
source("00_R_scripts/00.02_transform_data/property_details_tables_functions.R")
source("00_R_scripts/00.03_map_creation_functions/misc_data_plots.R")
source("00_R_scripts/00.03_map_creation_functions/map_creation_functions.R")

# Functions to create fly-to feature ----
# output$my_map <- renderLeaflet({
#     leaflet() %>%
#         addTiles()
# })
#
# zoom_to_map_coords <- function(map_id, lng, lat, zoom_level = 16, duration = 2){
#     leafletProxy(map_id) %>%
#         flyTo(
#             lng = lng,
#             lat = lat,
#             zoom = zoom_level,
#             options = list(duration = duration)
#         )
# }

zoom_to_map_coords <- function(lng = -82.46923, lat = 27.29021, zoom_level = 16){
    target_lng <- lng
    target_lat <- lat
    target_zoom <- zoom_level

    animap <- leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::flyTo(
            lng = target_lng,
            lat = target_lat,
            zoom = target_zoom,
            options = list(duration = 20))
    return(animap)
}
###################################

ui <- fluidPage(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
    useShinyjs(),

    fluidRow(
        column(12,
               div(
                   style = "display: flex; align-items: baseline; color: green;",
                   titlePanel("Emerald Gardens"),
                   div(style = "margin-left: 10px; font-size: 14pt; color: black;", "Sarasota County Florida"),
                   div(style = "margin-left: 10px; font-size: 10pt; color: black;", HTML("&copy; Charles Knell, 2025"))
               )
        )
    ),

    tabsetPanel(
        # MAPS PANEL UI ----
        tabPanel(
            title = "Maps",
            # Sidebar and Main Panel Layout ----
            sidebarLayout(
                sidebarPanel(
                    radioButtons(
                        inputId = "views",
                        label = "Choose map format:",
                        choices = c(
                            "Owner Occupied",
                            "Has Pool",
                            "Recent Sale",
                            "Assessed Value per Sq.Ft.",
                            "Effective Year Built",
                            "Roofs and Building Permits",
                            "Fly To Emerald Gardens"
                        )
                    ),
                    htmlOutput(outputId = "map_explanation"),  # Add dynamic explanation text
                    width = 2
                ),
                mainPanel(
                    width = 10,
                    conditionalPanel(
                        condition = "input.views == 'Fly To Emerald Gardens'",
                        leafletOutput(outputId = "emerald_gardens_map", height = "800px", width = "100%")
                    ),
                    conditionalPanel(
                        condition = "input.views != 'Fly To Emerald Gardens'",
                        plotlyOutput(outputId = "map_output", height = "800px", width = "100%")
                    )
                ),
                position = "left",
                fluid = TRUE
            )
        ),
        # PROPERTY DETAILS TAB UI ----
        tabPanel(
            title = "Property Details",
            # Sidebar Layout
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "filter_by_street",
                        label = "Filter by Street",
                        choices = c(
                            "All",
                            "DIAMOND CIR E",
                            "DIAMOND CIR S",
                            "DIAMOND CIR W",
                            "DIAMOND CIR N",
                            "OPAL CT",
                            "TOPAZ CT"
                        )
                    ),
                    width = 2
                ),
                # This is the Filter, Sort, and Search table
                mainPanel(
                    width = 10,
                    DTOutput("property_table"),
                    # Display of details panels
                    hr(),
                    fluidRow(
                        tabsetPanel(
                            id = "DT_selection_details",
                            type = "tabs",
                            tabPanel(
                                title = "Sales and Transfers History",
                                gt_output("s_and_t_history_table")
                            ),
                            tabPanel(
                                title = "Assessments History",
                                gt_output("assessments_table")
                            ),
                            tabPanel(
                                title = "Structures",
                                gt_output("buildings_property_table")
                            )
                        )
                    )
                )
            )
        ),

        # VALUATION TAB PLOT UI ----
        tabPanel(
            title = "Just Values",
            sidebarLayout(
                sidebarPanel(
                    div(style = "font-weight: bold; text-align: center; margin-bottom: 10px;",
                        "Click Buttons\nfor Explanations"
                    ),
                    div(id = "just_values_buttons_group",
                        style = "display: flex; flex-direction: column; align-items: center;",
                        div(style = "margin-bottom: 5px; width: 100%;",
                            actionButton(inputId = "actual_vs_predicted",
                                         label = "Actual vs. Predicted",
                                         class = "colored-button",
                                         style = "width: 100%;")),
                        div(style = "margin-bottom: 5px; width: 100%;",
                            actionButton(inputId = "qq-plot",
                                         label = "Q-Q Plot",
                                         style = "width: 100%;",
                                         class = "colored-button")),
                        div(style = "margin-bottom: 5px; width: 100%;",
                            actionButton(inputId = "residuals-plot",
                                         label = "Residuals Plot",
                                         style = "width: 100%;",
                                         class = "colored-button")),
                        div(style = "width: 100%;",
                            actionButton(inputId = "summary",
                                         label = "Model Summary",
                                         style = "width: 100%;",
                                         class = "colored-button"))
                    ),
                    tags$br(), # Add some vertical space
                    div(style = "text-align: center;",
                        actionButton(inputId = "scroll_to_bottom",
                                     label = tags$i(class = "fa fa-arrow-down"),
                                     class = "scroll-button"),
                        p(style = "font-size: 0.8em; margin-top: 5px; text-align: center;",
                          "Scroll to Model Summary")
                    ),
                    width = 2
                ),
                mainPanel(
                    width = 10,
                    plotlyOutput("top_row_plot"),
                    tags$br(),
                    fluidRow( # Add a fluidRow here
                        column(6,
                               plotOutput("bottom_left_plot")
                        ),
                        column(6,
                               plotOutput("bottom_right_plot")
                        )
                    ), # Close the fluidRow
                    tags$br(),
                    fluidRow( # model summary
                        htmlOutput("model_summary")
                    )
                )
            ),

            # Include Font Awesome for the arrow icon
            tags$head(
                tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
            ),

            # JavaScript to handle the scrolling
            tags$script(HTML("
                          $(document).ready(function() {
                            $('#scroll_to_bottom').on('click', function() {
                              window.scrollTo(0, document.body.scrollHeight);
                            });
                          });
                    ")
                ),

            # Optional CSS for the button
            tags$style(HTML("
                          .scroll-button {
                            background-color: #f0f0f0;
                            border: 1px solid #ccc;
                            border-radius: 5px;
                            padding: 10px;
                            font-size: 1.2em;
                            cursor: pointer;
                            width: 100%; /* Make the button take full width of the sidebar */
                            text-align: center;
                          }
                          .scroll-button:hover {
                            background-color: #e0e0e0;
                          }
                          .scroll-button i {
                                    font-size: 1.5em; /* Further increase icon size */
                            }
                        ")
            )
        ),
        # ESTIMATE JUST VALUE UI ----
        tabPanel(
            title = "Estimate Just Value",
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "filter_by_street_jv",
                        label = "Filter by Street",
                        choices = c(
                            "All",
                            "DIAMOND CIR E",
                            "DIAMOND CIR S",
                            "DIAMOND CIR W",
                            "DIAMOND CIR N",
                            "OPAL CT",
                            "TOPAZ CT"
                        )
                    ),
                    uiOutput("house_addr_select"),
                    width = 2
                ),
                mainPanel(
                    fluidRow(
                        column(3,
                               uiOutput("model_estimation_text")
                        ),
                        column(3,
                               uiOutput("expected_just_cards")
                        ),
                        column(8,
                              uiOutput("just_value_estimation")
                        )
                    )
                )
            )
        ),
        # SALES HISTORY ----
        tabPanel(
            title = "Sales History",
            sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId = "filter_by_street_sh",
                        label = "Filter by Street",
                        choices = c(
                            "All",
                            "DIAMOND CIR E",
                            "DIAMOND CIR S",
                            "DIAMOND CIR W",
                            "DIAMOND CIR N",
                            "OPAL CT",
                            "TOPAZ CT"
                        )
                    ),
                    uiOutput("house_addr_select_sh"),
                    width = 2
                ),
                mainPanel(
                    fluidRow( # Sales count by year
                        plotlyOutput("annual_sales_plot")
                    ),
                    tags$br(),
                    fluidRow( # Sale history of particular house
                        plotlyOutput("house_sale_history_plot")
                    )
                )
            )
        ),
        # HOA AND GOVERNMENT ----
        tabPanel(
            title = "HOA and Government",
            sidebarLayout(
                sidebarPanel(
                    div(id = "government_button_group",
                        div(actionButton(inputId = "hoa_button",
                                         label = "HOA",
                                         style = "width: 100px; font-size: 16px; font-weight: bold;
                                         background-color: #B3EE3A",
                                         class = "colored-button")),
                        div(actionButton(inputId = "county_button",
                                         label = "County",
                                         style = "width: 100px; font-size: 16px; font-weight: bold;
                                         background-color: #B3EE3A",
                                         class = "colored-button")),
                        div(actionButton(inputId = "state_button",
                                         label = "Florida",
                                         style = "width: 100px; font-size: 16px; font-weight: bold;
                                         background-color: #B3EE3A",
                                         class = "colored-button")),
                        div(actionButton(inputId = "federal_button",
                                         label = "Federal",
                                         style = "width: 100px; font-size: 16px; font-weight: bold;
                                         background-color: #B3EE3A",
                                         class = "colored-button"))
                        ),
                    width = 2
                ),
                mainPanel(
                    uiOutput(outputId = "hoa_gov_info")
                )
            )
        ),
       #  ABOUT THIS SITE ----
        tabPanel(
            title = "About This Site",
            fluidRow(
                column(3,
                       tags$img(src = "profile_image_2.jpg", style = "width: 100%; margin-bottom: 10px;"),
                       tags$img(src = "linked_in.jpg", style = "width: 10%"),
                       tags$span(style = "margin-left: 10px; vertical-align: middle;", # Adjust spacing as needed
                                 tags$a(href = "https://www.linkedin.com/in/charleshknell/", "See Profile, or")
                       ),
                       tags$div(style = "margin-top: 5px;", # Add space between the links
                                tags$a(href = "mailto:chasknell@icloud.com", "Send me an email")
                       )
                ),
                column(6,
                       tags$div(
                           tags$p("Emerald Gardens Information and Analysis Web Site",
                                  style = "font-size: 40px; color: green;")
                        ),
                       tags$p("I engineered this web site as an exercise in data gathering, transformation,
                              analysis and presentation. All the information presented here is from publicly-available
                              data, mainly from the Sarasota County Property Appraiser's web site."),
                       tags$p("This site was created using the R programming language and the Shiny web application
                              framework. The visualizations were made using the ggplot and plotly libraries."),
                       tags$p("The maps use shape files consisting of sets of points of latitude and longitude
                              which define the outline of a property's lot. Each lot has a tooltip which will
                              display information when you hover over the shape with your mouse.
                              If you click on a lot you will see further details related to the lot and
                              the context of the map format."),
                       tags$p("Data charts with a pale yellow-green background are interactive.
                              Hover over a point with your mouse to see details of the meaning of the point."),
                       tags$p(tags$strong("Site Updates")),
                       tags$p("Some data is very stable, the names of the streets, for example,
                              while the ownership and sales data will change relatively frequently.
                              Over the past decade there have been 2 to 10 sales annually.
                              This data will be updated monthly. The HOA and Government information will be
                              updated after each election or when a significant change comes to my attention."),
                       tags$p("Charles Knell, October 15, 2025")
                ),
             )
        )
    )
)

server <- function(input, output, session) {

    # MAPS PANEL SERVER FUNCTIONS --- BEGIN ----

    # Define explanatory text for each map type ----
    map_explanation_text <- list(
        "Owner Occupied" = "<b>Explanation:</b><br>
        <i>When the street address of the property and the mailing address of the owner is different,
        a rental property is inferred.</i><br><br>
        <b>Drill down:</b><br>
        <i><b>Hover over</b> the center of the lot to see the owners' names</i>.<br><br>
        <i><b>Click on</b> the center of a lot for more details.</i>",
        "Has Pool" = "<b>Explanation:</b><br>
        <i>This map indicates which properties have a swimming pool.<br><br></i><br>
        <b>Drill down:</b><br>
        <i><b>Hover over</b> the lot to see details of the pool.</i><br><br>
        <i><b>Click on</b> the center of a lot for more details on the pool and other exterior features.</i>",
        "Recent Sale" = "<b>Explanation:</b><br>
        <i>This map color-codes the properties in groups by the period in which each was most recently sold.<br><br>
        Unusally low prices (e.g., $100) indicate a transfer, usually to a trust to avoid probate.<br></i><br><br>
        <b>Drill down:</b><br><i><b>Hover over</b> the
        lot to see the most recent sale or transfer information.</i><br><br>
        <i><b>Click on</b> the center of a lot for a history of sales.</i>",
        "Assessed Value per Sq.Ft." = "<b>Explanation:</b><br>
        <i>This map displays the assessed property value per square foot of the building.<br><br>
        <b>Drill down:</b><br>
        <b>Hover over</b> the lot to see the details of the most recent assessment divided between the
        structure and the land,</i><br><br><i><b>Click on</b> the center of a lot for an assessment history.</i>",
        "Effective Year Built" = "<b>Explanation:</b><br><i>This map shows the effective year built
        color-coded in five-year periods.<br></i><br><br>
        <b>Drill down:</b><br>
        <b><i>Hover over</b> the lot.<br><br>
        <b>Click on</b> the center of a lot for more details.</i>",
        "Roofs and Building Permits" = "<b>Explanation:</b><br><i>This map shows the year of the most
        recent roof replacement building permit, color-coded in five-year periods.<br></i><br><br>
        <b>Drill down:</b><br>
        <b><i>Hover over</b> the lot for the date of the most recent roof replacement.<br><br>
        <b>Click on</b> the center of a lot for details on all building permits issued for this address.</i>")

    # Store pre-loaded plots in a list ----
    map_list <- list(
        "Owner Occupied" = owner_occupied_map,
        "Has Pool" = pool_map,
        "Recent Sale" = last_sale_map,
        "Assessed Value per Sq.Ft." = assessed_pr_sqft_map,
        "Effective Year Built" = effective_year_built_map,
        "Roofs and Building Permits" = roof_map,
        "Fly To Emerald Gardens" = NULL # Placeholder for leaflet map
    )

    # Reactive expression to get the current map selection ----
    current_map <- reactive({
        input$views
    })
    # Render leaflet map ----
    output$emerald_gardens_map <- renderLeaflet({
        req(input$views)
        if (input$views == "Fly To Emerald Gardens") {
            zoom_to_map_coords()
        }
    })

    # Render selected plotly map ----
    output$map_output <- renderPlotly({
        req(input$views)  # Ensure an option is selected
        map_list[[current_map()]]

    })

    # Display explanation for selected map ----
    output$map_explanation <- renderUI({
        req(input$views)
        HTML(map_explanation_text[[current_map()]])
    })

    # Display modal popups on shape click ----

    observeEvent(input$`plotly_click-A`, {
        req(input$`plotly_click-A`)

        # Extract account number from customdata ----
        account_num <- str_match(string = input$`plotly_click-A`,
                                 pattern = '"customdata":"(\\d{10})"')[, 2]
        req(account_num)  # Ensure the account_num is not NULL

        # Get the current map name ----
        current_map_name <- current_map()
        # print(paste("Current Map:", current_map_name))  # Debugging log
        # print(paste("Account Number:", account_num))    # Debugging log

        # Call the appropriate modal function based on the map ----
        modal_content <- switch(current_map_name,
                                "Owner Occupied" = make_ownership_table(account_num),
                                "Has Pool" = make_extra_features_table(account_num),
                                "Recent Sale" = make_gt_xfer_history_tbl(account_num),
                                "Assessed Value per Sq.Ft." = make_gt_valuation_history_tbl(account_num),
                                "Effective Year Built" = make_buildings_details_tbl(account_num),
                                "Roofs and Building Permits" = fetch_building_permit_DT(account_num),
                                NULL)  # Handle unexpected map names

        # Show the modal if content is available ----
        if (!is.null(modal_content)) {
            # print(paste("Modal content before processing: ", modal_content))
            # print(str(modal_content))

            # Convert gt or DT tables to HTML
            if (inherits(modal_content, "gt_tbl")) {
                modal_content <- as.character(gt::as_raw_html(modal_content))

                showModal(modalDialog(
                    HTML(modal_content)
                    )
                )
            }else if(inherits(modal_content, "datatables")){
                showModal(modalDialog(
                            DT::dataTableOutput("modal_dt")
                        )
                )
                output$modal_dt <- DT::renderDataTable({
                    modal_content
                })
            }
        }
})


    # MAPS PANEL SERVER FUNCTIONS --- END ----

    # PROPERTY DETAILS TAB SERVER FUNCTIONS --- BEGIN ----
    # Load the property details DT table ----
#
#     eg_DT_ssf <- read_rds("02_processed_data/eg_gt_ssf.rds") %>%
#         mutate(RowID = row_number(), .before = everything())

    # Reactive expression to filter data based on street selection ----
    filtered_data <- reactive({

        data <- eg_DT_ssf

        if (input$filter_by_street != "All" && !is.null(input$filter_by_street) && input$filter_by_street != "") {
            data <- subset(data, grepl(input$filter_by_street, `Street Name`))
        } else {
            data <- eg_DT_ssf
        }
        req(nrow(data) > 0) # Ensure there's at least one row before returning
        return(data)
    })

    # Render the DataTable for the filtered data ----
    output$property_table <- renderDT({

        data <- filtered_data() # store the data in a variable.

        if(nrow(data) > 0){
            dt <- datatable(
                data,
                selection = list(mode = "single", target = "row"),
                rownames = FALSE,
                options = list(
                    dom = "lftip",
                    columnDefs = list(
                        list(targets = c(0, 1), visible = FALSE)#,  # Hide both columns 0 and 1
                        #                    list(targets = c(0, 1), className = "dt-body-center") # Optionally, to center content
                    )
                )
            ) %>%
            formatCurrency(
                columns = c("Lot Size (sq.ft.)", "House Size (sq.ft.)"),
                currency = "",
                digits = 0,
                mark = ","
            ) %>%
            formatCurrency(
                columns = "Recorded Consideration",
                currency = "$",
                digits = 0,
                mark = ","
            )
        return(dt)
        }else{
            # Display a message when no data is available
             no_data_message <- data.frame(Message = "No data available for the selected filter.")
            return(DT::datatable(no_data_message, options = list(dom = 't'), selection = 'none'))
        }
    })

    # Capture the account number from the selected DT row ----
    selected_account <- reactive({
        selected_row <- input$property_table_rows_selected

        if (is.null(selected_row) || length(selected_row) == 0) {
            return(NULL)  # Return NULL if no row is selected
        }

        account_value <- filtered_data()[selected_row, "Account", drop = TRUE]

        if (is.null(account_value) || length(account_value) == 0) {
            return(NULL)  # Return NULL if the account value cannot be retrieved
        }

        return(account_value)  # Return only the account number
    })

    # Display the sales and transfers history for a property ----
    output$s_and_t_history_table <- render_gt({
        account = selected_account()
        if(is.null(account)){
            return(gt(data.frame(message = "Select a row to see the details.")) %>%
                       cols_label(message = ""))
        }
        return_transfers_history_table(account)
    })

    # Display the assessments history for a property ----
    output$assessments_table <- render_gt({
        account = selected_account()
        if(is.null(account)){
            return(gt(data.frame(message = "Select a row to see the details.")) %>%
                       cols_label(message = ""))
        }
        return_assessed_value_history_table(account)
    })

    # Display the information on the additional structures for a property ----
    output$buildings_property_table <- render_gt({
        account = selected_account()
        if(is.null(account)){
            return(gt(data.frame(message = "Select a row to see the details.")) %>%
                       cols_label(message = ""))
        }
        combined_buildings_property_info(account)
    })
    # PROPERTY DETAILS TAB SERVER FUNCTIONS --- END ----

    # VALUATION TAB PLOT SERVER FUNCTIONS --- BEGIN ----
    # PLOTS ----
    output$top_row_plot <- renderPlotly({
        read_rds("03_plot_files/model_dot_plot.rds")
    })

    output$bottom_left_plot <- renderPlot({
        read_rds("03_plot_files/q-q_residuals_plot.rds")
    })

    output$bottom_right_plot <- renderPlot({
        read_rds("03_plot_files/residuals_scatter_plot.rds")
    })

    output$model_summary <- renderUI({
        HTML(create_summary_table_HTML(create_glm_4a_log()))
    })

    # PLOT EXPLANATIONS (appear when action buttons are clicked) ----
    observeEvent(input$actual_vs_predicted, {
        showModal(modalDialog(
            size = "l",
            title = "Actual vs. Predicted",
            HTML(read_file("03_plot_files/just_values_explanation_chatgpt.html"))),
        )
    })

    observeEvent(input$`qq-plot`, {
        showModal(modalDialog(
            size = "l",
            title = "Q-Q Plot",
            HTML(read_file("03_plot_files/q-q_plot_explanation_chatgpt.html"))
        )
        )
    })

    observeEvent(input$`residuals-plot`, {
        showModal(modalDialog(
            size = "l",
            title = "Residuals Plot",
            HTML(read_file("03_plot_files/residuals_scatter_plot_explanation_chatgpt.html"))
            )
        )
    })

    observeEvent(input$summary, {
       # print("summary clicked")
        showModal(modalDialog(
            size = "l",
            title = "Summary",
            HTML(read_file("03_plot_files/model_explanation_chatgpt.html")))
        )
    })
    # VALUATION TAB PLOT SERVER FUNCTIONS --- END ----

    # ESTIMATE JUST VALUE TAB SERVER FUNCTIONS --- BEGIN ----

    observeEvent(input$filter_by_street_jv, { # Correct inputId
        tryCatch({
            filtered_addresses <- property_info_pages_tbl[[2]] %>%
                bind_rows() %>%
                mutate(street_addr = str_remove(.$situs_addr, " SARASOTA, FL, 34233")) %>%
                mutate(house_num = str_extract(string = street_addr, pattern = "^\\d{4}") %>%
                           as.numeric()) %>%
                mutate(street_name = str_extract(
                    string = street_addr,
                    pattern = "DIAMOND CIR W|DIAMOND CIR E|DIAMOND CIR N|DIAMOND CIR S|TOPAZ CT|OPAL CT"
                )) %>%
                arrange(., street_name, house_num) %>%
                mutate(`Street Address` = paste(house_num, street_name))
        }, error = function(e){
            cat("Error in bind_rows() for Estimate Just Values\n")
            cat(conditionMessage(e), "\n")
        })


        if (input$filter_by_street_jv != "All") { # Correct inputId
            filtered_addresses <- filtered_addresses %>%
                filter(street_name == input$filter_by_street_jv) # Correct inputId
        }

        output$house_addr_select <- renderUI({
            selectInput(
                inputId = "house_address",
                label = "Select House Address",
                choices = filtered_addresses %>%
                    select(`Street Address`) %>%
                    pull() # Extract as a vector
            )
        })
    })

    expected_just_bounds <- reactive(
        if(!is.null(input$house_address)){
            generate_expected_just_range(create_glm_4a_log()) %>%
                filter(addr == input$house_address) %>%
                mutate(
                    `Just value` = scales::dollar(log_just_exp),
                    `Lot Size` = scales::comma(exp(log_lot_size)),
                    `Living Area` =  scales::comma(exp(log_living_area)),
                    `Has Pool` = has_pool,
                    Bathrooms = bathrooms,
                    `Half-baths` = half_baths,
                    `Effective Year Built` = eff_yr_built,
                    high = scales::dollar(.$upper_2sigma_exp),
                    mean = scales::dollar(.$fitted_exp),
                    low = scales::dollar(.$lower_2sigma_exp)
                ) %>%
                select(`Just value`, `Lot Size`, `Living Area`, `Has Pool`, Bathrooms, `Half-baths`, `Effective Year Built`, high, mean, low)
        } else {
            NULL
        }
    )

    # Function to generate the right column of cards ----
    output$expected_just_cards <- renderUI({
        bounds <- expected_just_bounds()

        if (!is.null(bounds)) {
            tags$div(
                tags$div(style = "text-align: center; font-size: 16px; font-weight: bold; width: 200px;",
                         "Predicted Values:"),
                tags$div(
                    class = "card black-border-card rounded",
                    style = "width: 200px; background-color: #ffb5c5;",
                    tags$div(
                        class = "card-body",
                        tags$h6(class = "card-title", "High:", style = "text-align: center;"),
                        tags$p(class = "card-text", bounds$high, style = "text-align: center; font-size: 16px;")
                    )
                ),
                tags$div(
                    class = "card bg-light black-border-card rounded", # Style as needed
                    style = "width: 200px; background-color: #54FF9F;",
                    tags$div(
                        class = "card-body",
                        tags$h6(class = "card-title", "Mean:", style = "text-align: center;"),
                        tags$p(class = "card-text", bounds$mean, style = "text-align: center; font-size: 16px;")
                    )
                ),
                tags$div(
                    class = "card bg-light black-border-card rounded",
                    style = "width: 200px; background-color: #FFF5EE;",
                    tags$div(
                        class = "card-body",
                        tags$h6(class = "card-title", "Low:", style = "text-align: center;"),
                        tags$p(class = "card-text", bounds$low, style = "text-align: center; font-size: 16px;")
                    )
                )
            )
        } else {
            tags$p("No expected just bounds available.")
        }
    })

    # Function to create left column of cards ----
    output$model_estimation_text <- renderUI({
        params <- expected_just_bounds()

        if (!is.null(params) && nrow(params) > 0) {
            tags$div(
                class = "row",
                tags$div(
                    class = "col-md-6",
                    tags$div(
                        tags$div(
                            class = "card bg-info black-border-card rounded",
                            style = "width: 200px;",
                            tags$div(
                                class = "card-body",
                                tags$h6(class = "card-title", "Lot size:", style = "text-align: center;"),
                                tags$p(class = "card-text", params$`Lot Size`,
                                       style = "text-align: center; font-size: 16px;")
                            )
                        ),
                        tags$div(
                            class = "card bg-info black-border-card rounded",
                            style = "width: 200px;",
                            tags$div(
                                class = "card-body",
                                tags$h6(class = "card-title", "Living Area:", style = "text-align: center;"),
                                tags$p(class = "card-text", params$`Living Area`,
                                       style = "text-align: center; font-size: 16px;")
                            )
                        ),
                        tags$div(
                            class = "card bg-info black-border-card rounded",
                            style = "width: 200px;",
                            tags$div(
                                class = "card-body",
                                tags$h6(class = "card-title", "Bathrooms:", style = "text-align: center;"),
                                tags$p(class = "card-text", params$`Bathrooms`,
                                       style = "text-align: center; font-size: 16px;")
                            )
                        ),
                        tags$div(
                            class = "card bg-info black-border-card rounded",
                            style = "width: 200px;",
                            tags$div(
                                class = "card-body",
                                tags$h6(class = "card-title", "Half-baths:", style = "text-align: center;"),
                                tags$p(class = "card-text", params$`Half-baths`,
                                       style = "text-align: center; font-size: 16px;")
                            )
                        ),
                        tags$div(
                            class = "card bg-info black-border-card rounded",
                            style = "width: 200px;",
                            tags$div(
                                class = "card-body",
                                tags$h6(class = "card-title", "Has Pool:", style = "text-align: center;"),
                                tags$p(class = "card-text", params$`Has Pool`,
                                       style = "text-align: center; font-size: 16px;")
                            )
                        ),
                        tags$div(
                            class = "card bg-info black-border-card rounded",
                            style = "width: 200px;",
                            tags$div(
                                class = "card-body",
                                tags$h6(class = "card-title", "Effective Year Built:", style = "text-align: center;"),
                                tags$p(class = "card-text", params$`Effective Year Built`,
                                       style = "text-align: center; font-size: 16px;")
                            )
                        ),
                        tags$div(
                            class = "card bg-info black-border-card rounded",
                            style = "width: 200px;",
                            tags$div(
                                class = "card-body",
                                tags$h6(class = "card-title", "Just Value:", style = "text-align: center;"),
                                tags$p(class = "card-text", params$`Just value`,
                                       style = "text-align: center; font-size: 16px;")
                            )
                        )
                    )
                )
            )
        } else {
            tags$p("No parameters available for the selected address.")
        }
    })

    # Function to display Just Value explanation ----
    output$just_value_estimation <- renderUI({
                html_content <- paste(readLines("03_plot_files/just_value_estimation_explanation.html"), collapse = "\n")
        HTML(html_content)
    })

    output$house_addr_select <- renderUI({
        selectInput(
            inputId = "house_address_sh",
            label = "Select House Address",
            choices = filtered_addresses %>%
                select(`Street Address`) %>%
                pull() # Extract as a vector
        )
    })
    # ESTIMATE JUST VALUE TAB SERVER FUNCTIONS --- END ----

    # SALES HISTORY TAB SERVER FUNCTIONS --- BEGIN ----

    # Display sales by year plot
    output$annual_sales_plot <- renderPlotly({
        read_rds("03_plot_files/interactive_sales_by_year_plot.rds")
    })

    # Individual house sales history ----
    # Initialize a reactiveValues object to store the selected address and account number
    rv_selected_house_address_sh <- reactiveValues(
        selected_address_sh = NULL,
        account_num = NULL
    )

    observeEvent(input$filter_by_street_sh, { # Address selection drop-downs
        tryCatch({
            all_addresses <- property_info_pages_tbl[[2]] %>%
                bind_rows() %>%
                mutate(account = property_info_pages_tbl$account,
                       street_addr = str_remove(.$situs_addr, " SARASOTA, FL, 34233"),
                       street_name = str_extract(
                           string = street_addr,
                           pattern = "DIAMOND CIR W|DIAMOND CIR E|DIAMOND CIR N|DIAMOND CIR S|TOPAZ CT|OPAL CT"
                       ),
                       house_num = str_extract(string = street_addr, pattern = "^\\d{4}") %>% as.numeric(),
                       `Street Address` = paste(house_num, street_name)
                ) %>%
                arrange(., street_name, house_num)

            # Initialize filtered_addresses here
            filtered_addresses <- all_addresses

        }, error = function(e){
            cat("Error in bind_rows() for Sales History street filter.\n")
            cat(conditionMessage(e), "\n")
        })

        if (input$filter_by_street_sh != "All") { # Correct inputId
            filtered_addresses <- filtered_addresses %>%
                filter(street_name == input$filter_by_street_sh) # Correct inputId
        }

        output$house_addr_select_sh <- renderUI({
            selectInput(
                inputId = "house_address_sh",
                label = "Select House Address",
                choices = filtered_addresses %>%
                    select(`Street Address`) %>%
                    pull() # Extract as a vector
            )
        })
    })

    # Observe the house address selection and extract the account number ----
    observeEvent(input$house_address_sh, {
    tryCatch({
        account_num <- property_info_pages_tbl[[2]] %>%
            bind_rows() %>%
            mutate(account = property_info_pages_tbl$account,
                   street_addr = str_remove(.$situs_addr, " SARASOTA, FL, 34233"),
                   street_name = str_extract(
                       string = street_addr,
                       pattern = "DIAMOND CIR W|DIAMOND CIR E|DIAMOND CIR N|DIAMOND CIR S|TOPAZ CT|OPAL CT"
                   ),
                   house_num = str_extract(string = street_addr, pattern = "^\\d{4}") %>% as.numeric(),
                   `Street Address` = paste(house_num, street_name)
            ) %>%
            arrange(., street_name, house_num) %>%
            filter(street_addr == input$house_address_sh) %>%
            select(account) %>%
            pull()
    }, error = function(e) {
        cat("Error in bind_rows() for Sales History extract account number.\n")
        cat(conditionMessage(e), "\n")
    })


        rv_selected_house_address_sh$selected_house_address_sh <- input$house_address_sh
        rv_selected_house_address_sh$account_num <- account_num

    })


    # Display plot of the sale history of a specific house ----
    output$house_sale_history_plot <- renderPlotly({

        req(rv_selected_house_address_sh$account_num) # Require a non-NULL account_num
        create_sale_history_plot(rv_selected_house_address_sh$account_num)
    })

    # SALES HISTORY TAB SERVER FUNCTIONS --- END ----

    # HOA AND GOVERNMENT TAB SERVER FUNCTIONS --- BEGIN ----

    hoa_gov_info_selected_button <- reactiveVal(NULL)

    observeEvent(input$hoa_button, {
        hoa_gov_info_selected_button("hoa_button")
    })

    observeEvent(input$county_button, {
        hoa_gov_info_selected_button("county_button")
    })

    observeEvent(input$state_button, {
        hoa_gov_info_selected_button("state_button")
    })

    observeEvent(input$federal_button, {
        hoa_gov_info_selected_button("federal_button")
    })

    output$hoa_gov_info <- renderUI({
        button_clicked <- hoa_gov_info_selected_button()

        if (is.null(button_clicked)) {
            return(tags$p("Click a button for details.", style = "font-size: 22px;")) # Initial message
        }

        if (button_clicked == "hoa_button") {
            tags$iframe(src = "https://www.emeraldgardenshoa.com/",
                        width = "100%",
                        height = "600px",
                        frameborder = "0",
                        scrolling = "yes")
        } else if (button_clicked == "county_button") {
            tags$div(
                tags$p("Due to security restrictions set by the county,
                       the Sarasota County website cannot be displayed directly here.",
                       style = "font-size: 18px;"),
                tags$p("Click here  to open it in a new window.",
                       tags$a(href = "https://www.scgov.net",
                              target = "_blank", "Click here")),
                tags$div(
                    tags$p(tags$strong("This is a list of Sarasota County Public
                                       schools for residents of Emerald Gardens:"))
                ),
                tags$ul(
                    tags$li("Sarasota County High School: 2155 Bahia Vista Street"),
                    tags$li("Sarasota Middle School: 4826 Ashton Road"),
                    tags$li("Ashton Elementary School: 5110 Ashton Road")
                ),
                tags$p("The school system maintains a separate web site. You can reach it at this link:",
                       tags$a(href = "https://www.sarasotacountyschools.net/",
                              target = "_blank", "Click here")),
                tags$div(
                    tags$p(tags$strong("You can go directly to the county web sites for these
                                       agencies at the following links:"))
                ),
                tags$div(
                    tags$p(tags$strong("Sarasota County Sheriff's Office"),
                           tags$a(href = "https://www.sarasotasheriff.org/news_detail_T13_R1576.php",
                                  target = "_blank", "Click here")),
                    tags$p(tags$strong("Sarasota County Courts"),
                           tags$a(href = "https://www.sarasotaclerk.com/",
                                  target = "_blank", "Click here")),
                    tags$p(tags$strong("Sarasota County Property Appraiser"),
                                      tags$a(href = "https://www.sc-pa.com/",
                                             target = "_blank", "Click here")),
                    tags$p(tags$strong("Sarasota County Building Permits"),
                           tags$a(href = "https://www.scgov.net/government/planning-and-development-services/online-permitting",
                                                                      target = "_blank", "Click here")),
                    tags$p(tags$strong("Sarasota County Trash and Recycling"),
                           tags$a(href = "https://www.scgov.net/government/solid-waste/trash-and-recycling",
                                                                         target = "_blank", "Click here"))
                )
            )
        } else if (button_clicked == "state_button") {
            tags$div(
                tags$p(tags$strong("Here are websites for Florida state government.")),
                tags$p("They offer comprehensive information, but they have a deep structure
                       and can be tricky to navigate. You may find it helpful to have these
                       facts in hand before you start."),
                tags$ul(
                    tags$li(tags$strong("Governor Ron DeSantis"),
                            tags$a(href = "https://www.myflorida.com/", target = "_blank", "Click here")),
                    tags$li(tags$strong("State Senate District 22, Senator Joe Gruters"),
                            tags$a(href = "https://www.flsenate.gov/", target = "_blank", "Click here")),
                    tags$li(tags$strong("State House District 74, Representative James Buchanan"),
                            tags$a(href = "https://www.flhouse.gov", target = "_blank", "Click here")),
                    tags$li(tags$strong("Florida Attorney General"),
                            tags$a(href = "https://www.myfloridalegal.com/", target = "_blank", "Click here"))
                )
            )
        } else if(button_clicked == "federal_button"){

            tags$div(
                tags$div(tags$p(tags$strong("Legislative Branch")), style = "font-size: 22px;"),
                tags$p(tags$strong("U.S. Senior Senator Rick Scott"),
                       tags$a(href = "https://www.rickscott.senate.gov/", target = "_blank", "Click here")),
                tags$p(tags$strong("U.S. Junior Senator Ashley Moody"),
                       tags$a(href = "https://www.moody.senate.gov/", target = "_blank", "Click here")),
                tags$p(tags$strong("U.S. Representative W. Gregory Stube, Florida 17"),
                       tags$a(href = "https://steube.house.gov/", target = "_blank", "Click here")),
                tags$br(),
                tags$div(
                    tags$p(tags$strong("Frequently-consulted Federal Agencies:"), style = "font-size: 22px;"),
                    tags$p(tags$strong("Social Security Administration:")),
                    tags$p("2001 Siesta Drive, Suite 301"),
                    tags$p("(800) 772-1213"),
                    tags$p("Web site:", tags$a(href = "https://www.ssa.gov/", target = "_blank", "Click here")),
                    tags$br(),
                    tags$p(tags$strong("Veterans Administration (Sarasota Vet Center):")),
                    tags$p("4801 Swift Road, Suite A"),
                    tags$p("(941) 927-8285"),
                    tags$p("Web site:", tags$a(href = "https://www.va.gov/sarasota-vet-center/",
                                               target = "_blank", "Click here")),
                    tags$br(),
                    tags$p(tags$strong("FEMA:"),
                           tags$p("(800) 621-3362"),
                           tags$p("Web site:", tags$a(href = "https://www.fema.gov/",target = "_blank", "Click here"))
                    )
                 )
            )
        }
    })

    # HOA AND GOVERNMENT TAB SERVER FUNCTIONS --- END ----

    # ABOUT THIS SITE --- NO SERVER FUNCTIONS FOR THIS TAB ----

}

# Run the application
shinyApp(ui = ui, server = server)
