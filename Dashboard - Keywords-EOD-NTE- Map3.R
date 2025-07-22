required_packages <- c(
  "shiny", "ggplot2", "dplyr", "haven", "scales", "stringr", "forcats", "RColorBrewer", "tidyr",
  "DT", "shinycssloaders", "shinyWidgets", "rnaturalearth", "rnaturalearthdata", "sf", "leaflet", "shinydashboard", "networkD3"
)

installed <- rownames(installed.packages())
for(pkg in required_packages) {
  if(!pkg %in% installed) { install.packages(pkg, dependencies = TRUE) }
}

library(shiny)
library(ggplot2)
library(dplyr)
library(haven)
library(scales)
library(stringr)
library(forcats)
library(RColorBrewer)
library(tidyr)
library(DT)
library(shinycssloaders)
library(shinyWidgets)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
library(leaflet)
library(shinydashboard)
library(networkD3)

get_palette <- function(n, pal) {
  info <- RColorBrewer::brewer.pal.info
  min_n <- 3
  max_n <- info[pal, "maxcolors"]
  if (n < min_n) {
    cols <- RColorBrewer::brewer.pal(min_n, pal)[1:n]
  } else {
    cols <- RColorBrewer::brewer.pal(min(max_n, n), pal)
  }
  return(cols)
}

ui <- fluidPage(
  titlePanel("Seeds-Related Project Dashboard"),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/admin-lte/2.0.6/css/AdminLTE.min.css"),
    tags$style(HTML("
      .main-container { background-color: #f9f9f9; }
      .tab-pane { padding-top: 20px; }
      .shiny-output-error { color: red; }
      .well { background-color: #eaf2f8; }
      .custom-tab-color .nav-tabs > li > a {
        color: #fff !important;
        font-weight: bold;
      }
    "))
  ),
  sidebarLayout(
    sidebarPanel(
      helpText("Portfolio Analysis:"),
      radioButtons(
        inputId = "metric_select", 
        label = "Show metric on all graphs:", 
        choices = c("Budget", "Number of Projects"), 
        selected = "Budget", 
        inline = TRUE
      ),
      pickerInput(
        "palette", 
        "Choose a color palette:",
        choices = c("Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Dark2", "Accent", "Paired"),
        selected = "Set3",
        multiple = FALSE, options = list(style = "btn-info")
      ),
      pickerInput("region", "Select Region:", choices = NULL, selected = "All", options = list(`live-search`=TRUE)),
      uiOutput("eod_year_selector"),
      uiOutput("nte_year_selector"),
      textInput("keyword_search", "Search (multi-word, case-insensitive):", value = "", placeholder = "Enter keywords separated by space")
    ),
    mainPanel(
      fluidRow(
        valueBoxOutput("total_projects", width = 3),
        valueBoxOutput("total_budget", width = 3),
        valueBoxOutput("total_countries", width = 3),
        valueBoxOutput("total_years", width = 3)
      ),
      tabsetPanel(
        tabPanel(
          "World Map",
          withSpinner(leafletOutput("worldMapPlot", height = "600px")),
          withSpinner(DT::dataTableOutput("countryProjectsTable"))
        ),
        tabPanel(
          "FAO Portfolio Comparison",
          fluidRow(
            column(12, h4("Seeds Budget in FAO Total Portfolio"),
                   withSpinner(plotOutput("seedsFAOPie"))
            ),
            column(12, h4("Trends: Seeds & FAO Portfolio Budgets"),
                   withSpinner(plotOutput("trend_budget")),
                   withSpinner(plotOutput("trend_emergency_budget")),
                   withSpinner(plotOutput("trend_development_budget")),
                   withSpinner(plotOutput("trend_project_count"))
            )
          )
        ),
        tabPanel(
          "Regional Analysis", 
          fluidRow(
            column(6, withSpinner(plotOutput("regionalPie"))),
            column(6, withSpinner(plotOutput("regionalBar")))
          ),
          fluidRow(
            column(12, withSpinner(plotOutput("regionalTypeBar")))
          )
        ),
        tabPanel(
          "Country Analysis", 
          fluidRow(
            column(6, withSpinner(plotOutput("top_countries_bar"))),
            column(6, withSpinner(plotOutput("top_countries_type_bar")))
          )
        ),
        tabPanel(
          "Donor Analysis", 
          fluidRow(
            column(6, withSpinner(plotOutput("donorPie"))),
            column(6, withSpinner(DT::dataTableOutput("donorTable")))
          )
        ),
        tabPanel(
          "Operating Unit Analysis", 
          withSpinner(plotOutput("donorBar")) 
        ),
        tabPanel(
          "Betters / PPAs / SDGs Analysis",
          fluidRow(
            column(12, h4("Top 7 Betters by Seeds-related Project Budget")),
            column(12, withSpinner(plotOutput("bettersPlot")))
          ),
          fluidRow(
            column(12, h4("Top 7 PPAs by Seeds-related Project Budget")),
            column(12, withSpinner(plotOutput("ppasPlot")))
          ),
          fluidRow(
            column(12, h4("Top 7 SDGs by Seeds-related Project Budget")),
            column(12, withSpinner(plotOutput("sdgsPlot")))
          )
        ),
        tabPanel(
          "Project Explorer",
          fluidRow(
            column(12, 
                   DT::dataTableOutput("projectExplorer"),
                   downloadButton("downloadProjects", "Download as CSV")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # Data Loading and Preprocessing (ONE TIME)
  username <- Sys.getenv("USERNAME")
  drive <- if (tolower(username) == "sureka") {
    "C:/Users/Sureka/OneDrive - Food and Agriculture Organization/OED-Evaluations-eLibrary WorkSpace - Seeds"
  } else if (tolower(username) == "hp") {
    "C:/Users/hp/OneDrive - Food and Agriculture Organization/OED-Evaluations-eLibrary WorkSpace - Seeds"
  } else {
    "T:"
  }
  portfolio <- file.path(drive, "04 INCEPTION", "06 PORTFOLIO ANALYSIS", "Preliminary analysis")
  base_data <- read_dta(file.path(portfolio, "portfolio_final.dta")) |>
    filter(DWHBudget != 0) |>
    mutate(
      emerg = Emergency == "Yes",
      dev = Emergency == "No",
      n = 1L,
      DWHBudget = DWHBudget / 1e6,
      emer_budget = ifelse(emerg, DWHBudget, 0),
      dev_budget = ifelse(dev, DWHBudget, 0),
      year_EOD = as.integer(substr(as.character(EOD), 7, 10)),
      year_NTE = as.integer(substr(as.character(NTE), 7, 10)),
      Recipientregion = recode(
        Recipientregion,
        "Global/Interregional" = "Global",
        "Latin America" = "Latin_America",
        "Near East" = "Near_East"
      )
    )
  seeds_data <- base_data |>
    filter(`_merge` == 3 | keyword == 1)
  raw_data <- seeds_data |>
    mutate(
      Donors = recode(Donors,
        "Food and Agriculture Organization of the UN (FAO)" = "FAO",
        "United Nations Office for the Coordination of Humanitarian Affairs (OCHA) Pooled Funds" = "UNOCHA pooled funds",
        "United States of America" = "USA",
        "European Union" = "EU",
        "Democratic Republic of the Congo" = "DRC",
        "World Food Programme Administered Trust Fund" = "WFP Trust Fund",
        "Netherlands (Kingdom of the)" = "Netherlands",
        "United Nations Children's Fund (UNICEF)" = "UNICEF",
        "UNDP Administered Donor Joint Trust Fund (UNJ)" = "UNDP(UNJ)",
        "GEF (FAO) - Global Environment Facility (through FAO)" = "GEF(FAO)",
        "Bill and Melinda Gates Foundation" = "Gates Foundation",
        "GCF - Green Climate Fund (Accreditation Master Agreement - AMA)" = "GCF(AMA)",
        "UND - UNDP Administered Trust Funds" = "UND-UNDP",
        "United Kingdom of Great Britain and Northern Ireland" = "UK",
        "Asian Development Bank (ADB)"="ADB",
        "World Bank (WB)"="WB"
      )
    )
  donor_summary <- raw_data |>
    group_by(Donors, Recipientregion) |>
    summarise(
      DWHBudget = sum(DWHBudget, na.rm = TRUE),
      n = sum(n), .groups = "drop"
    ) |>
    mutate(
      Recipientregion = gsub(" ", "", Recipientregion),
      Recipientregion = gsub("/", "", Recipientregion)
    )

  # UI Controls
  observe({
    all_regions <- sort(unique(donor_summary$Recipientregion))
    updatePickerInput(session, "region", choices = c("All", all_regions), selected = "All")
  })
  output$eod_year_selector <- renderUI({
    years <- sort(unique(na.omit(seeds_data$year_EOD)))
    pickerInput("years_eod", "Select EOD Year(s):", choices = years, selected = years, multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search`=TRUE, style = "btn-info"))
  })
  output$nte_year_selector <- renderUI({
    years <- sort(unique(na.omit(seeds_data$year_NTE)))
    pickerInput("years_nte", "Select NTE Year(s):", choices = years, selected = years, multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search`=TRUE, style = "btn-info"))
  })

  # Filtering
  filter_by_keywords <- function(df, keywords, columns = c("Title", "Objectives")) {
    if (is.null(keywords) || trimws(keywords) == "") return(df)
    keys <- trimws(unlist(strsplit(keywords, "\\s+")))
    if (length(keys) == 0 || all(keys == "")) return(df)
    mask <- apply(sapply(keys, function(k) {
      rowSums(sapply(columns, function(col) grepl(k, tolower(as.character(df[[col]])), fixed = TRUE))) > 0
    }), 1, all)
    df[mask, ]
  }
  filtered_data <- reactive({
    dat <- seeds_data
    if (!is.null(input$region) && input$region != "All") {
      dat <- dat |> filter(Recipientregion == input$region)
    }
    if (!is.null(input$years_eod) && length(input$years_eod) > 0) {
      dat <- dat |> filter(year_EOD %in% input$years_eod)
    }
    if (!is.null(input$years_nte) && length(input$years_nte) > 0) {
      dat <- dat |> filter(year_NTE %in% input$years_nte)
    }
    filter_by_keywords(dat, input$keyword_search)
  })
  filtered_summary_data <- reactive({
    filtered_data() |>
      group_by(Recipientregion) |>
      summarise(
        n = sum(n),
        emerg = sum(emerg),
        dev = sum(dev),
        DWHBudget = sum(DWHBudget, na.rm = TRUE),
        emer_budget = sum(emer_budget, na.rm = TRUE),
        dev_budget = sum(dev_budget, na.rm = TRUE),
        .groups = "drop"
      )
  })
  filtered_donor_data <- reactive({
    filtered_data() |>
      group_by(OperatingUnit) |>
      summarise(
        n = sum(n),
        emerg = sum(emerg),
        dev = sum(dev),
        DWHBudget = sum(DWHBudget, na.rm = TRUE),
        emer_budget = sum(emer_budget, na.rm = TRUE),
        dev_budget = sum(dev_budget, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(DWHBudget)) |>
      slice_head(n = 11)
  })
  # Helper to get palette colors as hex
  get_palette_hex <- function(n, pal) {
    info <- RColorBrewer::brewer.pal.info
    min_n <- 3
    max_n <- info[pal, "maxcolors"]
    if (n < min_n) cols <- RColorBrewer::brewer.pal(min_n, pal)[1:n]
    else cols <- RColorBrewer::brewer.pal(min(max_n, n), pal)
    cols
  }
  
  # Dynamic tab color CSS
  output$tab_color_style <- renderUI({
    pal <- input$palette
    tab_names <- c(
      "Regional Analysis", "Operating Unit Analysis", "Donor Analysis", "Country Analysis",
      "Betters / PPAs / SDGs Analysis", "FAO Portfolio Comparison", "Project Explorer", "World Map"
    )
    cols <- get_palette_hex(length(tab_names), pal)
    css <- ""
    for(i in seq_along(tab_names)) {
      css <- paste0(
        css,
        sprintf(
          ".custom-tab-color .nav-tabs > li:nth-child(%d) > a { background-color: %s !important; }\n",
          i, cols[i]
        )
      )
    }
    tags$style(HTML(css))
  })

  # Value Boxes
  output$total_projects <- renderValueBox({
    valueBox(
      value = formatC(nrow(filtered_data()), format="d", big.mark=","), 
      subtitle = "Total Projects", 
      icon = icon("folder-open"),
      color = "blue"
    )
  })
  output$total_budget <- renderValueBox({
    valueBox(
      value = paste0("$", formatC(sum(filtered_data()$DWHBudget, na.rm=TRUE), format="f", digits=2, big.mark=","), "M"),
      subtitle = "Total Budget",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  output$total_countries <- renderValueBox({
    valueBox(
      value = length(unique(filtered_data()$Recipients)),
      subtitle = "Countries Covered",
      icon = icon("globe"),
      color = "orange"
    )
  })
  output$total_years <- renderValueBox({
    fd <- filtered_data()
    yrs <- range(c(fd$year_EOD, fd$year_NTE), na.rm = TRUE)
    valueBox(
      value = paste0(yrs[1], " - ", yrs[2]),
      subtitle = "Year Range",
      icon = icon("calendar"),
      color = "purple"
    )
  })

  # Project Explorer Table
  output$projectExplorer <- DT::renderDataTable({
    dat <- filtered_data() |>
      select(Symbol, Title, Recipients, Donors, year_EOD, year_NTE, DWHBudget, Emergency)
    DT::datatable(dat, options=list(pageLength=10, scrollX=TRUE), filter="top", rownames=FALSE)
  })
  output$downloadProjects <- downloadHandler(
    filename = function() paste0("projects-", Sys.Date(), ".csv"),
    content = function(file) {
      dat <- filtered_data() |> select(Symbol, Title, Recipients, Donors, year_EOD, year_NTE, DWHBudget, Emergency)
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
  # Donor/Country/SDG Profiles
  observeEvent(input$donorTable_rows_selected, {
    donor_dt <- if (is.null(input$region) || input$region == "All") {
      donor_summary %>% group_by(Donors) %>% summarise(DWHBudget = sum(DWHBudget, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop")
    } else {
      donor_summary %>% filter(Recipientregion == input$region) %>% group_by(Donors) %>% summarise(DWHBudget = sum(DWHBudget, na.rm = TRUE), n = sum(n, na.rm = TRUE), .groups = "drop")
    }
    sel <- input$donorTable_rows_selected
    if (length(sel) > 0) {
      donor_name <- donor_dt$Donors[sel[1]]
      donor_data <- filtered_data() %>% filter(Donors == donor_name)
      showModal(modalDialog(
        title = paste("Donor Profile:", donor_name),
        h4("Total Budget (Million USD):"), sum(donor_data$DWHBudget, na.rm=TRUE),
        h4("Total Projects:"), nrow(donor_data),
        h4("Top Countries:"),
        renderPlot({
          top_countries <- donor_data %>% 
            group_by(Recipients) %>%
            summarise(Budget = sum(DWHBudget, na.rm=TRUE)) %>%
            arrange(desc(Budget)) %>% head(5)
          barplot(top_countries$Budget, names.arg=top_countries$Recipients, col="skyblue")
        }),
        easyClose = TRUE, size = "l"
      ))
    }
  })
  output$donorProfileUI <- renderUI({
    # Space reserved for donor modal logic
    NULL
  })
  
  # Country Profile: click on top countries bar activates modal
  observeEvent(input$top_countries_budget_bar_click, {
    click <- input$top_countries_budget_bar_click
    if (!is.null(click$x)) {
      dat <- filtered_countries_data()
      idx <- round(click$x)
      if (idx > 0 && idx <= nrow(dat)) {
        country_name <- dat$Recipients[idx]
        country_data <- filtered_data() %>% filter(Recipients == country_name)
        showModal(modalDialog(
          title = paste("Country Profile:", country_name),
          h4("Total Budget (Million USD):"), sum(country_data$DWHBudget, na.rm=TRUE),
          h4("Total Projects:"), nrow(country_data),
          h4("Top Donors:"),
          renderPlot({
            top_donors <- country_data %>% 
              group_by(Donors) %>%
              summarise(Budget = sum(DWHBudget, na.rm=TRUE)) %>%
              arrange(desc(Budget)) %>% head(5)
            barplot(top_donors$Budget, names.arg=top_donors$Donors, col="orange")
          }),
          easyClose = TRUE, size = "l"
        ))
      }
    }
  })
  output$countryProfileUI <- renderUI({
    # Space reserved for country modal logic
    NULL
  })
  
  # SDG Profile: click on SDGs plot activates modal
  observeEvent(input$sdgsPlot_click, {
    click <- input$sdgsPlot_click
    if (!is.null(click$x)) {
      dat <- sdgs_budget()
      idx <- round(click$x)
      if (idx > 0 && idx <= nrow(dat)) {
        sdg_code <- dat$code[idx]
        sdg_data <- filtered_data() %>% filter(grepl(sdg_code, SDG2425))
        showModal(modalDialog(
          title = paste("SDG Profile:", sdg_code),
          h4("Total Budget (Million USD):"), sum(sdg_data$DWHBudget, na.rm=TRUE),
          h4("Total Projects:"), nrow(sdg_data),
          h4("Top Countries:"),
          renderPlot({
            top_countries <- sdg_data %>% 
              group_by(Recipients) %>%
              summarise(Budget = sum(DWHBudget, na.rm=TRUE)) %>%
              arrange(desc(Budget)) %>% head(5)
            barplot(top_countries$Budget, names.arg=top_countries$Recipients, col="green")
          }),
          easyClose = TRUE, size = "l"
        ))
      }
    }
  })
  output$sdgProfileUI <- renderUI({
    # Space reserved for SDG modal logic
    NULL
  })
  output$regionalPie <- renderPlot({
    pie_data <- filtered_data() %>%
      group_by(Recipientregion) %>%
      summarise(
        total_budget = sum(DWHBudget, na.rm = TRUE),
        num_projects = sum(n),
        .groups = "drop"
      )
    req(nrow(pie_data) > 0)
    palette <- get_palette(nrow(pie_data), input$palette)
    if (input$metric_select == "Budget") {
      pie_data <- pie_data %>% mutate(percent = total_budget / sum(total_budget))
      ggplot(pie_data, aes(x = "", y = total_budget, fill = Recipientregion)) +
        geom_col(width = 1) +
        coord_polar("y") +
        geom_text(aes(label = scales::percent(percent)),
                  position = position_stack(vjust = 0.5), color = "black", size = 4, fontface="bold") +
        scale_fill_manual(values = palette) +
        theme_void() +
        labs(title = "Top Regions by Seeds-related Project Budget", fill="Region")
    } else {
      pie_data <- pie_data %>% mutate(percent = num_projects / sum(num_projects))
      ggplot(pie_data, aes(x = "", y = num_projects, fill = Recipientregion)) +
        geom_col(width = 1) +
        coord_polar("y") +
        geom_text(aes(label = scales::percent(percent)),
                  position = position_stack(vjust = 0.5), color = "black", size = 4, fontface="bold") +
        scale_fill_manual(values = palette) +
        theme_void() +
        labs(title = "Top Regions by Number of Seeds-related Projects", fill="Region")
    }
  })
  
  output$regionalBar <- renderPlot({
    dat <- filtered_summary_data()
    req(nrow(dat) > 0)
    palette <- get_palette(nrow(dat), input$palette)
    if (input$metric_select == "Budget") {
      ggplot(dat, aes(x = fct_reorder(Recipientregion, DWHBudget), y = DWHBudget, fill=Recipientregion)) +
        geom_col(show.legend=FALSE) +
        coord_flip() +
        labs(title = "Top Regions by Seeds-related Project Budget", y = "Budget (Million USD)", x = NULL) +
        geom_text(aes(label = sprintf("%.2f", DWHBudget)), hjust = -0.1, color = "black", size = 3) +
        scale_fill_manual(values=palette) +
        theme_minimal()
    } else {
      ggplot(dat, aes(x = fct_reorder(Recipientregion, n), y = n, fill=Recipientregion)) +
        geom_col(show.legend=FALSE) +
        coord_flip() +
        labs(title = "Top Regions by Number of Seeds-related Projects", y = "Number of Projects", x = NULL) +
        geom_text(aes(label = n), hjust = -0.1, color = "black", size = 3) +
        scale_fill_manual(values=palette) +
        theme_minimal()
    }
  })
  
  output$regionalTypeBar <- renderPlot({
    dat <- filtered_summary_data()
    req(nrow(dat) > 0)
    palette <- get_palette(2, input$palette)
    if (input$metric_select == "Budget") {
      summary_data_long <- dat %>%
        select(Recipientregion, emer_budget, dev_budget, DWHBudget) %>%
        pivot_longer(cols = c(emer_budget, dev_budget), names_to = "Type", values_to = "Budget")
      ggplot(summary_data_long, aes(x = fct_reorder(Recipientregion, DWHBudget), y = Budget, fill = Type)) +
        geom_col(position = "dodge") +
        coord_flip() +
        labs(title = "Project Budget (Emergency vs Development)", y = "Budget (Million USD)", x = NULL) +
        scale_fill_manual(
          values = setNames(palette, c("dev_budget", "emer_budget")),
          labels = c("Development", "Emergency")
        ) +
        geom_text(aes(label = sprintf("%.2f", Budget)), position = position_dodge(width = 0.9), hjust = -0.1, color = "black", size = 3) +
        theme_minimal()
    } else {
      summary_data_long2 <- dat %>%
        select(Recipientregion, emerg, dev, DWHBudget) %>%
        pivot_longer(cols = c(emerg, dev), names_to = "Type", values_to = "Count")
      ggplot(summary_data_long2, aes(x = fct_reorder(Recipientregion, DWHBudget), y = Count, fill = Type)) +
        geom_col(position = "dodge") +
        coord_flip() +
        labs(title = "Project Count (Emergency vs Development)", y = "Number of Projects", x = NULL) +
        scale_fill_manual(
          values = setNames(palette, c("dev", "emerg")),
          labels = c("Development", "Emergency")
        ) +
        geom_text(aes(label = Count), position = position_dodge(width = 0.9), hjust = 1.1, color = "black", size = 3) +
        theme_minimal()
    }
  })
  ###### OPERATING UNIT
  output$donorBar <- renderPlot({
    dat <- filtered_donor_data()
    req(nrow(dat) > 0)
    # Choose metric and labels based on selection
    metric_col <- if (input$metric_select == "Budget") "DWHBudget" else "n"
    y_label <- if (input$metric_select == "Budget") "Budget (Million USD)" else "Number of Projects"
    title_label <- if (input$metric_select == "Budget") {
      "Top Operating Units by Seeds-related Budget"
    } else {
      "Top Operating Units by Project Count"
    }
    ggplot(dat, aes(
      x = fct_reorder(OperatingUnit, .data[[metric_col]]), 
      y = .data[[metric_col]], 
      fill = OperatingUnit
    )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = title_label, y = y_label, x = NULL) +
      geom_text(
        aes(label = if (input$metric_select == "Budget") sprintf("%.2f", DWHBudget) else n), 
        hjust = -0.1, color = "black", size = 3
      ) +
      scale_fill_manual(values = get_palette(nrow(dat), input$palette)) +
      theme_minimal()
  })
  
  output$donorPie <- renderPlot({
    req(input$region)
    donor_dt <- if (input$region == "All") {
      donor_summary %>%
        group_by(Donors) %>%
        summarise(
          DWHBudget = sum(DWHBudget, na.rm = TRUE),
          n = sum(n, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      donor_summary %>%
        filter(Recipientregion == input$region) %>%
        group_by(Donors) %>%
        summarise(
          DWHBudget = sum(DWHBudget, na.rm = TRUE),
          n = sum(n, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    donor_dt <- donor_dt %>%
      arrange(desc(DWHBudget)) %>%
      mutate(rank = rank(-DWHBudget, ties.method = "first")) %>%
      mutate(DonorTop10 = ifelse(rank <= 8, Donors, "Other")) %>%
      group_by(DonorTop10) %>%
      summarise(
        DWHBudget = sum(DWHBudget, na.rm = TRUE),
        n = sum(n, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(DWHBudget > 0) %>%
      mutate(percent = DWHBudget / sum(DWHBudget))
    
    req(nrow(donor_dt) > 0)
    palette <- get_palette(length(unique(donor_dt$DonorTop10)), input$palette)
    ggplot(donor_dt, aes(x = "", y = DWHBudget, fill = DonorTop10)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = palette) +
      geom_text(aes(label = scales::percent(percent, accuracy = 1)), 
                position = position_stack(vjust = 0.5), size = 5, color = "black") +
      theme_void() +
      labs(
        title = paste("Top Donors by Budget", 
                      ifelse(input$region == "All", "(All Regions)", paste("-", input$region))),
        fill = "Donor"
      )
  })
  
  output$donorTable <- DT::renderDataTable({
    req(input$region)
    donor_dt <- if (input$region == "All") {
      donor_summary %>%
        group_by(Donors) %>%
        summarise(
          `No. of Projects` = sum(n, na.rm = TRUE),
          `Project Budget (Million USD)` = sum(DWHBudget, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      donor_summary %>%
        filter(Recipientregion == input$region) %>%
        group_by(Donors) %>%
        summarise(
          `No. of Projects` = sum(n, na.rm = TRUE),
          `Project Budget (Million USD)` = sum(DWHBudget, na.rm = TRUE),
          .groups = "drop"
        )
    }
    donor_dt <- donor_dt %>%
      arrange(desc(`Project Budget (Million USD)`)) %>%
      mutate(rank = rank(-`Project Budget (Million USD)`, ties.method = "first")) %>%
      mutate(DonorTop10 = ifelse(rank <= 8, Donors, "Other")) %>%
      group_by(DonorTop10) %>%
      summarise(
        `No. of Projects` = sum(`No. of Projects`),
        `Project Budget (Million USD)` = sum(`Project Budget (Million USD)`),
        .groups = "drop"
      ) %>%
      arrange(desc(`Project Budget (Million USD)`)) %>%
      rename(`Donor Name` = DonorTop10)
    DT::datatable(donor_dt, rownames = FALSE, options = list(pageLength = 10, dom='tip', scrollX=TRUE))
  })
  
  # --- Country Analysis with Region & EOD/NTE Filter + Top 10 Countries Logic ---
  top_countries_data_all <- seeds_data %>%
    mutate(
      emerg = ifelse(Emergency == "Yes", 1, 0),
      dev = ifelse(Emergency == "No", 1, 0),
      n = 1,
      emer_budget = ifelse(emerg == 1, DWHBudget, 0),
      dev_budget = ifelse(dev == 1, DWHBudget, 0),
      Recipients = case_when(
        Recipients == "Central African Republic" ~ "Central African Rep.",
        Recipients == "Democratic Republic of the Congo" ~ "DRC",
        TRUE ~ Recipients
      )
    ) %>%
    group_by(Recipients, Recipientregion, year_EOD, year_NTE) %>%
    summarise(
      n = sum(n),
      emerg = sum(emerg),
      dev = sum(dev),
      DWHBudget = sum(DWHBudget, na.rm = TRUE),
      emer_budget = sum(emer_budget, na.rm = TRUE),
      dev_budget = sum(dev_budget, na.rm = TRUE),
      .groups = "drop"
    )
  
  filtered_countries_data <- reactive({
    dat <- top_countries_data_all
    # Region filter
    if (!is.null(input$region) && input$region != "All") {
      dat <- dat %>% filter(Recipientregion == input$region)
    }
    # Filter by EOD year
    if (!is.null(input$years_eod) && length(input$years_eod) > 0) {
      dat <- dat %>% filter(year_EOD %in% input$years_eod)
    }
    # Filter by NTE year
    if (!is.null(input$years_nte) && length(input$years_nte) > 0) {
      dat <- dat %>% filter(year_NTE %in% input$years_nte)
    }
    # Multi-keyword search (on Recipients)
    dat <- filter_by_keywords(dat, input$keyword_search, columns = c("Recipients"))
    # Top 10 countries by selected metric for the selected region+years
    metric_col <- if (input$metric_select == "Budget") "DWHBudget" else "n"
    dat_sum <- dat %>%
      filter(!grepl(",", Recipients)) %>% # remove entries with multiple countries
      group_by(Recipients) %>%
      summarise(
        n = sum(n),
        emerg = sum(emerg),
        dev = sum(dev),
        DWHBudget = sum(DWHBudget, na.rm = TRUE),
        emer_budget = sum(emer_budget, na.rm = TRUE),
        dev_budget = sum(dev_budget, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(.data[[metric_col]])) %>%
      mutate(rank_metric = row_number())
    top10 <- dat_sum$Recipients[1:min(10, nrow(dat_sum))]
    dat %>%
      filter(Recipients %in% top10) %>%
      group_by(Recipients) %>%
      summarise(
        n = sum(n),
        emerg = sum(emerg),
        dev = sum(dev),
        DWHBudget = sum(DWHBudget, na.rm = TRUE),
        emer_budget = sum(emer_budget, na.rm = TRUE),
        dev_budget = sum(dev_budget, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(.data[[metric_col]])) %>%
      mutate(rank_metric = row_number())
  })
  
  output$top_countries_bar <- renderPlot({
    dat <- filtered_countries_data()
    metric_col <- if (input$metric_select == "Budget") "DWHBudget" else "n"
    metric_label <- if (input$metric_select == "Budget") "Budget in Millions USD" else "Number of Projects"
    title_label <- if (input$metric_select == "Budget") {
      "Top 10 Countries by Seeds-related Project Budget"
    } else {
      "Top 10 Countries by Number of Seeds-related Projects"
    }
    ggplot(dat, aes(x = fct_reorder(Recipients, .data[[metric_col]]), y = .data[[metric_col]], fill = Recipients)) +
      geom_col(show.legend=FALSE) +
      coord_flip() +
      labs(
        title = paste(title_label, ifelse(input$region == "All", "", paste("in", input$region))),
        x = "",
        y = metric_label
      ) +
      geom_text(aes(label = if (input$metric_select == "Budget") sprintf("%.2f", DWHBudget) else n),
                hjust = -0.1, color = "black", size = 3) +
      scale_fill_manual(values=get_palette(nrow(dat), input$palette)) +
      theme_minimal()
  })
  
  output$top_countries_type_bar <- renderPlot({
    dat <- filtered_countries_data()
    palette <- get_palette(2, input$palette)
    if (input$metric_select == "Budget") {
      dat_long <- dat %>%
        select(Recipients, emer_budget, dev_budget, DWHBudget) %>%
        pivot_longer(cols = c(emer_budget, dev_budget), names_to = "Type", values_to = "Budget")
      ggplot(dat_long, aes(x = fct_reorder(Recipients, Budget), y = Budget, fill = Type)) +
        geom_col(position = "dodge") +
        coord_flip() +
        labs(
          title = paste("Top 10 Countries by Seeds-related Project Budget (by Type)",
                        ifelse(input$region == "All", "", paste("in", input$region))),
          x = "",
          y = "Budget in Millions USD"
        ) +
        scale_fill_manual(
          values = setNames(palette, c("dev_budget", "emer_budget")),
          labels = c("Development", "Emergency")
        ) +
        geom_text(aes(label = sprintf("%.2f", Budget)), position = position_dodge(width = 0.9), hjust = -0.1, color = "black", size = 3) +
        theme_minimal()
    } else {
      dat_long2 <- dat %>%
        select(Recipients, emerg, dev, DWHBudget) %>%
        pivot_longer(cols = c(emerg, dev), names_to = "Type", values_to = "Count")
      ggplot(dat_long2, aes(x = fct_reorder(Recipients, Count), y = Count, fill = Type)) +
        geom_col(position = "dodge") +
        coord_flip() +
        labs(
          title = paste("Top 10 Countries by Number of Seeds-related Projects (by Type)",
                        ifelse(input$region == "All", "", paste("in", input$region))),
          x = "",
          y = "Number of Projects"
        ) +
        scale_fill_manual(
          values = setNames(palette, c("dev", "emerg")),
          labels = c("Development", "Emergency")
        ) +
        geom_text(aes(label = Count), position = position_dodge(width = 0.9), hjust = 1.1, color = "black", size = 3) +
        theme_minimal()
    }
  })
  
  parse_tags <- function(df, tag_col, prefix) {
    tag_col <- enquo(tag_col)
    df %>%
      filter(!is.na(!!tag_col), !!tag_col != "") %>%
      mutate(tagstr = gsub("[ %]", "", !!tag_col)) %>%
      mutate(tag_pairs = str_extract_all(tagstr, "[A-Za-z0-9_\\.]+\\([0-9]+\\)")) %>%
      unnest_longer(tag_pairs) %>%
      filter(!is.na(tag_pairs)) %>%
      mutate(
        code = str_extract(tag_pairs, "^[A-Za-z0-9_\\.]+"),
        percent = as.numeric(str_extract(tag_pairs, "(?<=\\()[0-9]+(?=\\))")),
        budget = DWHBudget * percent / 100
      ) %>%
      filter(!is.na(code)) %>%
      select(code, budget)
  }
  
  betters_budget <- reactive({
    b <- parse_tags(filtered_data(), Better2425, "budget_") %>%
      group_by(code) %>%
      summarise(budget = sum(budget, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(budget)) %>%
      slice_head(n = 7)
    b
  })
  
  ppas_budget <- reactive({
    p <- parse_tags(filtered_data(), PPA2425, "budget_PPA_") %>%
      group_by(code) %>%
      summarise(budget = sum(budget, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(budget)) %>%
      slice_head(n = 7)
    p
  })
  
  sdgs_budget <- reactive({
    sdata <- filtered_data() %>%
      mutate(SDG2425 = SDG2425 %>%
               str_replace_all(",", "") %>%
               str_replace_all("Responsibleconsumptionandproduction", "Responsiblecons_prod") %>%
               str_replace_all("Peacejusticeandstronginstitutions", "Peace_justice") %>%
               str_replace_all("Industryinnovationandinfrastructure", "Indus_innov") %>%
               str_replace_all("Sustainablecitiesandcommunities", "Sus_comm"))
    s <- parse_tags(sdata, SDG2425, "b_") %>%
      group_by(code) %>%
      summarise(budget = sum(budget, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(budget)) %>%
      slice_head(n = 7)
    s
  })
  
  output$bettersPlot <- renderPlot({
    dat <- betters_budget()
    palette <- get_palette(nrow(dat), input$palette)
    ggplot(dat, aes(x = fct_reorder(code, budget), y = budget, fill = code)) +
      geom_col(show.legend = TRUE) +
      coord_flip() +
      geom_text(aes(label = sprintf("%.2f", budget)), hjust = -0.1, color = "black", size = 4) +
      scale_fill_manual(values = palette) +
      labs(
        title = "Top 7 Betters by Seeds-related Project Budget",
        x = "Better",
        y = "Budget in Millions USD",
        fill = "Better"
      ) +
      theme_minimal()
  })
  
  output$ppasPlot <- renderPlot({
    dat <- ppas_budget()
    palette <- get_palette(nrow(dat), input$palette)
    ggplot(dat, aes(x = fct_reorder(code, budget), y = budget, fill = code)) +
      geom_col(show.legend = TRUE) +
      coord_flip() +
      geom_text(aes(label = sprintf("%.2f", budget)), hjust = -0.1, color = "black", size = 4) +
      scale_fill_manual(values = palette) +
      labs(
        title = "Top 7 PPAs by Seeds-related Project Budget",
        x = "PPA",
        y = "Budget in Millions USD",
        fill = "PPA"
      ) +
      theme_minimal()
  })
  
  output$sdgsPlot <- renderPlot({
    dat <- sdgs_budget()
    palette <- get_palette(nrow(dat), input$palette)
    ggplot(dat, aes(x = fct_reorder(code, budget), y = budget, fill = code)) +
      geom_col(show.legend = TRUE) +
      coord_flip() +
      geom_text(aes(label = sprintf("%.2f", budget)), hjust = -0.1, color = "black", size = 4) +
      scale_fill_manual(values = palette) +
      labs(
        title = "Top 7 SDGs by Seeds-related Project Budget",
        x = "SDG",
        y = "Budget in Millions USD",
        fill = "SDG"
      ) +
      theme_minimal()
  })
  
  fao_data <- reactive({
    dt <- read_dta(file.path(portfolio, "portfolio_final.dta")) %>%
      mutate(
        seeds_project = ifelse(`_merge` == 3 | keyword == 1, 1, 0),
        emerg = ifelse(Emergency == "Yes", 1, 0),
        dev = ifelse(Emergency == "No", 1, 0),
        n = 1,
        DWHBudget = DWHBudget / 1e6,
        emer_budget = ifelse(emerg == 1, DWHBudget, 0),
        dev_budget = ifelse(dev == 1, DWHBudget, 0),
        year_EOD = as.integer(substr(as.character(EOD), 7, 10)),
        year_NTE = as.integer(substr(as.character(NTE), 7, 10))
      )
    # Apply EOD/NTE filters
    if (!is.null(input$years_eod) && length(input$years_eod) > 0) {
      dt <- dt %>% filter(year_EOD %in% input$years_eod)
    }
    if (!is.null(input$years_nte) && length(input$years_nte) > 0) {
      dt <- dt %>% filter(year_NTE %in% input$years_nte)
    }
    # Collapse for pie (total and seeds)
    pie_data <- dt %>%
      filter(`_merge` != 2) %>%
      group_by(seeds_project) %>%
      summarise(
        n = sum(n),
        DWHBudget = sum(DWHBudget, na.rm = TRUE),
        .groups = "drop"
      )
    pie_data$label <- c("Others", "Seeds-related Projects")[pie_data$seeds_project + 1]
    # Collapse by year_EOD and seeds_project for trends
    trend_data <- dt %>%
      filter(`_merge` != 2) %>%
      group_by(seeds_project, year_EOD) %>%
      summarise(
        n = sum(n),
        emerg = sum(emerg),
        dev = sum(dev),
        DWHBudget = sum(DWHBudget, na.rm = TRUE),
        emer_budget = sum(emer_budget, na.rm = TRUE),
        dev_budget = sum(dev_budget, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(year = year_EOD)
    # Collapse total FAO (all projects) by EOD year
    total_by_year <- dt %>%
      group_by(year_EOD) %>%
      summarise(
        n = sum(n),
        emerg = sum(emerg),
        dev = sum(dev),
        DWHBudget = sum(DWHBudget, na.rm = TRUE),
        emer_budget = sum(emer_budget, na.rm = TRUE),
        dev_budget = sum(dev_budget, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(seeds_project = 2) %>%
      rename(year = year_EOD)
    # Append total_by_year and reshape
    combined <- bind_rows(trend_data, total_by_year) %>%
      pivot_wider(
        id_cols = year,
        names_from = seeds_project,
        values_from = c(DWHBudget, emer_budget, dev_budget, n, emerg, dev),
        names_glue = "{.value}{seeds_project}"
      ) %>%
      arrange(year)
    list(
      pie = pie_data,
      trends = combined
    )
  })
  
  output$seedsFAOPie <- renderPlot({
    pie_data <- fao_data()$pie
    total_budget <- sum(pie_data$DWHBudget)
    pie_data <- pie_data %>%
      mutate(pct = 100 * DWHBudget / total_budget,
             label2 = sprintf("%s: %.2f%% (%.2f bn $)", label, pct, DWHBudget / 1000))
    palette <- get_palette(nrow(pie_data), input$palette)
    ggplot(pie_data, aes(x = "", y = DWHBudget, fill = label)) +
      geom_col(width = 1) +
      coord_polar("y") +
      geom_text(aes(label = sprintf("%.2f%%", pct)),
                position = position_stack(vjust = 0.5), color = "black", size = 6) +
      scale_fill_manual(values = palette) +
      labs(title = "Seeds-related Projects Budget in FAO Total Portfolio",
           fill = "", x = "", y = "") +
      theme_void()
  })
  
  output$trend_budget <- renderPlot({
    trends <- fao_data()$trends
    dat <- trends %>%
      select(year, DWHBudget1, DWHBudget2) %>%
      rename(Seeds = DWHBudget1, FAO = DWHBudget2) %>%
      pivot_longer(cols = c(Seeds, FAO), names_to = "Portfolio", values_to = "Budget")
    palette <- get_palette(2, input$palette)
    ggplot(dat, aes(x = year, y = Budget, fill = Portfolio)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = sprintf("%.2f", Budget)), position = position_dodge(width = 0.9),
                vjust = -0.3, color = "black", size = 3) +
      scale_fill_manual(values = palette) +
      labs(title = "Trends through years: Seeds-related and FAO Project Budget",
           y = "Millions of $", x = "Year") +
      theme_minimal()
  })
  
  output$trend_emergency_budget <- renderPlot({
    trends <- fao_data()$trends
    dat <- trends %>%
      select(year, emer_budget1, emer_budget2) %>%
      rename(Seeds = emer_budget1, FAO = emer_budget2) %>%
      pivot_longer(cols = c(Seeds, FAO), names_to = "Portfolio", values_to = "Emergency_Budget")
    palette <- get_palette(2, input$palette)
    ggplot(dat, aes(x = year, y = Emergency_Budget, fill = Portfolio)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = sprintf("%.2f", Emergency_Budget)), position = position_dodge(width = 0.9),
                vjust = -0.3, color = "black", size = 3) +
      scale_fill_manual(values = palette) +
      labs(title = "Trends through years: Emergency Budget",
           y = "Millions of $", x = "Year") +
      theme_minimal()
  })
  
  output$trend_development_budget <- renderPlot({
    trends <- fao_data()$trends
    dat <- trends %>%
      select(year, dev_budget1, dev_budget2) %>%
      rename(Seeds = dev_budget1, FAO = dev_budget2) %>%
      pivot_longer(cols = c(Seeds, FAO), names_to = "Portfolio", values_to = "Development_Budget")
    palette <- get_palette(2, input$palette)
    ggplot(dat, aes(x = year, y = Development_Budget, fill = Portfolio)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = sprintf("%.2f", Development_Budget)), position = position_dodge(width = 0.9),
                vjust = -0.3, color = "black", size = 3) +
      scale_fill_manual(values = palette) +
      labs(title = "Trends through years: Development Budget",
           y = "Millions of $", x = "Year") +
      theme_minimal()
  })
  
  output$trend_project_count <- renderPlot({
    trends <- fao_data()$trends
    dat <- trends %>%
      select(year, n1, n2) %>%
      rename(Seeds = n1, FAO = n2) %>%
      pivot_longer(cols = c(Seeds, FAO), names_to = "Portfolio", values_to = "No_of_Projects")
    palette <- get_palette(2, input$palette)
    ggplot(dat, aes(x = year, y = No_of_Projects, fill = Portfolio)) +
      geom_col(position = "dodge") +
      geom_text(aes(label = sprintf("%.0f", No_of_Projects)), position = position_dodge(width = 0.9),
                vjust = -0.3, color = "black", size = 3) +
      scale_fill_manual(values = palette) +
      labs(title = "Trends through years: Number of Projects",
           y = "Number of projects", x = "Year") +
      theme_minimal()
  })
  
  # ---- Prepare country DWH budget data for map ----
  country_budget_for_map <- reactive({
    filtered_data() %>%
      filter(!grepl(",", Recipients)) %>% # remove entries with multiple countries
      group_by(RecipientCountry = Recipients) %>%
      summarise(
        DWHBudget = sum(DWHBudget, na.rm = TRUE), 
        nProjects = sum(n, na.rm = TRUE),
        .groups = "drop"
      )
  })
  selected_country <- reactiveVal(NULL)
  
  observeEvent(input$worldMapPlot_shape_click, {
    click <- input$worldMapPlot_shape_click
    if (!is.null(click$id)) selected_country(click$id)
  })
  
  country_top_projects <- reactive({
    req(selected_country())
    filtered_data() %>%
      filter(
        !grepl(",", Recipients),
        Recipients == selected_country()
      ) %>%
      arrange(desc(DWHBudget)) %>%
      slice_head(n = 10) %>%
      select(Symbol, Title, DWHBudget)
  })
  
  output$countryProjectsTable <- renderDT({
    req(selected_country())
    dat <- country_top_projects()
    if (nrow(dat) == 0) return(NULL)
    datatable(
      dat,
      colnames = c("Project Code", "Project Title", "DWH Budget (Million USD)"),
      options = list(pageLength = 10, dom = 'tip', scrollX = TRUE)
    )
  })
  
  output$worldMapPlot <- renderLeaflet({
    # Get world map
    world <- ne_countries(scale = "medium", returnclass = "sf")
    cb <- country_budget_for_map()
    
    # Harmonize country names (add more rules as needed)
    cb$RecipientCountry[cb$RecipientCountry == "Central African Rep."] <- "Central African Republic"
    cb$RecipientCountry[cb$RecipientCountry == "DRC"] <- "Democratic Republic of the Congo"
    # Add more mappings as needed
    
    # Join budget data to world map
    world <- left_join(world, cb, by = c("name" = "RecipientCountry"))
    
    palette_bins <- c(0, 0.1, 0.5, 1, 2, 3, 5, 7, 10, 15, 20, 30, 50, 100, Inf)
    pal <- colorBin(
      palette = "YlOrRd",
      domain = world$DWHBudget,
      bins = palette_bins,
      na.color = "#eaeaea",
      pretty = FALSE
    )
    
    # Labels for tooltips
    world$label <- ifelse(
      is.na(world$DWHBudget),
      paste0("<strong>", world$name, "</strong><br>No seeds-related projects"),
      paste0(
        "<strong>", world$name, "</strong><br>",
        "Seeds-related Projects: <strong>", world$nProjects, "</strong><br>",
        "Budget: <strong>", round(world$DWHBudget, 2), " Million USD</strong>"
      )
    )
    
    leaflet(world, options = leafletOptions(minZoom = 2, maxZoom = 6, maxBounds = list(
      list(-60, -180),
      list(85, 180)
    ))) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addPolygons(
        fillColor = ~pal(DWHBudget),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        layerId = ~name,  # <- important for click
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = lapply(world$label, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal, values = ~DWHBudget, opacity = 0.7, title = "DWH Budget<br>(Million USD)",
        position = "bottomright"
      )
  })
  output$total_projects <- renderValueBox({
    valueBox(
      value = formatC(nrow(filtered_data()), format="d", big.mark=","), 
      subtitle = "Total Projects", 
      icon = icon("folder-open"),
    )
  })
  
  output$total_budget <- renderValueBox({
    valueBox(
      value = paste0("$", formatC(sum(filtered_data()$DWHBudget, na.rm=TRUE), format="f", digits=2, big.mark=","), "M"),
      subtitle = "Total Budget",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$total_countries <- renderValueBox({
    valueBox(
      value = length(unique(filtered_data()$Recipients)),
      subtitle = "Countries Covered",
      icon = icon("globe"),
      color = "orange"
    )
  })
  
  output$total_years <- renderValueBox({
    valueBox(
      value = paste0(
        min(filtered_data()$year_EOD, na.rm=TRUE), " - ", 
        max(filtered_data()$year_NTE, na.rm=TRUE)
      ),
      subtitle = "Year Range",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  output$projectExplorer <- DT::renderDataTable({
    dat <- filtered_data() %>%
      select(Symbol, Title, Recipients, Donors, year_EOD, year_NTE, DWHBudget, Emergency)
    DT::datatable(dat, options=list(pageLength=10, scrollX=TRUE), filter="top", rownames=FALSE)
  })
  
  output$downloadProjects <- downloadHandler(
    filename = function() { paste0("projects-", Sys.Date(), ".csv") },
    content = function(file) {
      dat <- filtered_data() %>%
        select(Symbol, Title, Recipients, Donors, year_EOD, year_NTE, DWHBudget, Emergency)
      write.csv(dat, file, row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)