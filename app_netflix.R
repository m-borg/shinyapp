# ------------------------------------------------------------------
# 1. SETUP - LOAD LIBRARIES AND DATA
# ------------------------------------------------------------------
# install.packages(c("shiny", "shinythemes", "ggplot2", "dplyr", "readr", "DT", "viridis", "forcats", "stringr"))

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)
library(viridis)
library(forcats)
library(stringr)

tryCatch({
  df_raw <- read_csv("netflix_titles.csv", show_col_types = FALSE)
}, error = function(e) {
  stop("Could not load 'netflix_titles.csv'. Please ensure the file is in the correct directory.")
})

# ------------------------------------------------------------------
# 2. DATA PREPARATION (DATA WRANGLING)
# ------------------------------------------------------------------
df_clean <- df_raw %>%
  # Handle multiple countries and genres by taking the first one for simplicity
  mutate(
    country = sapply(strsplit(country, ","), function(x) trimws(x[1])),
    genre = sapply(strsplit(listed_in, ","), function(x) trimws(x[1]))
  ) %>%
  # Create numeric duration and season columns
  mutate(
    duration_minutes = ifelse(type == "Movie", as.numeric(str_extract(duration, "\\d+")), NA),
    seasons = ifelse(type == "TV Show", as.numeric(str_extract(duration, "\\d+")), NA)
  ) %>%
  # Select and rename columns for clarity
  select(
    Type = type, Title = title, Director = director, Country = country,
    ReleaseYear = release_year, Rating = rating,
    DurationMinutes = duration_minutes, Seasons = seasons, Genre = genre
  ) %>%
  # Filter out rows with essential missing data
  filter(!is.na(Rating), !is.na(Country), !is.na(DurationMinutes) | !is.na(Seasons)) %>%
  # Convert data types
  mutate(
    Type = as.factor(Type),
    Rating = as.factor(Rating),
    Country = as.factor(Country),
    Genre = as.factor(Genre)
  )

# --- Define variable lists for the UI ---
# We focus on movie runtimes for numerical analysis as it's more consistent than seasons
movies_df <- df_clean %>% filter(Type == "Movie", !is.na(DurationMinutes))

numeric_vars <- c("ReleaseYear", "DurationMinutes")
categorical_vars <- c("Type", "Rating", "Country", "Genre")

# For T-Tests (vars with 2 levels) & ANOVA (vars with >2 levels)
# We will dynamically let the user choose levels, which is more flexible.
# But we can pre-filter for convenience.
categorical_vars_for_tests <- c("Rating", "Genre") 

# Create a mapping for user-friendly names in plots and labels
pretty_names <- list(
  "ReleaseYear" = "Release Year", "DurationMinutes" = "Duration (Minutes)",
  "Type" = "Type", "Rating" = "Rating", "Country" = "Country", "Genre" = "Primary Genre"
)

# --- Aesthetics ---
primary_color <- "#E50914" # Netflix Red
accent_color <- "#221f1f"  # Netflix Black
grey_color <- "#6c757d"

# ------------------------------------------------------------------
# 3. USER INTERFACE (UI)
# ------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("slate"),
  navbarPage("Netflix Content Analysis",
             # -- Visualization Tab --
             tabPanel("Data Visualization",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Chart Options"),
                                     selectInput("plot_type", "Select Plot Type:", choices = c("Distribution (Histogram)" = "hist", "Categorical Counts (Bar Chart)" = "bar", "Comparison (Box Plot)" = "boxplot")),
                                     hr(),
                                     # --- Dynamic UI based on plot type ---
                                     conditionalPanel("input.plot_type == 'hist'",
                                                      selectInput("var_hist", "Select Variable:", choices = numeric_vars, selected = "DurationMinutes"),
                                                      sliderInput("hist_bins", "Number of Bins:", min = 5, max = 50, value = 30)
                                     ),
                                     conditionalPanel("input.plot_type == 'bar'",
                                                      selectInput("var_bar", "Select Variable:", choices = c("Rating", "Genre", "Country"), selected = "Genre"),
                                                      sliderInput("bar_top_n", "Show Top N Categories:", min = 5, max = 20, value = 10)
                                     ),
                                     conditionalPanel("input.plot_type == 'boxplot'",
                                                      selectInput("var_y_box", "Numeric Variable (Y-axis):", choices = numeric_vars, selected = "DurationMinutes"),
                                                      selectInput("var_x_box", "Grouping Variable (X-axis):", choices = c("Rating", "Genre"), selected = "Rating")
                                     )
                        ),
                        mainPanel(width = 9, plotOutput("mainPlot", height = "600px"))
                      )
             ),
             # -- Statistical Tests Tab --
             tabPanel("Statistical Tests",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Test Options"),
                                     p("Compare the average runtime of movies across different groups."),
                                     selectInput("test_type", "Select Statistical Test:", choices = c("Two-Sample T-Test" = "ttest", "Analysis of Variance (ANOVA)" = "anova")),
                                     hr(),
                                     # --- Dynamic UI for tests ---
                                     selectInput("test_var_num", "Numeric Variable to Test:", choices = "DurationMinutes", selected = "DurationMinutes"),
                                     conditionalPanel("input.test_type == 'ttest'",
                                                      selectInput("test_var_cat_ttest", "Grouping Variable:", choices = categorical_vars_for_tests, selected = "Rating"),
                                                      # Let user pick the two levels to compare
                                                      uiOutput("ttest_level_select_ui")
                                     ),
                                     conditionalPanel("input.test_type == 'anova'",
                                                      selectInput("test_var_cat_anova", "Grouping Variable:", choices = categorical_vars_for_tests, selected = "Rating"),
                                                      uiOutput("anova_level_select_ui")
                                     )
                        ),
                        mainPanel(width = 9,
                                  h3("Statistical Test Results"),
                                  verbatimTextOutput("testOutput"),
                                  h4("Interpretation of the Result"),
                                  uiOutput("testInterpretation")
                        )
                      )
             ),
             # -- Modeling Tab --
             tabPanel("Predictive Modeling",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Model Setup"),
                                     p("Build a model to predict if content is a Movie or TV Show based on selected features."),
                                     checkboxGroupInput("model_predictors", "Select Predictor Variables:",
                                                        choices = c("ReleaseYear", "Rating", "Country", "Genre"),
                                                        selected = c("ReleaseYear", "Rating")),
                                     actionButton("run_model", "Build Logistic Regression Model", class = "btn-primary")
                        ),
                        mainPanel(width = 9,
                                  h3("Logistic Regression Model Summary"),
                                  verbatimTextOutput("modelSummary"),
                                  h4("Model Interpretation"),
                                  uiOutput("modelInterpretation")
                        )
                      )
             ),
             # -- About & Data Tab --
             tabPanel("About & Data",
                      fluidRow(
                        column(12,
                               h3("About This Application"),
                               p("This Shiny app provides an interactive platform for analyzing the Netflix Movies and TV Shows dataset. It is designed to fulfill project requirements by offering three core functionalities:"),
                               tags$ul(
                                 tags$li(strong("Dynamic Visualizations:"), "Explore data distributions and relationships through interactive charts."),
                                 tags$li(strong("Statistical Testing:"), "Perform t-tests and ANOVA to validate hypotheses about the data."),
                                 tags$li(strong("Predictive Modeling:"), "Build a logistic regression model to understand factors that differentiate Movies from TV Shows.")
                               ),
                               hr(),
                               h3("Data Preview"),
                               p("The table below shows the cleaned and processed data used throughout this application."),
                               DT::dataTableOutput("dataTable")
                        )
                      )
             )
  )
)

# ------------------------------------------------------------------
# 4. SERVER (APPLICATION LOGIC)
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # --- Base Theme for Plots ---
  base_theme <- theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 22, margin = margin(b = 10), color = "white"),
      plot.subtitle = element_text(size = 16, color = grey_color, margin = margin(b = 25)),
      axis.title = element_text(face = "bold", size = 14, color = "white"),
      axis.text = element_text(size = 12, color = "white"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", color = "white"),
      legend.text = element_text(color = "white"),
      panel.grid.major = element_line(color = "#404040"),
      panel.grid.minor = element_line(color = "#303030"),
      plot.background = element_rect(fill = "#221f1f", color = NA),
      panel.background = element_rect(fill = "#221f1f", color = NA),
      strip.text = element_text(color = "white", face = "bold", size = 14)
    )
  
  # --- Visualization Tab Logic ---
  output$mainPlot <- renderPlot({
    switch(input$plot_type,
           "hist" = {
             req(input$var_hist)
             mean_val <- round(mean(movies_df[[input$var_hist]], na.rm = TRUE), 1)
             ggplot(movies_df, aes(x = !!sym(input$var_hist))) +
               geom_histogram(bins = input$hist_bins, fill = primary_color, alpha = 0.9) +
               geom_vline(xintercept = mean_val, color = "white", linetype = "dashed", size = 1.5) +
               annotate("text", x = mean_val * 1.1, y = Inf, label = paste("Mean =", mean_val), vjust = 2, color = "white", size = 5) +
               labs(title = paste("Distribution of Movie", pretty_names[[input$var_hist]]),
                    subtitle = "The dotted line indicates the average value.",
                    x = pretty_names[[input$var_hist]], y = "Count") + base_theme
           },
           "bar" = {
             req(input$var_bar)
             data_to_plot <- df_clean %>%
               filter(!is.na(!!sym(input$var_bar))) %>%
               count(!!sym(input$var_bar), sort = TRUE) %>%
               top_n(input$bar_top_n, n)
             
             ggplot(data_to_plot, aes(x = fct_reorder(!!sym(input$var_bar), n), y = n)) +
               geom_col(aes(fill = n)) +
               coord_flip() +
               scale_fill_viridis(direction = -1) +
               labs(title = paste("Top", input$bar_top_n, "by", pretty_names[[input$var_bar]]),
                    subtitle = "Showing the most frequent categories in the dataset.",
                    x = pretty_names[[input$var_bar]], y = "Number of Titles") +
               geom_text(aes(label = n), hjust = -0.2, color = "white", size = 4) +
               base_theme + theme(legend.position = "none")
           },
           "boxplot" = {
             req(input$var_y_box, input$var_x_box)
             # Let's show the top 8 categories to avoid clutter
             top_cats <- movies_df %>% count(!!sym(input$var_x_box), sort=TRUE) %>% top_n(8) %>% pull(!!sym(input$var_x_box))
             data_to_plot <- movies_df %>% filter(!!sym(input$var_x_box) %in% top_cats)
             
             ggplot(data_to_plot, aes(x = fct_reorder(!!sym(input$var_x_box), !!sym(input$var_y_box)), y = !!sym(input$var_y_box))) +
               geom_boxplot(aes(fill = !!sym(input$var_x_box)), outlier.colour = primary_color, show.legend = FALSE) +
               scale_fill_viridis_d(alpha = 0.8) +
               labs(title = paste("Comparing", pretty_names[[input$var_y_box]], "by", pretty_names[[input$var_x_box]]),
                    subtitle = "Showing distributions for the top 8 categories.",
                    x = pretty_names[[input$var_x_box]], y = pretty_names[[input$var_y_box]]) +
               coord_flip() + base_theme
           }
    )
  }, bg="transparent")
  
  
  # --- Statistical Tests Tab Logic ---
  # Dynamic UI for selecting levels for t-test
  output$ttest_level_select_ui <- renderUI({
    req(input$test_var_cat_ttest)
    choices <- levels(factor(movies_df[[input$test_var_cat_ttest]]))
    tagList(
      selectInput("level1_ttest", "Select First Group:", choices = choices, selected = choices[1]),
      selectInput("level2_ttest", "Select Second Group:", choices = choices, selected = choices[2])
    )
  })
  
  # Dynamic UI for selecting levels for ANOVA
  output$anova_level_select_ui <- renderUI({
    req(input$test_var_cat_anova)
    choices <- levels(factor(movies_df[[input$test_var_cat_anova]]))
    selectInput("levels_anova", "Select Groups (3 or more):", choices = choices, multiple = TRUE,
                selected = choices[1:min(4, length(choices))])
  })
  
  # Reactive expression to run the chosen test
  test_results <- reactive({
    if (input$test_type == 'ttest') {
      req(input$level1_ttest, input$level2_ttest, input$level1_ttest != input$level2_ttest)
      test_data <- movies_df %>%
        filter(!!sym(input$test_var_cat_ttest) %in% c(input$level1_ttest, input$level2_ttest))
      t.test(as.formula(paste(input$test_var_num, "~", input$test_var_cat_ttest)), data = test_data)
    } else if (input$test_type == 'anova') {
      req(input$levels_anova, length(input$levels_anova) >= 3)
      test_data <- movies_df %>%
        filter(!!sym(input$test_var_cat_anova) %in% input$levels_anova)
      summary(aov(as.formula(paste(input$test_var_num, "~", input$test_var_cat_anova)), data = test_data))
    }
  })
  
  output$testOutput <- renderPrint({ test_results() })
  
  output$testInterpretation <- renderUI({
    res <- test_results()
    p_value <- if (input$test_type == 'ttest') res$p.value else res[[1]][["Pr(>F)"]][1]
    
    if (is.na(p_value)) return(p("Calculation error or insufficient data."))
    
    if (p_value < 0.05) {
      tags$p(style = "color:#5cb85c; font-weight:bold;",
             paste0("Result is significant (p-value = ", format(p_value, scientific = FALSE, digits = 4), ")."),
             "We reject the null hypothesis. There is a statistically significant difference in the means between the chosen groups.")
    } else {
      tags$p(style = "color:#d9534f;",
             paste0("Result is not significant (p-value = ", format(p_value, scientific = FALSE, digits = 4), ")."),
             "We fail to reject the null hypothesis. There is no statistically significant difference in the means between the chosen groups.")
    }
  })
  
  # --- Modeling Tab Logic ---
  model_fit <- eventReactive(input$run_model, {
    req(input$model_predictors, length(input$model_predictors) > 0)
    formula_str <- paste("Type ~", paste(input$model_predictors, collapse = " + "))
    glm(as.formula(formula_str), data = df_clean, family = "binomial")
  })
  
  output$modelSummary <- renderPrint({ summary(model_fit()) })
  
  output$modelInterpretation <- renderUI({
    req(model_fit())
    model_summary <- summary(model_fit())
    coeffs <- model_summary$coefficients
    significant_preds <- rownames(coeffs)[coeffs[, "Pr(>|z|)"] < 0.05 & rownames(coeffs) != "(Intercept)"]
    
    tagList(
      p("The model predicts the log-odds of content being a 'TV Show' vs. a 'Movie' (the baseline)."),
      p(strong("Interpreting Coefficients (Estimates):")),
      tags$ul(
        tags$li(strong("Positive:"), "Increases the odds of being a TV Show."),
        tags$li(strong("Negative:"), "Decreases the odds of being a TV Show (i.e., increases odds of being a Movie).")
      ),
      hr(),
      if (length(significant_preds) > 0) {
        p(strong("Statistically significant predictors (p < 0.05) in this model:"), tags$br(),
          paste(sapply(significant_preds, function(x) str_split(x, "(?<=[a-z])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])")[[1]][1]), collapse = ", "))
      } else {
        p(strong("No statistically significant predictors were found in the current model configuration."))
      }
    )
  })
  
  # --- About & Data Tab Logic ---
  output$dataTable <- DT::renderDataTable({
    DT::datatable(df_clean,
                  options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE,
                  filter = 'top')
  })
}

# ------------------------------------------------------------------
# 5. RUN THE APPLICATION
# ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
