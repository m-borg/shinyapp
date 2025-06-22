# ------------------------------------------------------------------
# 1. WSTĘP - ŁADOWANIE BIBLIOTEK I DANYCH
# ------------------------------------------------------------------
# install.packages(c("shiny", "shinythemes", "ggplot2", "dplyr", "readr", "DT", "RColorBrewer", "viridis", "forcats"))

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)
library(RColorBrewer)
library(viridis)
library(forcats)

# Wczytanie danych z lokalnego pliku CSV
tryCatch({
  df <- read_csv("student_depression_dataset.csv", show_col_types = FALSE)
}, error = function(e) {
  stop("Nie można wczytać pliku 'student_depression_dataset.csv'. Upewnij się, że plik znajduje się w odpowiednim folderze.")
})

# ------------------------------------------------------------------
# 2. PRZYGOTOWANIE DANYCH (DATA WRANGLING)
# ------------------------------------------------------------------
# POPRAWKA: Prawidłowe filtrowanie - zostawiamy tylko wiersze, 
# gdzie w kolumnie 'Profession' jest jawnie wpisane "Student".
df_student <- df %>%
  filter(Profession == "Student")

# Dalsze operacje wykonujemy na odfiltrowanej ramce danych 'df_student'
df_clean <- df_student %>%
  select(
    id, Gender, Age, `Academic Pressure`, CGPA, `Study Satisfaction`,
    `Sleep Duration`, `Dietary Habits`, `Have you ever had suicidal thoughts ?`,
    `Work/Study Hours`, `Financial Stress`, `Family History of Mental Illness`, Depression
  ) %>%
  rename(
    ID = id, Plec = Gender, Wiek = Age, Presja_Akademicka = `Academic Pressure`,
    Srednia_Ocen = CGPA, Satysfakcja_Ze_Studiow = `Study Satisfaction`,
    Dlugosc_Snu = `Sleep Duration`, Nawyki_Zywieniowe = `Dietary Habits`,
    Mysli_Samobojcze = `Have you ever had suicidal thoughts ?`,
    Godziny_Pracy_Nauki = `Work/Study Hours`, Stres_Finansowy = `Financial Stress`,
    Historia_Chorob_w_Rodzinie = `Family History of Mental Illness`,
    Depresja = Depression
  )

df_clean <- df_clean %>%
  mutate(
    Wiek = as.numeric(Wiek), Presja_Akademicka = as.numeric(Presja_Akademicka),
    Srednia_Ocen = as.numeric(Srednia_Ocen), Godziny_Pracy_Nauki = as.numeric(Godziny_Pracy_Nauki),
    Stres_Finansowy = as.numeric(Stres_Finansowy),
    Plec = as.factor(Plec),
    
    Dlugosc_Snu = case_when(
      Dlugosc_Snu == 'Less than 5 hours' ~ "Poniżej 5h",
      Dlugosc_Snu == '5-6 hours' ~ "5-6 godzin",
      Dlugosc_Snu == '7-8 hours' ~ "7-8 godzin",
      Dlugosc_Snu == 'More than 8 hours' ~ "Powyżej 8h",
      TRUE ~ NA_character_
    ),
    Dlugosc_Snu = factor(Dlugosc_Snu, 
                         levels = c("Poniżej 5h", "5-6 godzin", "7-8 godzin", "Powyżej 8h"),
                         ordered = TRUE),
    
    Nawyki_Zywieniowe = as.factor(Nawyki_Zywieniowe),
    Mysli_Samobojcze = as.factor(Mysli_Samobojcze),
    Historia_Chorob_w_Rodzinie = as.factor(Historia_Chorob_w_Rodzinie),
    Depresja = factor(Depresja, levels = c(0, 1), labels = c("Nie", "Tak"))
  )

# --- Listy i definicje dla UI i estetyki ---
zmienne_numeryczne <- c("Wiek", "Presja_Akademicka", "Srednia_Ocen", "Godziny_Pracy_Nauki", "Stres_Finansowy")
zmienne_kategoryczne <- c("Plec", "Dlugosc_Snu", "Nawyki_Zywieniowe", "Mysli_Samobojcze", "Historia_Chorob_w_Rodzinie", "Depresja")

zmienne_grupujace_boxplot <- c("Wiek", zmienne_kategoryczne)

zmienne_kategoryczne_2_poziomy <- zmienne_kategoryczne[sapply(df_clean[,zmienne_kategoryczne], function(x) nlevels(x) == 2)]
zmienne_kategoryczne_wiele_poziomow <- zmienne_kategoryczne[sapply(df_clean[,zmienne_kategoryczne], function(x) nlevels(x) > 2)]

pretty_names <- list(
  "Wiek" = "Wiek", "Presja_Akademicka" = "Presja Akademicka", "Srednia_Ocen" = "Średnia Ocen (CGPA)",
  "Godziny_Pracy_Nauki" = "Godziny Pracy/Nauki", "Stres_Finansowy" = "Stres Finansowy",
  "Plec" = "Płeć", "Dlugosc_Snu" = "Długość Snu", "Nawyki_Zywieniowe" = "Nawyki Żywieniowe",
  "Mysli_Samobojcze" = "Myśli Samobójcze", "Historia_Chorob_w_Rodzinie" = "Choroby w Rodzinie",
  "Depresja" = "Depresja"
)

primary_color <- "#0d6efd"; accent_color <- "#dc3545"; grey_color <- "#6c757d"

# ------------------------------------------------------------------
# 3. INTERFEJS UŻYTKOWNIKA (UI)
# ------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  navbarPage("Analiza Danych o Zdrowiu Psychicznym Studentów",
             tabPanel("Wizualizacja Danych",
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     h4("Opcje Wykresu"),
                                     selectInput("plot_type", "Wybierz typ wykresu:", choices = c("Rozkład zmiennej (Histogram)" = "hist", "Porównanie grup (Wykres pudełkowy)" = "boxplot", "Relacja dwóch zmiennych (Wykres punktowy)" = "scatter")),
                                     hr(style = "border-top: 1px solid #ccc;"),
                                     conditionalPanel("input.plot_type == 'hist'", selectInput("var_hist", "Wybierz zmienną:", choices = zmienne_numeryczne), sliderInput("hist_bins", "Liczba słupków:", min = 5, max = 50, value = 25, step = 1)),
                                     conditionalPanel("input.plot_type == 'boxplot'", selectInput("var_y_box", "Zmienna numeryczna (oś Y):", choices = zmienne_numeryczne), selectInput("var_x_box", "Zmienna grupująca (oś X):", choices = zmienne_grupujace_boxplot)),
                                     conditionalPanel("input.plot_type == 'scatter'", selectInput("var_x_scatter", "Zmienna dla osi X:", choices = zmienne_numeryczne, selected = "Wiek"), selectInput("var_y_scatter", "Zmienna dla osi Y:", choices = zmienne_numeryczne, selected = "Srednia_Ocen"), selectInput("scatter_color", "Pokoloruj punkty według:", choices = c("Brak", zmienne_kategoryczne), selected = "Depresja"),
                                                      p(em("Uwaga: Jeśli 'Wiek' jest jedną z osi, punkty są lekko rozproszone (jitter), aby uniknąć nakładania się.")))
                        ),
                        mainPanel(width = 9, plotOutput("mainPlot", height = "550px"))
                      )
             ),
             tabPanel("Testy Statystyczne",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Opcje Testu"),
                          selectInput("test_type", "Wybierz test statystyczny:", choices = c("Test t-Studenta dla prób niezależnych" = "ttest", "Analiza wariancji (ANOVA)" = "anova")),
                          hr(),
                          conditionalPanel("input.test_type == 'ttest'", p("Test t-Studenta porównuje średnie w dwóch grupach."), selectInput("var_ttest_num", "Wybierz zmienną numeryczną:", choices = zmienne_numeryczne), selectInput("var_ttest_cat", "Wybierz zmienną grupującą (2 poziomy):", choices = zmienne_kategoryczne_2_poziomy)),
                          conditionalPanel("input.test_type == 'anova'", p("ANOVA porównuje średnie w trzech lub więcej grupach."), selectInput("var_anova_num", "Wybierz zmienną numeryczną:", choices = zmienne_numeryczne), selectInput("var_anova_cat", "Wybierz zmienną grupującą (>2 poziomy):", choices = zmienne_kategoryczne_wiele_poziomow))
                        ),
                        mainPanel(
                          h3("Wyniki Testu Statystycznego"),
                          verbatimTextOutput("testOutput"),
                          h4("Interpretacja Wyniku"),
                          uiOutput("testInterpretation")
                        )
                      )
             ),
             tabPanel("Modelowanie (Regresja Logistyczna)",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Budowa Modelu Predykcyjnego"),
                          p("Celem jest zbudowanie modelu, który przewiduje prawdopodobieństwo wystąpienia depresji ('Tak'/'Nie') na podstawie wybranych zmiennych."),
                          checkboxGroupInput("model_predictors", "Wybierz zmienne objaśniające (predyktory):", choices = c(zmienne_numeryczne, zmienne_kategoryczne[zmienne_kategoryczne != "Depresja"]), selected = c("Stres_Finansowy", "Mysli_Samobojcze")),
                          actionButton("run_model", "Uruchom Model", class = "btn-primary")
                        ),
                        mainPanel(
                          h3("Podsumowanie Modelu Regresji Logistycznej"),
                          verbatimTextOutput("modelSummary"),
                          h4("Interpretacja Modelu"),
                          uiOutput("modelInterpretation")
                        )
                      )
             ),
             tabPanel("O Aplikacji i Danych",
                      fluidRow(
                        column(12,
                               h3("Projekt Zaliczeniowy - Aplikacja Shiny"),
                               p("Ta aplikacja została stworzona jako projekt zaliczeniowy. Umożliwia interaktywną analizę danych dotyczących zdrowia psychicznego studentów."),
                               p("Aplikacja pozwala na:"),
                               tags$ul(tags$li("Dynamiczne tworzenie wizualizacji danych."), tags$li("Przeprowadzanie podstawowych testów statystycznych."), tags$li("Budowanie modelu regresji logistycznej do predykcji depresji.")),
                               hr(),
                               h3("Podgląd Danych"),
                               p("Poniżej znajduje się tabela z przetworzonymi danymi użytymi w aplikacji. Zbiór został oczyszczony i zawiera wyłącznie dane studentów."),
                               DT::dataTableOutput("dataTable")
                        )
                      )
             )
  )
)

# ------------------------------------------------------------------
# 4. SERWER (LOGIKA APLIKACJI)
# ------------------------------------------------------------------
server <- function(input, output, session) {
  
  base_theme <- theme_minimal(base_size = 14) + 
    theme(
      plot.title = element_text(face = "bold", size = 20, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 14, color = grey_color, margin = margin(b = 25)),
      axis.title = element_text(face = "bold", size = 12),
      axis.text = element_text(size = 11),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    )
  
  output$mainPlot <- renderPlot({
    switch(input$plot_type,
           "hist" = {
             req(input$var_hist, input$hist_bins)
             mean_val <- round(mean(df_clean[[input$var_hist]], na.rm = TRUE), 2)
             sd_val <- round(sd(df_clean[[input$var_hist]], na.rm = TRUE), 2)
             ggplot(df_clean, aes(x = !!sym(input$var_hist))) +
               geom_histogram(aes(y = ..density..), bins = input$hist_bins, fill = primary_color, alpha = 0.8) +
               geom_density(color = accent_color, size = 1.2) +
               labs(title = paste("Rozkład zmiennej:", pretty_names[[input$var_hist]]), subtitle = paste("Średnia:", mean_val, "| Odchylenie standardowe:", sd_val), x = pretty_names[[input$var_hist]], y = "Gęstość") + base_theme
           },
           "boxplot" = {
             req(input$var_y_box, input$var_x_box)
             ggplot(df_clean, aes(x = factor(!!sym(input$var_x_box)), y = !!sym(input$var_y_box))) +
               geom_boxplot(aes(fill = factor(!!sym(input$var_x_box))), outlier.color = accent_color, outlier.shape = 18, outlier.size = 3) +
               scale_fill_viridis_d(alpha = 0.7) +
               labs(title = paste("Porównanie", pretty_names[[input$var_y_box]], "w grupach"), subtitle = "Pudełka pokazują rozkład od 25 do 75 kwantyla. Pogrubiona linia to mediana.", x = pretty_names[[input$var_x_box]], y = pretty_names[[input$var_y_box]]) + base_theme + theme(legend.position = "none")
           },
           "scatter" = {
             req(input$var_x_scatter, input$var_y_scatter)
             p_aes <- aes(x = !!sym(input$var_x_scatter), y = !!sym(input$var_y_scatter))
             plot <- ggplot(df_clean, p_aes)
             geom_to_use <- if (input$var_x_scatter == "Wiek" || input$var_y_scatter == "Wiek") {
               geom_jitter(alpha = 0.5, size = 2.5, width = 0.2, height = 0)
             } else {
               geom_point(alpha = 0.6, size = 2.5)
             }
             if (input$scatter_color != "Brak") {
               p_aes$colour <- as.name(input$scatter_color)
               plot <- ggplot(df_clean, p_aes) + geom_to_use + scale_color_viridis_d(name = pretty_names[[input$scatter_color]])
             } else {
               plot <- ggplot(df_clean, p_aes) + geom_to_use
             }
             plot +
               geom_smooth(method = "lm", se = FALSE, color = accent_color, formula = y ~ x) +
               labs(title = paste("Relacja między", pretty_names[[input$var_x_scatter]], "a", pretty_names[[input$var_y_scatter]]), subtitle = "Czerwona linia pokazuje ogólny trend liniowy w danych", x = pretty_names[[input$var_x_scatter]], y = pretty_names[[input$var_y_scatter]]) + base_theme
           }
    )
  })
  
  test_results <- reactive({
    if (input$test_type == 'ttest') {
      req(input$var_ttest_num, input$var_ttest_cat)
      t.test(as.formula(paste(input$var_ttest_num, "~", input$var_ttest_cat)), data = df_clean)
    } else if (input$test_type == 'anova') {
      req(input$var_anova_num, input$var_anova_cat)
      summary(aov(as.formula(paste(input$var_anova_num, "~", input$var_anova_cat)), data = df_clean))
    }
  })
  
  output$testOutput <- renderPrint({ test_results() })
  
  output$testInterpretation <- renderUI({
    res <- test_results()
    p_value <- if (input$test_type == 'ttest') res$p.value else res[[1]][["Pr(>F)"]][1]
    if (is.na(p_value)) return(p("Błąd w obliczeniach lub brak danych."))
    if (p_value < 0.05) {
      tags$p(style = "color:green; font-weight:bold;", paste0("Wartość p = ", round(p_value, 4), ". P < 0.05, odrzucamy H0. Istnieje statystycznie istotna różnica między grupami."))
    } else {
      tags$p(style = "color:red;", paste0("Wartość p = ", round(p_value, 4), ". P >= 0.05, brak podstaw do odrzucenia H0. Różnice między grupami nie są statystycznie istotne."))
    }
  })
  
  model_fit <- eventReactive(input$run_model, {
    req(input$model_predictors, length(input$model_predictors) > 0)
    formula_str <- paste("Depresja ~", paste(input$model_predictors, collapse = " + "))
    glm(as.formula(formula_str), data = df_clean, family = "binomial")
  })
  
  output$modelSummary <- renderPrint({ summary(model_fit()) })
  
  output$modelInterpretation <- renderUI({
    req(model_fit())
    model_summary <- summary(model_fit())
    coeffs <- model_summary$coefficients
    significant_preds <- rownames(coeffs)[coeffs[, "Pr(>|z|)"] < 0.05 & rownames(coeffs) != "(Intercept)"]
    tagList(
      p("Model regresji logistycznej ocenia, jak zmienne wpływają na szansę wystąpienia depresji."),
      p("Współczynniki (Estimate):"),
      tags$ul(tags$li(strong("Dodatni:"), "zwiększa szansę."), tags$li(strong("Ujemny:"), "zmniejsza szansę.")),
      hr(),
      if (length(significant_preds) > 0) {
        p(strong("Statystycznie istotne predyktory (p < 0.05):"), tags$br(), paste(pretty_names[significant_preds], collapse = ", "))
      } else {
        p(strong("Brak statystycznie istotnych predyktorów w modelu."))
      }
    )
  })
  
  output$dataTable <- DT::renderDataTable({
    DT::datatable(df_clean, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
}

# ------------------------------------------------------------------
# 5. URUCHOMIENIE APLIKACJI
# ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
