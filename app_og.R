# install.packages(c("shiny", "shinythemes", "ggplot2", "dplyr", "readr", "DT"))

# ------------------------------------------------------------------
# 1. WSTĘP - ŁADOWANIE BIBLIOTEK I DANYCH
# ------------------------------------------------------------------

# Wczytanie potrzebnych pakietów
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)

# Wczytanie danych z lokalnego pliku CSV
tryCatch({
  df <- read_csv("student_depression_dataset.csv", show_col_types = FALSE)
}, error = function(e) {
  stop("Nie można wczytać pliku 'student_depression_dataset.csv'. Upewnij się, że plik znajduje się w odpowiednim folderze i jest poprawnym plikiem CSV (rozdzielanym przecinkami).")
})


# ------------------------------------------------------------------
# 2. PRZYGOTOWANIE DANYCH (DATA WRANGLING)
# ------------------------------------------------------------------

df_clean <- df %>%
  select(
    id, Gender, Age, City, Profession, `Academic Pressure`, `Work Pressure`,
    CGPA, `Study Satisfaction`, `Job Satisfaction`, `Sleep Duration`, `Dietary Habits`,
    Degree, `Have you ever had suicidal thoughts ?`, `Work/Study Hours`, `Financial Stress`,
    `Family History of Mental Illness`, Depression
  ) %>%
  rename(
    ID = id,
    Plec = Gender,
    Wiek = Age,
    Miasto = City,
    Profesja = Profession,
    Presja_Akademicka = `Academic Pressure`,
    Presja_Zawodowa = `Work Pressure`,
    Srednia_Ocen = CGPA,
    Satysfakcja_Ze_Studiow = `Study Satisfaction`,
    Satysfakcja_Z_Pracy = `Job Satisfaction`,
    Dlugosc_Snu = `Sleep Duration`,
    Nawyki_Zywieniowe = `Dietary Habits`,
    Wyksztalcenie = Degree,
    Mysli_Samobojcze = `Have you ever had suicidal thoughts ?`,
    Godziny_Pracy_Nauki = `Work/Study Hours`,
    Stres_Finansowy = `Financial Stress`,
    Historia_Chorob_w_Rodzinie = `Family History of Mental Illness`,
    Depresja = Depression
  )

# Konwersja typów danych
df_clean <- df_clean %>%
  mutate(
    # Zmienne numeryczne
    Wiek = as.numeric(Wiek),
    Presja_Akademicka = as.numeric(Presja_Akademicka),
    Srednia_Ocen = as.numeric(Srednia_Ocen),
    Godziny_Pracy_Nauki = as.numeric(Godziny_Pracy_Nauki),
    Stres_Finansowy = as.numeric(Stres_Finansowy),
    
    # Zmienne kategoryczne (faktory)
    Plec = as.factor(Plec),
    Dlugosc_Snu = factor(Dlugosc_Snu, levels = c('Less than 5 hours', '5-6 hours', '7-8 hours', 'More than 8 hours')),
    Nawyki_Zywieniowe = as.factor(Nawyki_Zywieniowe),
    Mysli_Samobojcze = as.factor(Mysli_Samobojcze),
    Historia_Chorob_w_Rodzinie = as.factor(Historia_Chorob_w_Rodzinie),
    Depresja = factor(Depresja, levels = c(0, 1), labels = c("Nie", "Tak"))
  )
# --- USUNIĘTO LINIĘ na.omit() ---


# Listy zmiennych do wykorzystania w interfejsie użytkownika
zmienne_numeryczne <- c("Wiek", "Presja_Akademicka", "Srednia_Ocen", "Godziny_Pracy_Nauki", "Stres_Finansowy")
zmienne_kategoryczne <- c("Plec", "Dlugosc_Snu", "Nawyki_Zywieniowe", "Mysli_Samobojcze", "Historia_Chorob_w_Rodzinie", "Depresja")
zmienne_kategoryczne_dwa_poziomy <- c("Plec", "Mysli_Samobojcze", "Historia_Chorob_w_Rodzinie", "Depresja")
zmienne_kategoryczne_wiecej_poziomow <- c("Dlugosc_Snu", "Nawyki_Zywieniowe")


# ------------------------------------------------------------------
# 3. INTERFEJS UŻYTKOWNIKA (UI)
# ------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("cerulean"), # Wybór estetycznego motywu
  
  # Tytuł aplikacji widoczny na górze
  navbarPage(
    "Analiza Danych o Zdrowiu Psychicznym Studentów",
    
    # --- Zakładka 1: Wizualizacja ---
    tabPanel("Wizualizacja Danych",
             sidebarLayout(
               sidebarPanel(
                 h4("Opcje Wykresu"),
                 selectInput("plot_type", "Wybierz typ wykresu:",
                             choices = c("Boxplot" = "boxplot", 
                                         "Histogram" = "hist", 
                                         "Wykres punktowy" = "scatter")),
                 
                 # Dynamiczne UI w zależności od wybranego wykresu
                 # Dla Boxplot
                 conditionalPanel(
                   condition = "input.plot_type == 'boxplot'",
                   selectInput("var_y_box", "Wybierz zmienną numeryczną (oś Y):", choices = zmienne_numeryczne),
                   selectInput("var_x_box", "Wybierz zmienną kategoryczną (oś X):", choices = zmienne_kategoryczne)
                 ),
                 
                 # Dla Histogramu
                 conditionalPanel(
                   condition = "input.plot_type == 'hist'",
                   selectInput("var_hist", "Wybierz zmienną numeryczną:", choices = zmienne_numeryczne)
                 ),
                 
                 # Dla Wykresu Punktowego
                 conditionalPanel(
                   condition = "input.plot_type == 'scatter'",
                   selectInput("var_x_scatter", "Wybierz zmienną dla osi X:", choices = zmienne_numeryczne, selected = "Wiek"),
                   selectInput("var_y_scatter", "Wybierz zmienną dla osi Y:", choices = zmienne_numeryczne, selected = "Srednia_Ocen")
                 )
               ),
               mainPanel(
                 h3("Wygenerowany Wykres"),
                 plotOutput("mainPlot")
               )
             )
    ),
    
    # --- Zakładka 2: Testy Statystyczne ---
    tabPanel("Testy Statystyczne",
             sidebarLayout(
               sidebarPanel(
                 h4("Opcje Testu"),
                 selectInput("test_type", "Wybierz test statystyczny:",
                             choices = c("Test t-Studenta dla prób niezależnych" = "ttest",
                                         "Analiza wariancji (ANOVA)" = "anova")),
                 hr(),
                 
                 # UI dla Testu t
                 conditionalPanel(
                   condition = "input.test_type == 'ttest'",
                   p("Test t-Studenta porównuje średnie w dwóch grupach."),
                   selectInput("var_ttest_num", "Wybierz zmienną numeryczną:", choices = zmienne_numeryczne),
                   selectInput("var_ttest_cat", "Wybierz zmienną grupującą (2 poziomy):", choices = zmienne_kategoryczne_dwa_poziomy)
                 ),
                 
                 # UI dla ANOVA
                 conditionalPanel(
                   condition = "input.test_type == 'anova'",
                   p("ANOVA porównuje średnie w trzech lub więcej grupach."),
                   selectInput("var_anova_num", "Wybierz zmienną numeryczną:", choices = zmienne_numeryczne),
                   selectInput("var_anova_cat", "Wybierz zmienną grupującą (>2 poziomy):", choices = zmienne_kategoryczne_wiecej_poziomow)
                 )
               ),
               mainPanel(
                 h3("Wyniki Testu Statystycznego"),
                 verbatimTextOutput("testOutput"),
                 h4("Interpretacja Wyniku"),
                 uiOutput("testInterpretation")
               )
             )
    ),
    
    # --- Zakładka 3: Modelowanie ---
    tabPanel("Modelowanie (Regresja Logistyczna)",
             sidebarLayout(
               sidebarPanel(
                 h4("Budowa Modelu Predykcyjnego"),
                 p("Celem jest zbudowanie modelu, który przewiduje prawdopodobieństwo wystąpienia depresji ('Tak'/'Nie') na podstawie wybranych zmiennych."),
                 
                 checkboxGroupInput("model_predictors", "Wybierz zmienne objaśniające (predyktory):",
                                    choices = c(zmienne_numeryczne, zmienne_kategoryczne[zmienne_kategoryczne != "Depresja"]),
                                    selected = c("Stres_Finansowy", "Mysli_Samobojcze")),
                 
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
    
    # --- Zakładka 4: O Aplikacji ---
    tabPanel("O Aplikacji i Danych",
             fluidRow(
               column(12,
                      h3("Projekt Zaliczeniowy - Aplikacja Shiny"),
                      p("Ta aplikacja została stworzona jako projekt zaliczeniowy. Umożliwia interaktywną analizę danych dotyczących zdrowia psychicznego studentów."),
                      p("Aplikacja pozwala na:"),
                      tags$ul(
                        tags$li("Dynamiczne tworzenie wizualizacji danych."),
                        tags$li("Przeprowadzanie podstawowych testów statystycznych."),
                        tags$li("Budowanie modelu regresji logistycznej do predykcji depresji.")
                      ),
                      hr(),
                      h3("Podgląd Danych"),
                      p("Poniżej znajduje się tabela z przetworzonymi danymi użytymi w aplikacji."),
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
  
  # --- Logika Zakładki 1: Wizualizacja ---
  output$mainPlot <- renderPlot({
    
    # Użycie funkcji req() aby upewnić się, że dane wejściowe są dostępne
    req(input$plot_type)
    
    # Stworzenie wykresu w zależności od wyboru użytkownika
    p <- switch(input$plot_type,
                "boxplot" = {
                  req(input$var_y_box, input$var_x_box)
                  ggplot(df_clean, aes_string(x = input$var_x_box, y = input$var_y_box, fill = input$var_x_box)) +
                    geom_boxplot() +
                    labs(title = paste("Rozkład", input$var_y_box, "w grupach", input$var_x_box),
                         x = input$var_x_box, y = input$var_y_box) +
                    theme_minimal() + theme(legend.position = "none")
                },
                "hist" = {
                  req(input$var_hist)
                  ggplot(df_clean, aes_string(x = input$var_hist)) +
                    geom_histogram(bins = 20, fill = "skyblue", color = "black") +
                    labs(title = paste("Histogram dla zmiennej", input$var_hist),
                         x = input$var_hist, y = "Częstość") +
                    theme_minimal()
                },
                "scatter" = {
                  req(input$var_x_scatter, input$var_y_scatter)
                  ggplot(df_clean, aes_string(x = input$var_x_scatter, y = input$var_y_scatter)) +
                    geom_point(alpha = 0.6, color = "darkblue") +
                    geom_smooth(method = "lm", se = FALSE, color = "red") +
                    labs(title = paste("Zależność między", input$var_x_scatter, "a", input$var_y_scatter),
                         x = input$var_x_scatter, y = input$var_y_scatter) +
                    theme_minimal()
                }
    )
    
    # Wyświetlenie wykresu
    print(p)
  })
  
  # --- Logika Zakładki 2: Testy Statystyczne ---
  
  # Reaktywny obiekt przechowujący wyniki testu
  test_results <- reactive({
    if (input$test_type == 'ttest') {
      req(input$var_ttest_num, input$var_ttest_cat)
      formula <- as.formula(paste(input$var_ttest_num, "~", input$var_ttest_cat))
      t.test(formula, data = df_clean)
    } else if (input$test_type == 'anova') {
      req(input$var_anova_num, input$var_anova_cat)
      formula <- as.formula(paste(input$var_anova_num, "~", input$var_anova_cat))
      summary(aov(formula, data = df_clean))
    }
  })
  
  # Wyświetlanie surowych wyników
  output$testOutput <- renderPrint({
    test_results()
  })
  
  # Interpretacja wyników
  output$testInterpretation <- renderUI({
    res <- test_results()
    p_value <- if (input$test_type == 'ttest') res$p.value else res[[1]][["Pr(>F)"]][1]
    
    if (is.na(p_value)) return(p("Błąd w obliczeniach. Sprawdź wybrane zmienne."))
    
    if (p_value < 0.05) {
      tags$p(
        style = "color: green; font-weight: bold;",
        paste0("Wartość p = ", round(p_value, 4), ". Ponieważ p < 0.05, odrzucamy hipotezę zerową. Oznacza to, że istnieje statystycznie istotna różnica w średnich wartościach badanej zmiennej pomiędzy analizowanymi grupami.")
      )
    } else {
      tags$p(
        style = "color: red;",
        paste0("Wartość p = ", round(p_value, 4), ". Ponieważ p >= 0.05, nie ma podstaw do odrzucenia hipotezy zerowej. Oznacza to, że nie znaleziono statystycznie istotnych różnic w średnich wartościach badanej zmiennej pomiędzy analizowanymi grupami.")
      )
    }
  })
  
  # --- Logika Zakładki 3: Modelowanie ---
  
  # Model jest uruchamiany tylko po kliknięciu przycisku
  model_fit <- eventReactive(input$run_model, {
    req(input$model_predictors, length(input$model_predictors) > 0)
    
    # Tworzenie formuły modelu
    formula_str <- paste("Depresja ~", paste(input$model_predictors, collapse = " + "))
    
    # Dopasowanie modelu regresji logistycznej
    glm(as.formula(formula_str), data = df_clean, family = "binomial")
  })
  
  # Wyświetlanie podsumowania modelu
  output$modelSummary <- renderPrint({
    summary(model_fit())
  })
  
  # Interpretacja modelu
  output$modelInterpretation <- renderUI({
    # Czekaj na uruchomienie modelu
    req(model_fit())
    
    model_summary <- summary(model_fit())
    coeffs <- model_summary$coefficients
    
    # Znajdź istotne predyktory (p < 0.05)
    significant_preds <- rownames(coeffs)[coeffs[, "Pr(>|z|)"] < 0.05 & rownames(coeffs) != "(Intercept)"]
    
    # Tworzenie tekstu interpretacji
    tagList(
      p("Model regresji logistycznej ocenia, jak wybrane zmienne wpływają na szansę (odds) wystąpienia depresji."),
      p("Współczynniki (Estimate) mówią nam o kierunku i sile tego wpływu:"),
      # ***** POCZĄTEK POPRAWIONEGO FRAGMENTU *****
      tags$ul(
        tags$li(strong("Współczynnik dodatni:"), " zwiększa szansę na wystąpienie depresji."),
        tags$li(strong("Współczynnik ujemny:"), " zmniejsza szansę na wystąpienie depresji.")
      ),
      # ***** KONIEC POPRAWIONEGO FRAGMENTU *****
      hr(),
      if (length(significant_preds) > 0) {
        p(strong("Statystycznie istotne predyktory (p < 0.05) w tym modelu to:"),
          tags$br(),
          paste(significant_preds, collapse = ", ")
        )
      } else {
        p(strong("W tym modelu nie znaleziono żadnych statystycznie istotnych predyktorów."))
      }
    )
  })
  
  # --- Logika Zakładki 4: Tabela Danych ---
  output$dataTable <- DT::renderDataTable({
    DT::datatable(df_clean, options = list(pageLength = 10, scrollX = TRUE))
  })
  
}

# ------------------------------------------------------------------
# 5. URUCHOMIENIE APLIKACJI
# ------------------------------------------------------------------

shinyApp(ui = ui, server = server)
