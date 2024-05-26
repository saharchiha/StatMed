# -*- coding: UTF-8 -*-
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(reshape2)
# D??finition de l'interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "StatMed"),
  # D??finition des volets:
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Management", tabName = "data_management", icon = icon("database"),
               menuSubItem("Import Dataset", tabName = "import_dataset"),
               menuSubItem("Data Summary", tabName = "data_summary"),
               menuSubItem("Data Cleaning", tabName = "data_cleaning")
      ),
      menuItem("Descriptive Analysis", tabName = "descriptive_analysis", icon = icon("chart-bar"),
               menuSubItem("Data Histogram", tabName = "data_histogram"),
               menuSubItem("Boxplot", tabName = "boxplot"),
               menuItem("Bivariate table", tabName = "bivariate_table", icon = icon("table")),
               menuItem("Linear Regression", tabName = "linear_regression", icon = icon("tachometer-alt")),
               menuItem("Correlation Matrix", tabName = "correlation_matrix", icon = icon("chart-line"))
      ),
      menuItem("Tests", tabName = "Tests", icon = icon("check"),
               menuSubItem("Correlation Test", tabName = "Correlation_Test"),
               menuSubItem("Independence Test", tabName = "Independence_Test"),
               menuSubItem("Normality Test", tabName = "Normality_test")
      )
    ) 
  ),
   dashboardBody(
     tags$head(
       tags$meta(charset = "UTF-8")
     ),
    tabItems(
      # Data Management Tabs
      tabItem(tabName = "import_dataset",
              fileInput("file", "Choose your File"),
              actionButton("import", "Import Dataset", icon = icon("upload")),
              dataTableOutput("datatable")
      ),
      tabItem(tabName = "data_summary",
              verbatimTextOutput("summary")
      ),
      tabItem(tabName = "data_cleaning",
              box(
                selectInput("variable", "Variable ?? imputer :", choices = NULL),
                selectInput("method", "M??thode d'imputation :", choices = c("Suppression de lignes", "M??diane", "Moyenne", "Valeur personnalis??e")),
                numericInput("custom_value", "Valeur personnalis??e :", value = 0, min = -100, max = 100),
                actionButton("impute", "Imputer")
              ),
              box(
                titlePanel("Histogramme des valeurs manquantes par variable"),
                plotOutput("missing_values_histogram")
              )
      ),
      # Descriptive Analysis Tabs:
      tabItem(tabName = "data_histogram",
              selectInput("hist_variable", "Choose a variable:", choices = NULL),
              plotOutput("data_histogram_plot")
      ),
      tabItem(tabName = "boxplot",
              selectInput("box_variable", "Choose a variable:", choices = NULL),
              plotOutput("boxplot_plot")
      ),
      tabItem(tabName = "bivariate_table",
              fluidRow(
                box(
                  title = "S??lectionner les Variables",
                  selectInput("x_var", "Variable X:", choices = NULL),
                  selectInput("y_var", "Variable Y:", choices = NULL)
                )
              ),
              fluidRow(
                box(
                  title = "Tableau Bivari??",
                  dataTableOutput("bivariate_table")
                )
              )
      ),
      tabItem(tabName = "linear_regression",
              fluidRow(
                box(
                  title = "S??lectionner les Variables",
                  selectInput("x_var_lr", "Variable X:", choices = NULL),
                  selectInput("y_var_lr", "Variable Y:", choices = NULL)
                )
              ),
              fluidRow(
                box(
                  title = "R??gression Lin??aire",
                  plotOutput("linear_regression_plot")
                )
              )
      ),
      # Correlation Matrix Tab:
      tabItem(tabName = "correlation_matrix",
              plotOutput("correlation_plot")
      ),
      tabItem(tabName = "Correlation_Test",
              fluidRow(
                box(
                  title = "S??lectionner les Variables",
                  selectInput("corr_var1", "Variable X:", choices = NULL),
                  selectInput("corr_var2", "Variable Y:", choices = NULL)
                )),
              fluidRow(
                box(
                  plotOutput("scatter_plot"),
                  textOutput("correlation_output")
                )
              )
      ),
      # Independence Test (Chi-squared Test):
      tabItem(tabName = "Independence_Test",
              fluidRow(
                box(
                  title = "S??lectionner les Variables Qualitatives",
                  selectInput("ind_var1", "Variable X:", choices = NULL),
                  selectInput("ind_var2", "Variable Y:", choices = NULL)
                )
              ),
              fluidRow(
                box(
                  title = "Distribution des Variables",
                  plotOutput("var1_boxplot"),
                  plotOutput("var2_boxplot")
                )
              ),
              fluidRow(
                box(
                  title = "R??sultats du Test Chi-deux",
                  textOutput("test_result")
                )
              )
      ),
      # normality test:
      tabItem(tabName = "Normality_test",
              fluidRow(
                box(
                  title = "S??lectionner une Variable",
                  selectInput("norm_var", "Variable :", choices = NULL)
              )
              ),
              fluidRow(
                box(
                  title = "Distribution des Variables",
                  plotOutput("density_plot"))
              ),
              fluidRow(
                box(
                  title = "R??sultats du Test shapiro??",
                  textOutput("shapiro_test_result")
                )
              )
      )
      
    )
  )
)


# d??finir le serveur logique 
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  #importer n'imorte quel fichier avec n'importe quel extension 
  observeEvent(input$import, {
    req(input$file)
    file_ext <- tools::file_ext(input$file$name)
    if (tolower(file_ext) == "csv") {
      data(read.csv(input$file$datapath))
    } else if (tolower(file_ext) == "xls" || tolower(file_ext) == "xlsx") {
      data(readxl::read_excel(input$file$datapath))
    } else if (tolower(file_ext) == "txt") {
      data(read.table(input$file$datapath, header = TRUE))
    }
    #lier les selecteurs avec les variables de b.o. import??e
    updateSelectInput(session, "variable", choices = names(data()))
    updateSelectInput(session, "hist_variable", choices = names(data()))
    updateSelectInput(session, "box_variable", choices = names(data()))
    updateSelectInput(session, "x_var", choices = names(data()))
    updateSelectInput(session, "y_var", choices = names(data()))
    updateSelectInput(session, "x_var_lr", choices = names(data()))
    updateSelectInput(session, "y_var_lr", choices = names(data()))
    updateSelectInput(session, "corr_var1", choices = names(data()))
    updateSelectInput(session, "corr_var2", choices = names(data()))
    updateSelectInput(session, "ind_var1", choices = names (data()))
    updateSelectInput(session, "ind_var2", choices = names(data()))
    updateSelectInput(session, "norm_var", choices = names(data()))
    
  })
  #afficher la b.o.:
  output$datatable <- renderDT({
    datatable(data(), editable = TRUE)
  })
  #afficher le tableau de stat.desc.:
  output$summary <- renderPrint({
    if (!is.null(data())) {
      summary(data())
    }
  })
  #afficher le diagramme des valeurs manquante de chaque variable :
  output$missing_values_histogram <- renderPlot({
    if (!is.null(data())) {
      missing_values <- colSums(is.na(data()))
      missing_df <- data.frame(variable = names(missing_values), count = missing_values)
      ggplot(missing_df, aes(x = variable, y = count, fill = variable)) +
        geom_col() +
        labs(title = "Histogramme des valeurs manquantes par variable", x = "Variables", y = "Nombre de valeurs manquantes") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  #imputation des valeurs manquante :
  observeEvent(input$impute, {
    if (!is.null(data())) {
      method <- input$method
      variable <- input$variable
      imputed_data <- data()
      switch(method,
             "Suppression de lignes" = {
               imputed_data <- imputed_data[complete.cases(imputed_data), ]
             },
             "M??diane" = {
               imputed_data[is.na(imputed_data[, variable]), variable] <- median(imputed_data[, variable], na.rm = TRUE)
             },
             "Moyenne" = {
               imputed_data[is.na(imputed_data[, variable]), variable] <- mean(imputed_data[, variable], na.rm = TRUE)
             },
             "Valeur personnalis??e" = {
               imputed_data[is.na(imputed_data[, variable]), variable] <- input$custom_value
             }
      )
      data(imputed_data)
    }
  })
  
  # Placeholder pour data histogram visualization
  output$data_histogram_plot <- renderPlot({
    if (!is.null(data()) && !is.null(input$hist_variable)) {
      ggplot(data(), aes_string(x = input$hist_variable)) +
        geom_histogram(fill = "blue", color = "black", bins = 10) +
        labs(title = paste("Histogramme de", input$hist_variable),
             x = input$hist_variable,
             y = "Fr??quence") 
    }
  })
  
  # Placeholder pour boxplot visualization
  output$boxplot_plot <- renderPlot({
    if (!is.null(data()) && !is.null(input$box_variable)) {
      ggplot(data(), aes_string(y = input$box_variable)) +
        geom_boxplot(fill = "skyblue", color = "black") +
        labs(title = paste("Boxplot de", input$box_variable),
             y = input$box_variable)
    }
  })
  
  # Fonction de cr??ation du tableau bivari??
  output$bivariate_table<- renderDT({
    if (!is.null(input$x_var) && !is.null(input$y_var)){
      tab <- table(data()[[input$x_var]], data()[[input$y_var]])
      datatable(tab, caption = "Tableau Bivari??")
    }
  })
  
  # Placeholder pour linear regression plot
  output$linear_regression_plot <- renderPlot({
    if (!is.null(data()) && !is.null(input$x_var_lr) && !is.null(input$y_var_lr)) {
      lm_model <- lm(data()[[input$y_var_lr]] ~ data()[[input$x_var_lr]], data = data())
      plot(data()[[input$x_var_lr]], data()[[input$y_var_lr]], pch = 16, col = "blue", xlab = input$x_var_lr, ylab = input$y_var_lr)
      abline(lm_model, col = "red")
      title(main = "Linear Regression Plot")
    }
  })
  
  # Placeholder pour correlation matrix plot
  output$correlation_plot <- renderPlot({
    if (!is.null(data())) {
      # Filtrer les variables num??riques
      numeric_data <- data()[sapply(data(), is.numeric)]
      
      # Calcul de la matrice de corr??lation
      correlation_matrix <- cor(numeric_data)
      
      # Conversion de la matrice de corr??lation en format de donn??es longues
      correlation_long <- melt(correlation_matrix)
      
      # Trac?? de la matrice de corr??lation
      ggplot(correlation_long, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient(low = "blue", high = "red") +
        labs(title = "Matrice de corr??lation",
             x = "Variables",
             y = "Variables")
    }
  })
  # Calculer la corr??lation entre les variables s??lectionn??es
  output$correlation_output <- renderText({
    correlation_coefficient <- cor(data()[[input$corr_var1]], data()[[input$corr_var2]])
    paste("Coefficient de corr??lation (r) entre", input$corr_var1, "et", input$corr_var2, ":", correlation_coefficient)
  })
  
  # Afficher le scatter plot des variables s??lectionn??es
  output$scatter_plot <- renderPlot({
    ggplot(data(), aes_string(x = input$corr_var1, y = input$corr_var2)) +
      geom_point() +
      labs(title = paste("Scatter Plot de", input$corr_var2, "en fonction de", input$corr_var1),
           x = input$corr_var1,
           y = input$corr_var2)
  })
  #boxplot de la variable selectionn??e : 
  output$var1_boxplot <- renderPlot({
    req(data())
    if (!is.null(input$ind_var1)) {
      ggplot(data(), aes_string(x = input$ind_var1, fill = input$ind_var1)) +
        geom_bar() +
        labs(title = paste("Distribution de", input$ind_var1))
    }
  })
  
  output$var2_boxplot <- renderPlot({
    req(data())
    if (!is.null(input$ind_var2)) {
      ggplot(data(), aes_string(x = input$ind_var2, fill = input$ind_var2)) +
        geom_bar() +
        labs(title = paste("Distribution de", input$ind_var2))
    }
  })
  
  observeEvent(c(input$ind_var1, input$ind_var2), {
    req(data())
    if (!is.null(input$ind_var1) && !is.null(input$ind_var2)) {
      # Creation du tableau de contingence:
      contingency_table <- table(data()[[input$ind_var1]], data()[[input$ind_var2]])
      
      # Performer Chi-squared test
      chi2_result <- tryCatch({
        chisq.test(contingency_table)
      }, error = function(e) {
        return(NULL)  # Retourner NULL en cas d'error (e.g., insufficient data)
      })
      
      # afficher les r??sultats
      output$test_result <- renderPrint({
       print(chi2_result)
      })
    }
  })

  
  # Graphique pour la distrubtion de la variable s??letionn??e: 
  output$density_plot <- renderPlot({
    req(data())
    if (!is.null(input$norm_var)) {
      ggplot(data(), aes_string(x = input$norm_var)) +
        geom_density(fill = "blue", alpha = 0.4) +
        labs(title = paste("Distribution de", input$norm_var),
             x = input$norm_var)
    }
  })
  
  # Shapiro-Wilk Test resultats:
  output$shapiro_test_result <- renderPrint({
    req(data())
    if (!is.null(input$norm_var)) {
      # Performer Shapiro-Wilk test et afficher les r??sultats:
      shapiro_result <- tryCatch({
        shapiro.test(data()[[input$norm_var]])
      }, error = function(e) {
        return(NULL)  # Return NULL on error (e.g., insufficient data)
      })
      print(shapiro_result)
      
    }
  })
  
}
  
# Run the application
shinyApp(ui, server)