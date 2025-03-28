library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Fonctions précédentes du script original
generalized_excitation <- function(x, s_plus, a_ex = 1, sigma_ex = 1) {
  excitation <- a_ex * exp(-((x - s_plus)^2) / (2 * sigma_ex^2))
  return(excitation)
}

generalized_inhibition <- function(x, s_minus, a_inh = 0.8, sigma_inh = 1.5) {
  inhibition <- -a_inh * exp(-((x - s_minus)^2) / (2 * sigma_inh^2))
  return(inhibition)
}

hull_spence_net_response <- function(x, s_plus, s_minus, a_ex = 1, a_inh = 0.8, 
                                     sigma_ex = 1, sigma_inh = 1.5) {
  excitation <- generalized_excitation(x, s_plus, a_ex, sigma_ex)
  inhibition <- generalized_inhibition(x, s_minus, a_inh, sigma_inh)
  
  net_tendency <- excitation + inhibition
  net_response <- pmax(0, net_tendency)
  
  return(list(
    excitation = excitation,
    inhibition = inhibition,
    net_tendency = net_tendency,
    net_response = net_response
  ))
}

# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Modélisation de la théorie de Hull et Spence"),
  
  sidebarLayout(
    sidebarPanel(
      # Paramètres pour S+
      sliderInput("s_plus", "Stimulus Conditionné (S+)", 
                  min = 0, max = 15, value = 5, step = 0.5),
      
      # Paramètres pour S-
      sliderInput("s_minus", "Stimulus Non-Conditionné (S-)", 
                  min = 0, max = 15, value = 8, step = 0.5),
      
      # Paramètres d'excitation
      sliderInput("a_ex", "Amplitude de l'Excitation", 
                  min = 0, max = 2, value = 1, step = 0.1),
      sliderInput("sigma_ex", "Dispersion de l'Excitation", 
                  min = 0.1, max = 3, value = 1, step = 0.1),
      
      # Paramètres d'inhibition
      sliderInput("a_inh", "Amplitude de l'Inhibition", 
                  min = 0, max = 2, value = 0.8, step = 0.1),
      sliderInput("sigma_inh", "Dispersion de l'Inhibition", 
                  min = 0.1, max = 3, value = 1.5, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("hull_spence_plot"),
      
      tags$h4("Métriques Importantes"),
      tableOutput("metrics_table")
    )
  )
)

# Serveur Shiny
server <- function(input, output) {
  # Calcul des réponses
  response_data <- reactive({
    stimulus_values <- seq(0, 15, by = 0.1)
    
    responses <- hull_spence_net_response(
      stimulus_values, 
      input$s_plus, 
      input$s_minus, 
      input$a_ex, 
      input$a_inh, 
      input$sigma_ex, 
      input$sigma_inh
    )
    
    results <- data.frame(
      stimulus = stimulus_values,
      excitation = responses$excitation,
      inhibition = responses$inhibition,
      net_tendency = responses$net_tendency,
      net_response = responses$net_response
    )
    
    results_long <- results %>%
      pivot_longer(cols = c(excitation, inhibition, net_tendency),
                   names_to = "curve_type", values_to = "value")
    
    list(results = results, results_long = results_long)
  })
  
  # Calcul des métriques
  metrics <- reactive({
    results <- response_data()$results
    
    list(
      max_excitation_value = max(results$excitation),
      max_excitation_at = results$stimulus[which.max(results$excitation)],
      max_inhibition_value = min(results$inhibition),
      max_inhibition_at = results$stimulus[which.min(results$inhibition)],
      max_response_value = max(results$net_response),
      max_response_at = results$stimulus[which.max(results$net_response)],
      peak_shift = results$stimulus[which.max(results$net_response)] - input$s_plus
    )
  })
  
  # Graphique principal
  output$hull_spence_plot <- renderPlot({
    results_long <- response_data()$results_long
    results <- response_data()$results
    
    p1 <- ggplot(results_long, aes(x = stimulus, y = value, color = curve_type)) +
      geom_line(size = 1) +
      geom_vline(xintercept = input$s_plus, linetype = "dashed", color = "darkgreen") +
      geom_vline(xintercept = input$s_minus, linetype = "dashed", color = "darkred") +
      scale_color_manual(values = c("excitation" = "green3", 
                                    "inhibition" = "red3", 
                                    "net_tendency" = "blue3"),
                         labels = c("Excitation", "Inhibition", "Tendance nette")) +
      labs(title = "Courbes d'excitation, d'inhibition et tendance nette",
           x = "Valeur du stimulus", y = "Force de la réponse",
           color = "Type de courbe") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    p2 <- ggplot(results, aes(x = stimulus, y = net_response)) +
      geom_line(size = 1, color = "blue3") +
      geom_vline(xintercept = input$s_plus, linetype = "dashed", color = "darkgreen") +
      geom_vline(xintercept = input$s_minus, linetype = "dashed", color = "darkred") +
      labs(title = "Tendance nette à répondre",
           x = "Valeur du stimulus", y = "Force de la réponse") +
      theme_minimal() +
      annotate("text", x = input$s_plus, y = max(results$net_response) * 0.9, 
               label = "S+", color = "darkgreen", hjust = -0.5) +
      annotate("text", x = input$s_minus, y = max(results$net_response) * 0.9, 
               label = "S-", color = "darkred", hjust = -0.5)
    
    combined_plot <- p1 / p2
    print(combined_plot)
  })
  
  # Tableau des métriques
  output$metrics_table <- renderTable({
    m <- metrics()
    data.frame(
      Métrique = c(
        "Valeur max d'excitation",
        "Position max d'excitation",
        "Valeur max d'inhibition",
        "Position max d'inhibition", 
        "Valeur max de réponse",
        "Position max de réponse",
        "Déplacement du pic"
      ),
      Valeur = c(
        round(m$max_excitation_value, 3),
        round(m$max_excitation_at, 3),
        round(m$max_inhibition_value, 3),
        round(m$max_inhibition_at, 3),
        round(m$max_response_value, 3),
        round(m$max_response_at, 3),
        round(m$peak_shift, 3)
      )
    )
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
