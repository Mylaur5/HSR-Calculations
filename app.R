# Shiny app : EHR Calculator

# For a lvl 80 SW with lvl 80 Tutorial LC, is Hp% orb or Def% orb better ?
# Stats : Head (705 Hp) + Hp% orb : +43.2% or +54% Def% Orb


library(shiny)
library(ragg)
library(ggplot2)
library(ggpubr)
library(purrr)
library(dplyr)
library(magrittr)
library(vroom)

# Load data
character_data <- vroom("data/character_data.tsv") %>%
  rename(character_name = colnames(.[1]))
light_cone_data <- vroom("data/light_cone_data.tsv") %>%
  rename(lc_name = colnames(.[1]))

# Define UI
ui <- fluidPage(
  # App title
  titlePanel("EHP Calculator"),

  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      selectInput("character_selection", "Select Character:", choices = character_data$character_name),
      selectInput("lc_selection", "Select Light Cone:", choices = light_cone_data$lc_name), # Initially empty

      # HP Substat value input
      numericInput("hp_percent", "HP Bonus (%)", 0, min = 0, max = 100),
      numericInput("flat_hp", "Flat HP", 0, min = 0, max = 1000),

      # DEF Substat value input
      numericInput("def_percent", "Defense Bonus (%)", 0, min = 0, max = 100),
      numericInput("flat_def", "Flat DEF", 0, min = 0, max = 1000),

      # Orb Type selection
      checkboxGroupInput("orb_type", "Select Orb Type:",
        choices = c("HP% Orb (43.2 %) " = "hp_orb", "Def% Orb (54 %)" = "def_orb"),
        selected = c("hp_orb", "def_orb")
      ),

      # Head Piece selection
      checkboxInput("head_piece", "Head Piece (705 HP)", value = TRUE),
      sliderInput("x_limit", "X-axis Limit:", 4000, min = 0, max = 5000),
      # Attacker level input
      sliderInput("attacker_level", "Attacker Level:", min = 1, max = 100, value = 90)
    ),

    # Main panel for displaying outputs
    mainPanel(
      plotOutput("ehp_plot"),
      tableOutput("character_stats"),
      tableOutput("ehp_stats"),
      tableOutput("lc_stats"),
      verbatimTextOutput("character_stats_text")
    )
  )
)



server <- function(input, output, session){
  # Create a reactive expression to filter light cone options based on character selection
  filtered_light_cones <- reactive({
    selected_character <- input$character_selection
    character_path <- character_data$character_path[character_data$character_name == selected_character]

    # Filter light cone data based on character_path
    light_cone_data_filtered <- light_cone_data %>%
      filter(lc_path == character_path)

    # Return the names of the filtered light cones
    light_cone_data_filtered$lc_name
  })

  # Update the choices in the light_cone dropdown based on the reactive expression
  observe({
    choices <- filtered_light_cones()
    updateSelectInput(session, "lc_selection", choices = choices)
  })

  observeEvent(c(input$hp_percent, input$def_percent), {
    if (is.null(input$hp_percent)) {
      updateNumericInput(session, "hp_percent", value = 0)
    }
    if (is.null(input$def_percent)) {
      updateNumericInput(session, "def_percent", value = 0)
    }
  })

  # Create a reactive expression for calculations
  data <- reactive({
    # Filter data for the selected character
    selected_character_stats <- character_data[character_data$character_name == input$character_selection, ]
    # Extract character stats
    base_hp <- selected_character_stats$HP
    base_def <- selected_character_stats$DEF
    # Extract the selected light cone's base stats
    lc_data <- light_cone_data[light_cone_data$lc_name == input$lc_selection, ]
    lc_base_hp <- lc_data$HP
    lc_base_def <- lc_data$DEF
    # Add head_piece
    head_piece <- ifelse(input$head_piece, 705, 0)

    data <- dplyr::tibble(
      total_base_hp = base_hp + lc_base_hp,
      head_piece = head_piece,
      flat_hp = input$flat_hp,
      flat_def = input$flat_def,
      total_hp = total_base_hp * (1 + input$hp_percent / 100) + head_piece + flat_hp,
      total_base_def = base_def + lc_base_def,
      total_def = total_base_def * (1 + input$def_percent / 100) + flat_def,
      def_multiplier = total_def / (total_def + 200 + 10 * input$attacker_level)
    )
    # We use tibble because data.frame cannot reuse data that just got defined in the df
    return(data)
  })
  # Create a function to calculate EHP
  calculate_ehp <-
    function(total_base_hp,
             hp_percent = input$hp_percent,
             head_piece = data()$head_piece,
             def_percent = input$def_percent,
             attacker_level = input$attacker_level,
             flat_hp = input$flat_hp,
             flat_def = input$flat_def) {
      total_def <- data()$total_base_def * (1 + def_percent / 100) + flat_def
      def_multiplier <-
        total_def / (total_def + 200 + 10 * attacker_level)
      total_hp <- total_base_hp * (1 + hp_percent / 100) + head_piece + flat_hp
      ehp <- total_hp / (1 - def_multiplier)
      return(ehp)
    }

  # Check if at least one checkbox is active

  output$ehp_plot <- renderPlot({
    ehp_curve <- ggplot() +
      xlim(0, input$x_limit) +
      geom_function(
        aes(color = "Base EHP"),
        fun = calculate_ehp,
        args = list(
          hp_percent = input$hp_percent,
          head_piece = data()$head_piece,
          def_percent = input$def_percent,
          attacker_level = input$attacker_level
        ),
        show.legend = TRUE # Show legend for this layer
      ) +
      geom_point(
        aes(
          x = data()$total_base_hp,
          y = calculate_ehp(data()$total_base_hp)
        ),
        color = "black",
        size = 2
      ) +
      labs(
        title = "EHP vs Base HP",
        x = "Total Base HP",
        y = "Effective HP"
      ) +
      scale_colour_manual(
        name = "Legend",
        values = c("Base EHP" = "black", "HP% Orb" = "darkgreen", "Def% Orb" = "orange")
      ) +
      theme_pubclean()

    if ("def_orb" %in% input$orb_type) {
      ehp_curve <- ehp_curve +
        geom_function(
          aes(color = "Def% Orb"),
          fun = calculate_ehp,
          args = list(
            hp_percent = input$hp_percent,
            head_piece = data()$head_piece,
            def_percent = input$def_percent + 54,
            attacker_level = input$attacker_level
          ),
          show.legend = TRUE
        ) +
        geom_point(
          aes(
            x = data()$total_base_hp,
            y = calculate_ehp(data()$total_base_hp,
              def_percent = input$def_percent + 54
            ),
            color = "orange",
            size = 1
          )
        )
    }
    if ("hp_orb" %in% input$orb_type) {
      ehp_curve <- ehp_curve +
        geom_function(
          aes(color = "HP% Orb"),
          fun = calculate_ehp,
          args = list(
            hp_percent = input$hp_percent + 43.2,
            head_piece = data()$head_piece,
            def_percent = input$def_percent,
            attacker_level = input$attacker_level
          ),
          show.legend = TRUE # Show legend for this layer
        ) +
        geom_point(
          aes(
            x = data()$total_base_hp,
            y = calculate_ehp(data()$total_base_hp,
              hp_percent = input$hp_percent + 43.2
            ),
            color = "darkgreen",
            size = 1
          )
        )
    }

    print(ehp_curve)
  })



  output$character_stats <- renderTable({
    # Filter data for the selected character
    selected_character_stats <- character_data[character_data$character_name == input$character_selection, ]
    # Extract character stats
    base_hp <- selected_character_stats$HP

    # Calculate the Total Base HP
    total_base_hp <- data()$total_base_hp



    # Create a data frame for the table
    character_stats <- data.frame(
      "Max Lvl Character" = input$character_selection,
      "Base HP" = base_hp,
      "Base DEF" = selected_character_stats$DEF,
      "Total Base DEF" = data()$total_base_def,
      "Total Base DEF with Subs" = data()$total_def,
      "Total Base HP" = total_base_hp,
      "Total HP with Subs" = data()$total_hp
    )
    return(character_stats)
  })

  output$ehp_stats <- renderTable({
    # Calculate EHP gain from HP and Def orbs
    ehp_gain_hp_orb <- calculate_ehp(
      total_base_hp = data()$total_base_hp,
      hp_percent = input$hp_percent + 43.2,
      def_percent = input$def_percent,
      head_piece = data()$head_piece,
      attacker_level = input$attacker_level
    )

    ehp_gain_def_orb <- calculate_ehp(
      total_base_hp = data()$total_base_hp,
      hp_percent = input$hp_percent,
      def_percent = input$def_percent + 54,
      head_piece = data()$head_piece,
      attacker_level = input$attacker_level
    )
    ehp_calculated <- calculate_ehp(
      total_base_hp = data()$total_base_hp,
      hp_percent = input$hp_percent,
      def_percent = input$def_percent,
      head_piece = data()$head_piece,
      attacker_level = input$attacker_level
    )
    ehp_stats <- dplyr::tibble(
      "EHP without Orb" = ehp_calculated,
      "EHP (HP% Orb)" = ehp_gain_hp_orb,
      "EHP (Def% Orb)" = ehp_gain_def_orb
    )
    return(ehp_stats)
  })


  output$lc_stats <- renderTable({
    lc_data <- light_cone_data[light_cone_data$lc_name == input$lc_selection, ]
    lc_stats <- data.frame(
      "LC Name" = lc_data$lc_name,
      "LC Base HP" = lc_data$HP,
      "LC Base DEF" = lc_data$DEF
    )
    return(lc_stats)
  })
}

shinyApp(ui, server)

