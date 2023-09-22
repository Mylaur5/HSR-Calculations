# Shiny app : EHR Calculator

# For a lvl 80 SW with lvl 80 Tutorial LC, is Hp% orb or Def% orb better ?
# Stats : Head (705 Hp) + Hp% orb : +43.2% or +54% DEF% Orb


library(shiny)
library(ragg)
library(ggplot2)
library(ggpubr)
library(purrr)
library(dplyr)
library(magrittr)
library(vroom)
library(stringr)
library(shinythemes)

# Load data
character_data <- vroom("data/character_data.tsv") %>%
  rename(character_name = colnames(.[1]))
light_cone_data <- vroom("data/light_cone_data.tsv") %>%
  rename(lc_name = colnames(.[1]))


# Define UI ---------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("paper"),
  shinythemes::themeSelector(),
  navbarPage(
    "HSR Calculator",
    # TabsetPanel for organizing different sections of the app
    tabPanel(
      "EHP",
      sidebarLayout(
        # Sidebar panel for inputs
        sidebarPanel(
          selectInput("character_selection", "Select Character:", choices = character_data$character_name),
          selectInput("lc_selection", "Select Light Cone:", choices = light_cone_data$lc_name),

          # Custom text input for HP Bonus (%)
          textInput("hp_percent_input", "HP Bonus (%)", "0"),
          textInput("flat_hp_input", "Flat HP", "0"),

          # DEF Substat value input
          textInput("def_percent_input", "Defense Bonus (%)", "0"),
          textInput("flat_def_input", "Flat DEF", "0"),

          # Orb Type selection
          checkboxGroupInput("orb_type", "Select Orb Type:",
            choices = c("HP% Orb (43.2 %) " = "hp_orb", "DEF% Orb (54 %)" = "def_orb"),
            selected = c("hp_orb", "def_orb")
          ),

          # Head Piece selection
          checkboxInput("head_piece", "Head Piece (705 HP)", value = TRUE),
          sliderInput("x_limit", "X-axis Limit:", value = c(0, 4000), min = 0, max = 5000),
          sliderInput("attacker_level", "Attacker Level:", min = 1, max = 100, value = 90)
        ),

        # Main panel for displaying outputs
        mainPanel(
          plotOutput("ehp_plot"),
          tableOutput("character_stats"),
          tableOutput("ehp_stats"),
          tableOutput("lc_stats")
        )
      ),
    ),

    ## ATK Panel ---------------------------------------------------------------

    tabPanel(
      "ATK",
      sidebarLayout(
        # Sidebar panel for inputs (unique IDs within this tab)
        sidebarPanel(
          selectInput("atk_character_selection", "Select Character:", choices = character_data$character_name),
          selectInput("atk_lc_selection", "Select Light Cone:", choices = light_cone_data$lc_name),
          tabsetPanel( # TabsetPanel for Substats and Fixed Stats
            tabPanel(
              "Substats", # Substats panel
              numericInput("skill_multiplier", "Skill Multiplier", 100, min = 0, max = 1000),
              checkboxGroupInput("line", "Toggles:",
                choices = c("ATK% " = "atk_toggle", "Crit Rate" = "crit_rate_toggle", "Crit Damage" = "crit_damage_toggle", "Damage%" = "dmg_toggle"),
                selected = "atk_toggle"
              ),
              textInput("atk_percent", "ATK Bonus (%)", "0"),
              textInput("flat_atk", "Flat ATK", "0"),
              textInput("crit_rate", "CRIT Rate (%)", "0"),
              textInput("crit_damage", "CRIT Damage (%)", "0"),
              textInput("dmg_multiplier", "DMG Multiplier (%)", "0"),
              sliderInput("x_limit_atk", "X-axis Limit:", value = c(0, 2000), min = 0, max = 5000),
              checkboxInput("hp_scaling", "HP Scaling", value = FALSE),
              checkboxInput("def_scaling", "DEF Scaling", value = FALSE)
            ),
            tabPanel(
              "Fixed Stats", # Fixed Stats panel
              textInput("atk_percent_base", "ATK Bonus (%)", "0"),
              textInput("flat_atk_base", "Flat ATK", "0"),
              textInput("crit_rate_base", "CRIT Rate (%)", "5"),
              textInput("crit_damage_base", "CRIT Damage (%)", "50"),
              textInput("dmg_multiplier_base", "DMG Multiplier (%)", "0"),
            )
          )
        ),
        # Main panel for displaying outputs (unique IDs within this tab)
        mainPanel(
          plotOutput("atk_plot"),
          tableOutput("character_stats_atk"),
          tableOutput("lc_stats_atk")
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  # EHP Calculator -----------------------------------------------------------


  ## Create a reactiveVal to store the calculated stat bonus =================
  # (allows user to add multiple values inside the box)
  calculated_numeric_input <- function(input_id) {
    calculated_value <- reactiveVal(0)

    observe({
      input_expression <- input[[input_id]]

      if (is.null(input_expression) || str_detect(input_expression, "[^0-9+. ]+")) {
        calculated_value(0)
      } else {
        result <- try(eval(parse(text = input_expression)), silent = TRUE)
        if (class(result) == "try-error") {
          calculated_value(0)
        } else {
          calculated_value(result)
        }
      }
    })

    return(calculated_value)
  }
  calculated_def_percent <- calculated_numeric_input("def_percent_input")
  calculated_hp_percent <- calculated_numeric_input("hp_percent_input")
  calculated_flat_hp <- calculated_numeric_input("flat_hp_input")
  calculated_flat_def <- calculated_numeric_input("flat_def_input")
  calculated_atk_percent <- calculated_numeric_input("atk_percent")
  calculated_atk_percent_base <- calculated_numeric_input("atk_percent_base")
  calculated_flat_atk <- calculated_numeric_input("flat_atk")
  calculated_flat_atk_base <- calculated_numeric_input("flat_atk_base")
  calculated_crit_rate <- calculated_numeric_input("crit_rate")
  calculated_crit_rate_base <- calculated_numeric_input("crit_rate_base")
  calculated_crit_damage <- calculated_numeric_input("crit_damage")
  calculated_crit_damage_base <- calculated_numeric_input("crit_damage_base")
  calculated_dmg_multiplier <- calculated_numeric_input("dmg_multiplier")
  calculated_dmg_multiplier_base <- calculated_numeric_input("dmg_multiplier_base")

  ## Create a reactive expression to filter light cone options ===============
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
      flat_hp = calculated_flat_hp(),
      flat_def = calculated_flat_def(),
      total_hp = total_base_hp * (1 + calculated_hp_percent() / 100) + head_piece + flat_hp,
      total_base_def = base_def + lc_base_def,
      total_def = total_base_def * (1 + calculated_def_percent() / 100) + flat_def,
      def_multiplier = total_def / (total_def + 200 + 10 * input$attacker_level)
    )
    # We use tibble because data.frame cannot reuse data that just got defined in the data.frame
    return(data)
  })
  # Create a function to calculate EHP
  calculate_ehp <-
    function(total_base_hp,
             hp_percent = calculated_hp_percent(),
             head_piece = data()$head_piece,
             def_percent = calculated_def_percent(),
             attacker_level = input$attacker_level,
             flat_hp = calculated_flat_hp(),
             flat_def = calculated_flat_def()) {
      total_def <- data()$total_base_def * (1 + def_percent / 100) + flat_def
      def_multiplier <-
        total_def / (total_def + 200 + 10 * attacker_level)
      total_hp <- total_base_hp * (1 + hp_percent / 100) + head_piece + flat_hp
      ehp <- total_hp / (1 - def_multiplier)
      return(ehp)
    }


  ## Render EHP Plot ---------------------------------------------------------


  output$ehp_plot <- renderPlot({
    ehp_curve <- ggplot() +
      xlim(input$x_limit) +
      geom_function(
        aes(color = "Base EHP"),
        fun = calculate_ehp,
        args = list(
          hp_percent = calculated_hp_percent(),
          head_piece = data()$head_piece,
          def_percent = calculated_def_percent(),
          attacker_level = input$attacker_level
        ),
        show.legend = TRUE # Show legend for this layer
      ) +
      geom_point(
        aes(
          x = data()$total_base_hp,
          y = calculate_ehp(data()$total_base_hp),
          color = "Base EHP",
        ),
        size = 2
      ) +
      labs(
        title = "EHP vs Base HP",
        x = "Total Base HP",
        y = "Effective HP"
      ) +
      scale_colour_manual(
        name = "Legend",
        values = c("Base EHP" = "black", "HP% Orb" = "darkgreen", "DEF% Orb" = "orange")
      ) +
      theme_pubclean()

    if ("def_orb" %in% input$orb_type) {
      ehp_curve <- ehp_curve +
        geom_function(
          aes(color = "DEF% Orb"),
          fun = calculate_ehp,
          args = list(
            hp_percent = calculated_hp_percent(),
            head_piece = data()$head_piece,
            def_percent = calculated_def_percent() + 54,
            attacker_level = input$attacker_level
          ),
          show.legend = TRUE
        ) +
        geom_point(
          aes(
            x = data()$total_base_hp,
            y = calculate_ehp(data()$total_base_hp,
              def_percent = calculated_def_percent() + 54
            ),
            color = "DEF% Orb",
          ),
          size = 2
        )
    }
    if ("hp_orb" %in% input$orb_type) {
      ehp_curve <- ehp_curve +
        geom_function(
          aes(color = "HP% Orb"),
          fun = calculate_ehp,
          args = list(
            hp_percent = calculated_hp_percent() + 43.2,
            head_piece = data()$head_piece,
            def_percent = calculated_def_percent(),
            attacker_level = input$attacker_level
          ),
          show.legend = TRUE # Show legend for this layer
        ) +
        geom_point(
          aes(
            x = data()$total_base_hp,
            y = calculate_ehp(data()$total_base_hp,
              hp_percent = calculated_hp_percent() + 43.2
            ),
            color = "HP% Orb",
          ),
          size = 2
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
    character_stats <- tibble(
      "Lvl 80 Character" = input$character_selection,
      "Base HP" = base_hp,
      "Base DEF" = selected_character_stats$DEF,
      "HP% Substats" = calculated_hp_percent(),
      "DEF% Substats" = calculated_def_percent(),
      "Total Base DEF" = data()$total_base_def,
      "Total Base DEF with Subs" = data()$total_def,
      "Total Base HP" = total_base_hp,
      "Total HP with Subs" = data()$total_hp
    ) %>%
      mutate("HP% Substats" = paste(.$"HP% Substats", "%", sep = "")) %>%
      mutate("DEF% Substats" = paste(.$"DEF% Substats", "%", sep = ""))
    return(character_stats)
  })

  output$ehp_stats <- renderTable({
    # Calculate EHP gain from HP and Def orbs
    ehp_gain_hp_orb <- calculate_ehp(
      total_base_hp = data()$total_base_hp,
      hp_percent = calculated_hp_percent() + 43.2,
      def_percent = calculated_def_percent(),
      head_piece = data()$head_piece,
      attacker_level = input$attacker_level
    )

    ehp_gain_def_orb <- calculate_ehp(
      total_base_hp = data()$total_base_hp,
      hp_percent = calculated_hp_percent(),
      def_percent = calculated_def_percent() + 54,
      head_piece = data()$head_piece,
      attacker_level = input$attacker_level
    )
    ehp_calculated <- calculate_ehp(
      total_base_hp = data()$total_base_hp,
      hp_percent = calculated_hp_percent(),
      def_percent = calculated_def_percent(),
      head_piece = data()$head_piece,
      attacker_level = input$attacker_level
    )
    ehp_stats <- tibble(
      "EHP without Orb" = ehp_calculated,
      "EHP (HP% Orb)" = ehp_gain_hp_orb,
      "EHP (DEF% Orb)" = ehp_gain_def_orb
    )
    return(ehp_stats)
  })


  output$lc_stats <- renderTable({
    lc_data <- light_cone_data[light_cone_data$lc_name == input$lc_selection, ]
    lc_stats <- tibble(
      "LC Name" = lc_data$lc_name,
      "LC Base HP" = lc_data$HP,
      "LC Base DEF" = lc_data$DEF
    )
    return(lc_stats)
  })

  # ATK Calculator ---------------------------------------------------------


  # Filter light cones for ATK calculator
  filtered_atk_light_cones <- reactive({
    selected_character <- input$atk_character_selection
    character_path <- character_data$character_path[character_data$character_name == selected_character]
    atk_lc_data <- light_cone_data[light_cone_data$lc_path == character_path, ]

    # Return the names of the filtered light cones
    atk_lc_data$lc_name
  })

  # Update the choices in the light_cone dropdown based on the reactive expression
  observe({
    choices <- filtered_atk_light_cones()
    updateSelectInput(session, "atk_lc_selection", choices = choices)
  })

  calculate_average_crit <- function(crit_rate, crit_damage) {
    crit_rate <-
      pmax(0, pmin(100, crit_rate)) # Clamp crit_rate between 0% and 100%
    AverageCrit <- 1 + (crit_rate / 100) * (crit_damage / 100)
    return(AverageCrit)
  }

  # Define a function to calculate damage based on factors
  calculate_damage <-
    function(base_atk,
             skill_multiplier = 100,
             atk_percent = calculated_atk_percent_base(),
             flat_atk = calculated_flat_atk_base(),
             crit_rate = calculated_crit_rate_base(),
             crit_damage = calculated_crit_damage_base(),
             dmg_multiplier = calculated_dmg_multiplier_base(),
             hp_scaling = FALSE,
             def_scaling = FALSE) {
      if (hp_scaling) { # Check if HP Scaling is enabled
        max_hp <- data()$total_base_hp
        base_dmg <- max_hp * skill_multiplier / 100
      } else {
        total_atk <- base_atk * (1 + atk_percent / 100) + flat_atk
        base_dmg <- total_atk * skill_multiplier / 100
      }
      total_atk <- base_atk * (1 + atk_percent / 100) + flat_atk
      base_dmg <- total_atk * skill_multiplier / 100
      dmg_multiplier <- 1 + dmg_multiplier / 100
      average_crit <- calculate_average_crit(crit_rate, crit_damage)

      # Calculate damage using the formula
      damage <-
        base_dmg * average_crit * dmg_multiplier

      return(damage)
    }
  ## Calculate data for ATK --------------------------------------------------------

  atk_data <- reactive({
    # Calculate relative damage increase as a percentage
    atk_character_data <- character_data[character_data$character_name == input$atk_character_selection, ]
    atk_lc_data <- light_cone_data[light_cone_data$lc_name == input$atk_lc_selection, ]

    atk_data <- tibble(
      base_atk = atk_character_data$ATK + atk_lc_data$ATK,
      total_atk_fix = base_atk * (1 + calculated_atk_percent_base() / 100) + calculated_flat_atk_base(),
      base_damage_fixed = calculate_damage(
        base_atk = base_atk,
        skill_multiplier = input$skill_multiplier,
        atk_percent = calculated_atk_percent_base(),
        flat_atk = calculated_flat_atk_base(),
        crit_rate = calculated_crit_rate_base(),
        crit_damage = calculated_crit_damage_base(),
        dmg_multiplier = calculated_dmg_multiplier_base(),
        hp_scaling = input$hp_scaling,
        def_scaling = input$def_scaling
      ),
      total_atk = base_atk * (1 + (calculated_atk_percent() + calculated_atk_percent_base()) / 100) + calculated_flat_atk_base() + calculated_flat_atk(),
      skill_multiplier = input$skill_multiplier,
      base_damage_total =
        calculate_damage(
          base_atk = base_atk,
          skill_multiplier = input$skill_multiplier,
          atk_percent = calculated_atk_percent_base() + calculated_atk_percent(),
          flat_atk = calculated_flat_atk_base() + calculated_flat_atk(),
          crit_rate = calculated_crit_rate_base() + calculated_crit_rate(),
          crit_damage = calculated_crit_damage_base() + calculated_crit_damage(),
          dmg_multiplier = calculated_dmg_multiplier_base() + calculated_dmg_multiplier(),
          hp_scaling = input$hp_scaling,
          def_scaling = input$def_scaling
        )
    )
    return(atk_data)
  })

  relative_damage_increase <- function(..., data = atk_data()) {
    ((calculate_damage(...) - data$base_damage_fixed) / data$base_damage_fixed) * 100
  }

  ## Output ATK Plot ---------------------------------------------------------


  output$atk_plot <- renderPlot({
    # Create a ggplot base with updated Y-axis scale and labels
    base_plot <- ggplot() +
      xlim(input$x_limit_atk) +
      labs(
        title = "Relative Damage Increase vs Base Damage",
        x = "Base Damage",
        y = "Relative Damage Increase (%)"
      ) +
      theme_pubclean()


    # Define a function to add geom_function and geom_point layers
    geom_function_base <- function(skill_multiplier = input$skill_multiplier,
                                   atk_percent = calculated_atk_percent_base(),
                                   flat_atk = calculated_flat_atk_base(),
                                   crit_rate = calculated_crit_rate_base(),
                                   crit_damage = calculated_crit_damage_base(),
                                   dmg_multiplier = calculated_dmg_multiplier_base(),
                                   data = atk_data(),
                                   legend_label = "Base") {
      geom_func <- geom_function(
        fun = relative_damage_increase,
        args = list(
          skill_multiplier = skill_multiplier,
          atk_percent = atk_percent,
          flat_atk = flat_atk,
          crit_rate = crit_rate,
          crit_damage = crit_damage,
          dmg_multiplier = dmg_multiplier
        ),
        aes(color = {{ legend_label }}),
        show.legend = TRUE,
      )
      geom_pt <- geom_point(
        data = data,
        aes(
          x = base_atk, y = relative_damage_increase(
            base_atk = base_atk,
            skill_multiplier = skill_multiplier,
            atk_percent = atk_percent,
            flat_atk = flat_atk,
            crit_rate = crit_rate,
            crit_damage = crit_damage,
            dmg_multiplier = dmg_multiplier
          ),
          colour = {{ legend_label }}
        ),
        size = 2,
        show.legend = TRUE
      )

      return(list(geom_func, geom_pt))
    }


    ## Store the geom_function plots ----
    geom_base <- geom_function_base(
      data = atk_data(),
      legend_label = "Base"
    )
    geom_atk <- geom_function_base(
      atk_percent = calculated_atk_percent() + calculated_atk_percent_base(),
      flat_atk = calculated_flat_atk() + calculated_flat_atk_base(),
      legend_label = "ATK"
    )
    geom_crit_rate <- geom_function_base(
      crit_rate = calculated_crit_rate() + calculated_crit_rate_base(),
      crit_damage = calculated_crit_damage_base(),
      legend_label = "CR"
    )
    geom_crit_damage <- geom_function_base(
      crit_rate = calculated_crit_rate_base(),
      crit_damage = calculated_crit_damage() + calculated_crit_damage_base(),
      legend_label = "CD"
    )
    geom_dmg <- geom_function_base(
      dmg_multiplier = calculated_dmg_multiplier() + calculated_dmg_multiplier_base(),
      legend_label = "DMG"
    )

    # Create a list to store the layers
    layers <- list()
    # Add the base geom_function
    layers <- append(layers, geom_base)

    # Add the geom_function layers to the base plot
    if ("atk_toggle" %in% input$line) layers <- append(layers, geom_atk)
    if ("crit_rate_toggle" %in% input$line) layers <- append(layers, geom_crit_rate)
    if ("crit_damage_toggle" %in% input$line) layers <- append(layers, geom_crit_damage)
    if ("dmg_toggle" %in% input$line) layers <- append(layers, geom_dmg)





    final_plot <- base_plot +
      scale_color_manual(
        name = "Legend",
        values = c(
          "Base" = "black",
          "ATK" = "red",
          "CR" = "orange",
          "CD" = "purple",
          "DMG" = "darkgreen"
        )
      ) + layers

    # Display the final plot
    print(final_plot)
  })

  ## Render Table ----------------------------------------------------------------


  output$character_stats_atk <- renderTable({
    character_stats_atk <- tibble(
      "Base ATK" = atk_data()$base_atk,
      "Total ATK (Fixed)" = atk_data()$base_atk *
        (1 + calculated_atk_percent_base() / 100) + calculated_flat_atk_base(),
      "Base damage (Fixed)" = atk_data()$base_damage_fixed,
      "Total ATK" = atk_data()$total_atk,
      "Base damage Total" = atk_data()$base_damage_total,
      "Relative Damage Increase (ATK)" = relative_damage_increase(
        base_atk = atk_data()$base_atk,
        atk_percent = calculated_atk_percent() + calculated_atk_percent_base(),
        flat_atk = calculated_flat_atk() + calculated_flat_atk_base()
      ),
      "Relative Damage Increase (CR)" = relative_damage_increase(
        base_atk = atk_data()$base_atk,
        crit_rate = calculated_crit_rate() + calculated_crit_rate_base(),
      ),
      "Relative Damage Increase (CD)" = relative_damage_increase(
        base_atk = atk_data()$base_atk,
        crit_damage = calculated_crit_damage() + calculated_crit_damage_base(),
      ),
      "Relative Damage Increase (DMG)" = relative_damage_increase(
        base_atk = atk_data()$base_atk,
        dmg_multiplier = calculated_dmg_multiplier() + calculated_dmg_multiplier_base(),
      )
    )
    return(character_stats_atk)
  })
  output$lc_stats <- renderTable({
    atk_lc_data <- light_cone_data[light_cone_data$lc_name == input$atk_lc_selection, ]
    lc_stats <- tibble(
      "LC Name" = atk_lc_data$lc_name,
      "LC Base HP" = atk_lc_data$HP,
      "LC Base DEF" = atk_lc_data$DEF
    )
  })
}

shinyApp(ui, server)
