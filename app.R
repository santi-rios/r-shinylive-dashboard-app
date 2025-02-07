library(shiny)
library(bslib)
library(tidyverse)
library(plotly)

######### Data Preparation #########
read_figuras <- function(directory) {
  files <- list.files(directory, pattern = "^Figure1.*\\.tsv$", full.names = TRUE)
  
  data_list <- lapply(files, function(file) {
    read_tsv(file) %>%
      pivot_longer(cols = -Country, names_to = "Year", values_to = "Value") %>%
      mutate(Year = as.numeric(Year),
             source = tools::file_path_sans_ext(basename(file)))
  })
  
  bind_rows(data_list)
}

df <- read_figuras("data/")

######### Animation Function #########
accumulate_by <- function(dat, var) {
  var <- rlang::ensym(var)
  lvls <- sort(unique(dat[[var]]))
  dats <- lapply(lvls, function(x) {
    dat %>% filter(!!var <= x) %>% mutate(frame = x)
  })
  bind_rows(dats)
}

######### APP #########
ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  div(
    class = "navbar navbar-static-top primary bg-primary",
    div("China's rise in the chemical space and the decline of US influence", class = "container-fluid")
  ),
  br(),
  layout_columns(
    col_widths = c(3, 9),  # 25% controls, 75% plot
    card(
      card_header("Controls"),
      card_body(
        selectInput("facet", "Select  figure:",
                    choices = unique(df$source),
                    selected = "Figure1a",
                    width = "100%"),
        selectInput("countries", "Choose Countries or delete with delete key in keyboard:",
                    choices = NULL,
                    selected = NULL,
                    multiple = TRUE,
                    width = "100%"),
        uiOutput("figure_description")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Figure 1"),
      card_body(
        plotlyOutput("emissionsPlot", height = "75vh")
      )
    )
  ),
  br(),
    div(
        class = "navbar navbar-static-bottom bg-light",
        div(
        class = "container-fluid",
        "Data taken from the article Bermudez-Montana, Garcia-Chung, et al, 2025: ",
        a("https://doi.org/10.26434/chemrxiv-2025-d2zc8", href = "https://chemrxiv.org/engage/chemrxiv/article-details/67920ada6dde43c908f688f6")
        )
    )
)

server <- function(input, output) {
  # Dynamic country selection based on dataset
  observeEvent(input$facet, {
    countries <- df %>% 
      dplyr::filter(source == input$facet) %>% 
      dplyr::pull(Country) %>% 
      unique()
    
    updateSelectInput(inputId = "countries", 
                      choices = countries,
                      selected = head(countries, 4))
  })

  # Figure description text
  output$figure_description <- renderUI({
    req(input$facet)
    desc_text <- case_when(
      input$facet == "Figure1a" ~ "Countrywise expansion of the chemical space, Country participation in the growth of the CS",
      input$facet == "Figure1b" ~ "Eight most relevant international collaborations in the CS",
      input$facet == "Figure1c" ~ "Percentage of new substances with participation of country, is either China or the US, resulting from China-US collaboration",
      input$facet == "Figure1d" ~ "Percentage of the annual growth rate of the gross domestic product (GDP) per capita.6 Two important economic events are highlighted: the 2007/2008 Global Financial Crisis and the COVID pandemic (2020)",
      input$facet == "Figure1e" ~ "Number of researchers in research and development activities (SI)",
      TRUE ~ paste("Displaying:", input$facet)
    )
    div(class = "text-muted", style = "margin-bottom: 15px;", desc_text)
  })
  
  # Main plot
  output$emissionsPlot <- renderPlotly({
    req(input$countries, input$facet)
    
    filtered_data <- df %>% 
      filter(Country %in% input$countries, 
             source == input$facet) %>% 
      dplyr::arrange(Year)
    
    if (nrow(filtered_data) == 0) return(plotly_empty())
    
    animated_data <- accumulate_by(filtered_data, Year)
    
    # Create base plot
    fig <- animated_data %>%
      plot_ly(
        x = ~Year,
        y = ~Value,
        color = ~Country,
        frame = ~frame,
        type = 'scatter',
        mode = 'lines',
        line = list(simplify = FALSE, width = 2),
        hoverinfo = 'text',
        text = ~paste("Country:", Country, "<br>Year:", Year, "<br>Value:", round(Value, 2))
      )
    
    # Add country annotations at final frame
    final_data <- filtered_data %>% 
      group_by(Country) %>% 
      filter(Year == max(Year)) %>% 
      ungroup()
    
    for(i in 1:nrow(final_data)) {
      fig <- fig %>% add_annotations(
        x = final_data$Year[i],
        y = final_data$Value[i],
        text = final_data$Country[i],
        xref = "x",
        yref = "y",
        xanchor = 'left',
        yanchor = 'middle',
        showarrow = FALSE,
        # font = list(color = plotly:::toRGB(final_data$Country[i]), size = 12),
        xshift = 10
      )
    }
    
    # Layout configuration
    fig %>% layout(
      title = paste("Countrywise expansion of the chemical space -", input$facet),
      xaxis = list(title = "Year", range = c(1995, 2023)),
      yaxis = list(title = "Value"),
      hovermode = "x unified",
      legend = list(orientation = "h", y = -0.2),
      margin = list(r = 40)  # Add right margin for annotations
    ) %>%
    animation_opts(
      frame = 300, 
      transition = 100,
      redraw = FALSE,
      mode = "afterall"
    ) %>%
    animation_slider(
      currentvalue = list(font = list(color = "black"))
    ) %>%
    animation_button(
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
  })
}

shinyApp(ui, server)
