# Copyright 2024 Matěj Kolouch Grabovský
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(bslib)
library(patchwork)
library(shiny)
library(tidyverse)
library(tsibble)

options(
  lubridate.week.start = 1,
  readr.show_col_types = FALSE
)

theme_set(
  hrbrthemes::theme_ipsum_rc(
    axis_title_size = 12,
    base_size = 12,
    grid = "XY"
  )
)

df_installed <- tribble(
  ~ Source, ~ Year, ~ InstalledMW,
  "Onshore", 2015, 278,
  "Onshore", 2016, 282,
  "Onshore", 2017, 308,
  "Onshore", 2018, 317,
  "Onshore", 2019, 339,
  "Onshore", 2020, 339,
  "Onshore", 2021, 339,
  "Onshore", 2022, 339,
  "Onshore", 2023, 343,
  "Solar", 2015, 2078,
  "Solar", 2016, 2076,
  "Solar", 2017, 2077,
  "Solar", 2018, 2062,
  "Solar", 2019, 2069,
  "Solar", 2020, 2080,
  "Solar", 2021, 2088,
  "Solar", 2022, 2082,
  "Solar", 2023, 2800  # Approximately.
)

nuclear_installed_mw <- 4047

yhour <- \(dt) hour(dt) + (yday(dt) - 1) * 24

cz_entsoe <- arrow::read_parquet("entsoe-cz.parquet") |>
  select(Date, Demand = Load, Nuclear, Onshore, Solar) |>
  mutate(
    Year = year(Date),
    # Month = month(Date),
    # Day = day(Date),
    # DayOfYear = yday(Date),
    # Hour = hour(Date),
    # HourOfYear = yhour(Date)
  ) |>
  left_join(
    pivot_wider(
      df_installed,
      names_from = Source,
      names_prefix = "Installed",
      values_from = InstalledMW
    ),
    join_by(Year)
  ) |>
  mutate(
    Nuclear = Nuclear / nuclear_installed_mw,
    Onshore = Onshore / InstalledOnshore,
    Solar = Solar / InstalledSolar
  ) |>
  select(!starts_with("Installed"))

ui <- page_sidebar(
  title = "Explorace variability OZE",
  sidebar = sidebar(
    sliderInput(
      inputId = "load_increase_pct",
      label = "Nárůst spotřeby:",
      min = 0,
      max = 100,
      step = 10,
      pre = "+",
      post = " %",
      value = 40
    ),
    sliderInput(
      inputId = "pv_installed_gw",
      label = "Inst. výkon FVE:",
      min = 2,
      max = 40,
      step = 1,
      post = " GW",
      value = 25
    ),
    sliderInput(
      inputId = "wind_installed_gw",
      label = "Inst. výkon VtE:",
      min = 0.3,
      max = 20,
      step = 1,
      post = " GW",
      value = 10
    ),
    sliderInput(
      inputId = "nuclear_installed_gw",
      label = "Inst. výkon jádra:",
      min = 0,
      max = 8,
      step = .1,
      post = " GW",
      value = 4.1
    ),
    sliderInput(
      inputId = "dispatchable_installed_gw",
      label = "Inst. výkon řiditelných zdrojů:",
      min = 0,
      max = 20,
      step = 1,
      post = " GW",
      value = 4
    )
  ),
  fluidRow(
    h2("Analýza dostatečnosti výroby"),
    column(7, plotOutput(outputId = "residual_histograms", height = "800px")),
    column(5, 
      fluidRow(
        column(
          12,
          selectInput(
            inputId = "residual_window_size",
            label = "Velikost okna:",
            choice = list(
              "1 h" = 1,
              "6 h" = 6,
              "12 h" = 12,
              "24 h" = 24,
              "48 h" = 48,
              "72 h" = 72
            ),
            selected = 1
          ),
          h4("Shrnutí"),
          tags$p(
            "Průměrné roční hodnoty spotřeby, výroby a poměr výroby z větru a slunce:"
          ),
          tableOutput(outputId = "summaries_table")
        )
      )
    )
  ),
  h2("Analýza patternů výroby"),
  fluidRow(
    column(8, plotOutput(outputId = "cf_plots", height = "450px")),
    column(
      4,
      fluidRow(
        column(
          12,
          sliderInput(
            inputId = "cf_window_size",
            label = "Velikost okna:",
            min = 1,
            max = 14,
            post = " d",
            value = 1
          ),
          sliderInput(
            inputId = "cf_solar_weight",
            label = "Poměr inst. výkonu ve fotovoltaice:",
            min = 0,
            max = 1,
            step = .05,
            value = .5
          ),
          helpText(
            "Poměr 0 znamená 100 % instaloveného výkonu ve větru,",
            "poměr 1 znamená 100 % ve fotovoltaice. Poměr 0,5 odpovídá",
            "50 % větru a 50 % fotovoltaiky."
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  df_scaled <- reactive({
    load_scaling_factor <- 1 + input$load_increase_pct / 100
    nuclear_mw <- 1000 * input$nuclear_installed_gw
    onshore_mw <- 1000 * input$wind_installed_gw
    solar_mw <- 1000 * input$pv_installed_gw

    cz_entsoe |>
      mutate(
        Demand = Demand * load_scaling_factor,
        Nuclear = Nuclear * nuclear_mw,
        Onshore = Onshore * onshore_mw,
        Solar = Solar * solar_mw,
        Residual = Demand - Nuclear - Solar - Onshore,
        Dispatchable = 0,
        Shortage = pmax(0, Residual - Dispatchable)
      )
  })

  df_windowed <- reactive({
    num_years <- length(unique(df_scaled()$Year))
    window_size <- as.integer(input$residual_window_size)
    dispatchable_installed_mw <- 1000 * input$dispatchable_installed_gw

    # Sum in each window tile.
    df_scaled_windowed <- df_scaled() |>
      as_tsibble(index = Date) |>
      tile_tsibble(.size = window_size) |>
      as_tibble() |>
      summarise(
        Date = first(Date),
        across(
          Demand | Dispatchable | Nuclear | Onshore | Residual | Shortage | Solar,
          sum
        ),
        .by = .id
      ) |>
      mutate(
        Year = year(Date),
        Month = month(Date)
      )

    if (dispatchable_installed_mw > 0) {
      df_aggregated <- df_scaled_windowed
      # Dispatch available capacity in hours of shortage, but only if there's
      # residual demand in the window.
      df_scaled_windowed <- df_scaled() |>
        as_tsibble(index = Date) |>
        tile_tsibble(.size = window_size) |>
        as_tibble() |>
        left_join(
          select(df_aggregated, .id, ResidualAgg = Residual),
          join_by(.id)
        ) |>
        # Calculate dispatch of (virtual) dispatchable sources in each hour.
        mutate(
          Dispatchable = if_else(
            ResidualAgg <= 0,
            # No dispatch as there's no residual demand in the window -- we assume
            # any shortages in this tile are balanced by excess generation in some of
            # the hours (assuming perfect storage).
            0,
            # Dispatch to cover residual demand up to available capacity.
            pmin(Residual, dispatchable_installed_mw)
          )
        ) |>
        summarise(
          Date = first(Date),
          across(
            Demand | Dispatchable | Nuclear | Onshore | Residual | Solar,
            sum
          ),
          # Update shortage so that it matches following the aggregation.
          Shortage = sum(pmax(0, Residual - Dispatchable)),
          .by = .id
        ) |>
        mutate(
          Year = year(Date),
          Month = month(Date)
        )
    }

    df_scaled_windowed |>
      mutate(Excess = pmax(0, -Residual))
  })

  output$summaries_table <- renderTable({
    dispatchable_installed_gw <- input$dispatchable_installed_gw

    df_windowed() |>
      summarise(
        across(
          Demand | Nuclear | Onshore | Solar | Dispatchable | Excess | Shortage,
          ~ sum(.x) / 1e6
        ),
        `Dispatchable CF` = Dispatchable / (dispatchable_installed_gw * 8.76),
        .by = Year
      ) |>
      summarise(across(!Year, mean)) |>
      mutate(`Onshore:Solar` = round(Onshore / Solar, 1)) |>
      pivot_longer(
        everything(),
        names_to = "Veličina",
        values_to = "Hodnota"
      )
  })

  output$residual_histograms <- renderPlot(
    {
      num_years <- length(unique(df_scaled()$Year))
      window_size <- as.integer(input$residual_window_size)

      df_windowed() |>
        mutate(
          Month = factor(Month, labels = month.abb),
          Category = case_when(
            Residual <= 0 ~ "Excess",
            # NOTE: Shortage should never be negative, but just to be on the safe
            # side...
            Shortage <= 0 ~ "Covered",
            .default = "Shortage"
          ) |> factor(levels = c("Shortage", "Covered", "Excess")),
        ) |>
        ggplot(aes(-Residual / 1000)) +
        geom_vline(xintercept = 0, colour = "grey") +
        geom_histogram(
          aes(y = after_stat(count) / num_years, fill = Category),
          boundary = 0,
          bins = 40
        ) +
        facet_wrap(vars(Month), nrow = 4) +
        scale_fill_manual(
          "",
          values = c(
            Covered = "lightblue",
            Excess = "grey60",
            Shortage = "tomato3"
          ),
          labels = c(
            Covered = "Spotřeba pokryta\nřiditelnými zdroji",
            Excess = "Nadvýroba z neflexibilních\nzdrojů",
            Shortage = "Spotřeba nepokryta zcela"
          )
        ) +
        labs(
          x = "Přebytky z výroby J+FVE+VtE (GWh)",
          y = str_glue(
            "Průměrný počet ",
            if (window_size > 1) window_size else "",
            "hodin v měsíci"
          )
        ) +
        theme(legend.position = "bottom")
    },
    res = 90
  )

  output$cf_plots <- renderPlot(
    {
      solar_weight <- input$cf_solar_weight
      window_size <- input$cf_window_size

      df_cfs <- cz_entsoe |>
        rename(Datetime = Date) |>
        mutate(
          CombinedCf = (1 - solar_weight) * Onshore + solar_weight * Solar
        ) |>
        as_tsibble(index = Datetime) |>
        index_by(Date = as_date(Datetime)) |>
        summarise(across(CombinedCf | Onshore | Solar, mean))

      if (window_size > 1) {
        df_cfs <- df_cfs |>
          tile_tsibble(.size = window_size) |>
          as_tibble() |>
          summarise(
            Date = first(Date),
            across(CombinedCf | Onshore | Solar, mean),
            .by = .id
          ) |>
          as_tsibble(index = Date)
      }

      p_combined <- df_cfs |>
        ggplot() +
        geom_tile(
          aes(yday(Date), year(Date), fill = CombinedCf),
          width = window_size
        ) +
        scale_fill_viridis_c("Koef.\nvyužití", option = "inferno") +
        scale_x_continuous("Den v roce", expand = expansion(mult = .01)) +
        scale_y_reverse() +
        expand_limits(fill = 0) +
        ggtitle("Využití kombinace FVE + VtE") +
        theme(
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank()
        )

      p_combined
    },
    res = 90
  )
}

shinyApp(ui = ui, server = server)

