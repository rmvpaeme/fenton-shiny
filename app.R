library(tidyverse)
library(shiny)
library(bslib)
library(shiny.i18n)
library(shinycssloaders)
library(DT)

# ---------------------------------------------------------------------------
# Internationalisation (NL default, EN). All UI/plot strings live in
# www/translations.json. usei18n() swaps static UI text client-side so app
# state (uploaded file, active tab) is preserved when switching language.
# ---------------------------------------------------------------------------
i18n <- Translator$new(translation_json_path = "www/translations.json")
i18n$set_translation_language("nl")

# ---------------------------------------------------------------------------
# Nord Light theme (palette ported from the rshiny_claude reference app).
# Calm, muted: frost blues for curves/chrome, aurora red for measurements.
# ---------------------------------------------------------------------------
NORD_BG    <- "#FFFFFF"
NORD_FG    <- "#2E3440"
NORD_BLUE  <- "#5E81AC" # frost3 - primary / curves
NORD_BLUE2 <- "#81A1C1" # frost2 - secondary / spinner
NORD_RED   <- "#BF616A" # aurora red - measured points
NORD_GRID  <- "#E5E9F0"

app_font <- font_collection("UGent Panno Text", "Arial", "sans-serif")
app_theme <- bs_theme(
  version      = 5,
  bg           = NORD_BG,
  fg           = NORD_FG,
  primary      = NORD_BLUE,
  secondary    = NORD_BLUE2,
  base_font    = app_font,
  heading_font = app_font
)

# ---------------------------------------------------------------------------
# Reference data: load the static Fenton L/M/S + percentile curves once at
# startup instead of on every reactive invalidation.
# ---------------------------------------------------------------------------
read_reference <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(PML = Time) %>%
    select(-Time)
}
REFERENCE <- list(
  M = read_reference("./data/boys_all.csv"),
  F = read_reference("./data/girls_all.csv")
)

# ---------------------------------------------------------------------------
# Safe postmenstrual-age parser. Replaces the previous
# `eval(parse(text = x))`, which evaluated arbitrary user-supplied GET
# strings. Reproduces the documented formats exactly:
#   "23+1/7" -> 23 + 1/7,  "29.14" -> 29.14,  "30" -> 30,  "NA"/"" -> NA
# ---------------------------------------------------------------------------
parse_fraction <- function(s) {
  nd <- strsplit(s, "/", fixed = TRUE)[[1]]
  if (length(nd) == 2) as.numeric(nd[1]) / as.numeric(nd[2]) else as.numeric(s)
}
parse_pml_one <- function(s) {
  s <- trimws(s)
  if (is.na(s) || s == "" || toupper(s) == "NA") return(NA_real_)
  if (grepl("+", s, fixed = TRUE)) {
    parts <- strsplit(s, "+", fixed = TRUE)[[1]]
    return(as.numeric(parts[1]) + parse_fraction(parts[2]))
  }
  if (grepl("/", s, fixed = TRUE)) return(parse_fraction(s))
  suppressWarnings(as.numeric(s))
}
parse_pml <- function(x) vapply(as.character(x), parse_pml_one, numeric(1), USE.NAMES = FALSE)

split_values <- function(x) suppressWarnings(as.numeric(unlist(strsplit(as.character(x), ",", fixed = TRUE))))

# ---------------------------------------------------------------------------
# Single plotting helper, replaces three near-identical renderPlot blocks.
# `tr` is the active (reactive) translator; `y_accuracy` forces decimals on
# the y-axis labels (used for head circumference -> XX.X).
# ---------------------------------------------------------------------------
growth_plot <- function(df, measure_type, sex_word, y_breaks, y_name, subtitle, tr,
                        y_limits = NULL, y_accuracy = NULL) {
  curves   <- df %>% filter(type == measure_type, annotation != "measure", value > 0)
  measured <- df %>% filter(type == measure_type, annotation == "measure", value > 0)

  y_scale <- if (is.null(y_accuracy)) {
    scale_y_continuous(breaks = y_breaks, limits = y_limits, name = y_name)
  } else {
    scale_y_continuous(breaks = y_breaks, limits = y_limits, name = y_name,
                       labels = scales::label_number(accuracy = y_accuracy))
  }

  ggplot(curves, aes(x = PML, y = as.numeric(value), linetype = annotation)) +
    geom_line(color = NORD_BLUE) +
    geom_point(
      data = measured,
      aes(x = PML, y = as.numeric(value), color = "measure"),
      inherit.aes = FALSE, size = 3
    ) +
    scale_color_manual(values = c(measure = NORD_RED),
                       labels = tr$t("meting"), name = NULL) +
    theme_bw(base_size = 18) +
    labs(subtitle = paste0(subtitle, sex_word)) +
    theme(
      text            = element_text(family = "Arial"),
      panel.grid      = element_line(color = NORD_GRID),
      legend.position = "bottom",
      legend.box      = "horizontal",
      legend.title    = element_blank(),
      plot.subtitle   = element_text(color = NORD_BLUE, face = "bold")
    ) +
    scale_x_continuous(breaks = seq(22, 50, 2), name = tr$t("zwangerschapsduur (weken)")) +
    y_scale
}

# ===========================================================================
# UI
# ===========================================================================
ui <- fluidPage(
  theme = app_theme,
  usei18n(i18n),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "nord.css"),
    tags$title("Fenton - NICU groeicurves")
  ),

  # Compact language toggle, fixed in the top-right corner. The label shows
  # the language you switch TO (NL -> shows "EN", EN -> shows "NL").
  div(class = "lang-switch", actionButton("toggle_lang", "EN")),

  div(
    class = "app-header",
    h1(i18n$t("NICU-groeicurves")),
    p(class = "app-subtitle", i18n$t("Fenton 2013 groeicurves voor premature baby's"))
  ),

  sidebarLayout(
    sidebarPanel(
      # Input id and values ("no"/"yes") are kept identical to the previous
      # selectInput so the ?advanced=... URL param stays backwards compatible.
      radioButtons(
        "advanced", i18n$t("Invoermethode"),
        choiceNames  = list(i18n$t("Excel-upload"),
                            i18n$t("Handmatige invoer (geavanceerd)")),
        choiceValues = list("no", "yes"),
        selected = "no"
      ),
      conditionalPanel(
        condition = "input.advanced == 'no'",
        fileInput(
          "file_excel",
          tagList(
            i18n$t("Upload een Excel-bestand met eigen gegevens."),
            a(i18n$t("Klik hier"),
              href = "https://github.com/rmvpaeme/fenton-shiny/raw/main/example_excel.xlsx"),
            i18n$t(" om het Excel-sjabloon te downloaden.")
          ),
          accept = c(".xlsx")
        )
      ),
      conditionalPanel(
        condition = "input.advanced == 'yes'",
        textInput("sex_GET", i18n$t("Geslacht (M of F)"), value = "M"),
        textInput("PML_GET", i18n$t("Postmenstruele leeftijd (weken, formaat 23+1/7) in CSV"), value = NA),
        textInput("HC_GET", i18n$t("Schedelomtrek (in cm) in CSV"), value = NA),
        textInput("weight_GET", i18n$t("Gewicht (in gram) in CSV"), value = NA),
        textInput("length_GET", i18n$t("Lengte (in cm) in CSV"), value = NA)
      )
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          i18n$t("Gewicht"),
          withSpinner(plotOutput("weight", height = "800px", width = "90%"), color = NORD_BLUE2),
          hr(),
          h4("Disclaimer"),
          p(
            class = "disclaimer",
            i18n$t("Deze tool is niet uitgebreid getest; voorzichtigheid is geboden. De code is beschikbaar op "),
            a("https://github.com/rmvpaeme/fenton-shiny",
              href = "https://github.com/rmvpaeme/fenton-shiny"),
            i18n$t(". Bron: Fenton TR, Kim JH. A systematic review and meta-analysis to revise the Fenton growth chart for preterm infants. BMC Pediatr. 2013;13:59. Een uitgebreidere toepassing om groeiwaarden manueel in te voeren is beschikbaar op "),
            a("https://peditools.org/peditools_universal/",
              href = "https://peditools.org/peditools_universal/")
          )
        ),
        tabPanel(i18n$t("Lengte"),
                 withSpinner(plotOutput("L", height = "800px", width = "90%"), color = NORD_BLUE2)),
        tabPanel(i18n$t("Schedelomtrek"),
                 withSpinner(plotOutput("HC", height = "800px", width = "90%"), color = NORD_BLUE2)),
        tabPanel(i18n$t("Percentieltabel"), DT::dataTableOutput("table")),
        tabPanel(
          i18n$t("Gebruik"),
          p(
            i18n$t("Waarden kunnen ook via een GET-verzoek worden ingevoerd. Voorbeeld: "),
            a("http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA",
              href = "http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA")
          )
        )
      )
    )
  )
)

# ===========================================================================
# Server
# ===========================================================================
server <- function(input, output, session) {

  # Current language (NL default) toggled by the corner button.
  lang <- reactiveVal("nl")
  observeEvent(input$toggle_lang, {
    lang(if (lang() == "nl") "en" else "nl")
  })

  # Keep static UI text and the toggle's own label in sync with the language.
  observeEvent(lang(), {
    update_lang(lang())
    updateActionButton(session, "toggle_lang",
                       label = if (lang() == "nl") "EN" else "NL")
  })

  # Reactive translator for server-rendered output (plots, table button).
  tr <- reactive({
    i18n$set_translation_language(lang())
    i18n
  })

  newData <- reactive({
    inFile_excel <- input$file_excel

    if (is.null(inFile_excel)) {
      # ---- URL GET / manual-entry path -------------------------------------
      # Backwards-compatible query string handling.
      query <- parseQueryString(session$clientData$url_search)
      text_params <- c("naam", "sex_GET", "HC_GET", "length_GET", "weight_GET", "PML_GET")
      for (p in text_params) {
        if (!is.null(query[[p]])) updateTextInput(session, p, value = query[[p]])
      }
      if (!is.null(query[["advanced"]])) {
        updateRadioButtons(session, "advanced", selected = query[["advanced"]])
      }

      pml <- parse_pml(unlist(strsplit(as.character(input$PML_GET), ",", fixed = TRUE)))

      build_measure <- function(values, type) {
        tibble(value = split_values(values), PML = pml, annotation = "measure", type = type)
      }
      df_measure <- bind_rows(
        build_measure(input$weight_GET, "weight"),
        build_measure(input$length_GET, "length"),
        build_measure(input$HC_GET, "HC")
      )

      sex_code <- as.character(input$sex_GET)
      df <- bind_rows(REFERENCE[[sex_code]], df_measure)

    } else {
      # ---- Excel-upload path ----------------------------------------------
      # Read the workbook once: cols = date, PML, weight, length, HC.
      raw <- readxl::read_excel(
        inFile_excel$datapath, skip = 9,
        col_names = c("date", "PML", "weight", "length", "HC")
      )
      df_measure <- raw %>%
        select(PML, weight, length, HC) %>%
        pivot_longer(c(weight, length, HC), names_to = "type", values_to = "value") %>%
        mutate(value = as.double(value), annotation = "measure")

      sex_code <- as.character(readxl::read_excel(inFile_excel$datapath, range = "B5", col_names = FALSE)[[1]])
      df <- bind_rows(REFERENCE[[sex_code]], df_measure)
    }

    list(df = df, sex_code = sex_code)
  })

  sex_word <- reactive({
    tr()$t(if (identical(newData()$sex_code, "M")) "jongens" else "meisjes")
  })

  output$table <- DT::renderDataTable({
    LMS2p <- function(L, M, S, X) pnorm((((X / M) ^ L) - 1) / (L * S))
    df <- newData()$df

    df_spread_measure <- df %>%
      filter(annotation == "measure") %>%
      spread(key = annotation, value = value) %>%
      select(-c(L, M, S))
    df_spread_measure$PML_orig <- df_spread_measure$PML
    df_spread_measure$PML <- round(df_spread_measure$PML, 2)
    df_spread_measure$annotation <- "measure"

    df_spread_LMS <- df %>%
      filter(annotation != "measure") %>%
      spread(key = annotation, value = value)
    df_spread_LMS$PML <- round(df_spread_LMS$PML, 2)

    df_spread_all <- left_join(df_spread_LMS, df_spread_measure)
    df_spread_all$P_measure <- LMS2p(df_spread_all$L, df_spread_all$M, df_spread_all$S, X = df_spread_all$measure)
    df_spread_all <- df_spread_all %>%
      filter(annotation == "measure") %>%
      transmute(measurement = type, GA = round(PML_orig, 2),
                Percentile = round(P_measure * 100, 2), value = measure) %>%
      filter(value > 0) %>%
      arrange(measurement, GA) %>%
      # Head circumference is shown with one decimal everywhere (XX.X).
      mutate(value = case_when(
        measurement == "HC"     ~ sprintf("%.1f", value),
        measurement == "weight" ~ sprintf("%.0f", value),
        TRUE                    ~ sprintf("%.1f", value)
      ))

    DT::datatable(
      df_spread_all,
      extensions = "Buttons",
      options = list(
        paging = TRUE, searching = TRUE, fixedColumns = TRUE, autoWidth = TRUE,
        ordering = TRUE, dom = "frtBip",
        buttons = list(list(extend = "excel", text = tr()$t("Bewaar als Excel-bestand")))
      ),
      rownames = FALSE,
      class = "display"
    )
  })

  output$weight <- renderPlot({
    growth_plot(
      newData()$df, "weight", sex_word(),
      y_breaks = seq(0, 7200, 400), y_limits = c(0, 7200),
      y_name = tr()$t("gram"),
      subtitle = tr()$t("Fenton-groeicurve, gewicht voor "), tr = tr()
    )
  })

  output$L <- renderPlot({
    growth_plot(
      newData()$df, "length", sex_word(),
      y_breaks = seq(18, 70, 4),
      y_name = tr()$t("centimeter"),
      subtitle = tr()$t("Fenton-groeicurve, lengte voor "), tr = tr()
    )
  })

  output$HC <- renderPlot({
    growth_plot(
      newData()$df, "HC", sex_word(),
      y_breaks = seq(18, 60, 2),
      y_name = tr()$t("centimeter"),
      subtitle = tr()$t("Fenton-groeicurve, schedelomtrek voor "), tr = tr(),
      y_accuracy = 0.1
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
