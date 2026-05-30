library(tidyverse)
library(shiny)
library(shinythemes)
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
    geom_line() +
    geom_point(
      data = measured,
      aes(x = PML, y = as.numeric(value), color = annotation),
      inherit.aes = FALSE, size = 3
    ) +
    theme_bw(base_size = 18) +
    labs(subtitle = paste0(subtitle, sex_word)) +
    theme(
      text            = element_text(size = 20),
      legend.position = "bottom",
      legend.box      = "horizontal",
      legend.title    = element_blank()
    ) +
    scale_x_continuous(breaks = seq(22, 50, 2), name = tr$t("zwangerschapsduur (weken)")) +
    y_scale
}

# ===========================================================================
# UI
# ===========================================================================
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  usei18n(i18n),
  tags$head(
    tags$style(HTML("
      .lang-switch { position: fixed; top: 14px; right: 18px; z-index: 1050; }
    ")),
    tags$title("Fenton - NICU groeicurves")
  ),

  # Compact language toggle, fixed in the top-right corner. The label shows
  # the language you switch TO (NL -> shows "EN", EN -> shows "NL").
  div(class = "lang-switch", actionButton("toggle_lang", "EN")),

  titlePanel(i18n$t("NICU-groeicurves")),

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
          withSpinner(plotOutput("weight", height = "800px", width = "90%")),
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
                 withSpinner(plotOutput("L", height = "800px", width = "90%"))),
        tabPanel(i18n$t("Schedelomtrek"),
                 withSpinner(plotOutput("HC", height = "800px", width = "90%"))),
        tabPanel(i18n$t("Percentieltabel"), DT::dataTableOutput("table")),
        tabPanel(i18n$t("Gebruik"), uiOutput("gebruik_content"))
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

  output$gebruik_content <- renderUI({
    nl <- lang() == "nl"

    example_url <- "http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA"

    if (nl) tagList(
      div(style = "max-width:760px; padding: 8px 4px 32px;",

        h3("Gebruik"),

        h4("Stap 1 — Kies een invoermethode"),
        p("Selecteer in het linkerpaneel hoe u de gegevens wilt invoeren."),

        tags$b("Excel-upload (standaard)"),
        tags$ul(
          tags$li("Download het sjabloonbestand via de link in het linkerpaneel en vul de meetwaarden in."),
          tags$li(tags$span("Stel het geslacht in cel ", tags$b("B5"), " in: ", tags$code("M"), " voor jongen, ", tags$code("F"), " voor meisje.")),
          tags$li("Vul per meting de postmenstruele leeftijd (decimaal, bv. 29,14 = 29 weken + 1 dag), het gewicht (gram), de lengte (cm) en de schedelomtrek (cm) in."),
          tags$li(tags$span("Gebruik ", tags$code("NA"), " voor ontbrekende waarden.")),
          tags$li("Upload het bestand via de knop in het linkerpaneel.")
        ),

        tags$b("Handmatige invoer (geavanceerd)"),
        tags$ul(
          tags$li("Vul de waarden per veld in als door komma's gescheiden lijst, bv. 400,500,600."),
          tags$li(tags$span("Gebruik ", tags$code("NA"), " voor ontbrekende waarden.")),
          tags$li("Alle velden moeten evenveel waarden bevatten.")
        ),

        h4("Stap 2 — Groeicurves"),
        p("De tabbladen ", tags$b("Gewicht"), ", ", tags$b("Lengte"), " en ", tags$b("Schedelomtrek"),
          " tonen de Fenton 2013 referentiecurven (P03, P10, P50, P90, P97) met uw meetwaarden als punten."),

        h4("Stap 3 — Percentieltabel"),
        p("Het tabblad ", tags$b("Percentieltabel"), " toont voor elke meting het berekende percentiel op de Fenton-referentie. Gebruik de knop ", tags$b("Bewaar als Excel-bestand"), " om de tabel te downloaden."),

        h4("Taalknop"),
        p("De knop ", tags$b("EN"), " rechtsboven wisselt naar het Engels. Alle interfacelabels, astitels en tabbladnamen worden bijgewerkt. Klik nogmaals op ", tags$b("NL"), " om terug te schakelen."),

        h4("URL-parameters (geavanceerd)"),
        p("Waarden kunnen ook worden meegegeven via een URL GET-verzoek. Ondersteunde parameters:"),
        tags$ul(
          tags$li(tags$code("advanced=yes"), " — activeer handmatige invoer"),
          tags$li(tags$code("sex_GET=M"), " of ", tags$code("sex_GET=F"), " — geslacht"),
          tags$li(tags$code("PML_GET=23+1/7,24+1/7"), " — postmenstruele leeftijd (weken; decimaal of breuknotatie)"),
          tags$li(tags$code("weight_GET=400,500"), " — gewicht in gram"),
          tags$li(tags$code("length_GET=34,NA"), " — lengte in cm"),
          tags$li(tags$code("HC_GET=23,NA"), " — schedelomtrek in cm")
        ),
        p("Voorbeeld-URL:"),
        p(a(example_url, href = example_url, style = "word-break:break-all;"))
      )
    ) else tagList(
      div(style = "max-width:760px; padding: 8px 4px 32px;",

        h3("Usage"),

        h4("Step 1 — Choose an input method"),
        p("Select how you want to enter data in the left-hand panel."),

        tags$b("Excel upload (default)"),
        tags$ul(
          tags$li("Download the template file using the link in the left panel and fill in your measurements."),
          tags$li(tags$span("Set the sex in cell ", tags$b("B5"), ": ", tags$code("M"), " for boy, ", tags$code("F"), " for girl.")),
          tags$li("For each measurement, enter the postmenstrual age (decimal, e.g. 29.14 = 29 weeks + 1 day), weight (grams), length (cm), and head circumference (cm)."),
          tags$li(tags$span("Use ", tags$code("NA"), " for missing values.")),
          tags$li("Upload the completed file using the button in the left panel.")
        ),

        tags$b("Manual entry (advanced)"),
        tags$ul(
          tags$li("Enter values for each field as a comma-separated list, e.g. 400,500,600."),
          tags$li(tags$span("Use ", tags$code("NA"), " for missing values.")),
          tags$li("All fields must contain the same number of values.")
        ),

        h4("Step 2 — Growth curves"),
        p("The ", tags$b("Weight"), ", ", tags$b("Length"), ", and ", tags$b("Head circumference"),
          " tabs display the Fenton 2013 reference curves (P03, P10, P50, P90, P97) with your measurements plotted as points."),

        h4("Step 3 — Percentile table"),
        p("The ", tags$b("Percentile table"), " tab shows the calculated percentile on the Fenton reference for each measurement. Use the ", tags$b("Save as Excel file"), " button to download the table."),

        h4("Language button"),
        p("Click the ", tags$b("NL"), " button in the top-right corner to switch to Dutch. All interface labels, axis titles, and tab names are updated. Click ", tags$b("EN"), " to switch back."),

        h4("URL parameters (advanced)"),
        p("Values can also be passed via a URL GET request. Supported parameters:"),
        tags$ul(
          tags$li(tags$code("advanced=yes"), " — enable manual entry"),
          tags$li(tags$code("sex_GET=M"), " or ", tags$code("sex_GET=F"), " — sex"),
          tags$li(tags$code("PML_GET=23+1/7,24+1/7"), " — postmenstrual age (weeks; decimal or fraction notation)"),
          tags$li(tags$code("weight_GET=400,500"), " — weight in grams"),
          tags$li(tags$code("length_GET=34,NA"), " — length in cm"),
          tags$li(tags$code("HC_GET=23,NA"), " — head circumference in cm")
        ),
        p("Example URL:"),
        p(a(example_url, href = example_url, style = "word-break:break-all;"))
      )
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
