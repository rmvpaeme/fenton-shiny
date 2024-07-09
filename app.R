library(tidyverse)
library(shinythemes)
library(shiny)
library(DT)

# Define UI for application 
#shiny::addResourcePath('www', '/srv/shiny-server/www')
ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("NICU growth curves"),
  sidebarLayout(sidebarPanel(
    tabsetPanel(
      type = "hidden",
      id = "main",
      tabPanel("Main",
               selectInput(
                 "advanced", "Show advanced settings",
                 c("no" = "no",
                   "yes" = "yes")
               ),),
      conditionalPanel(
        condition = "input.advanced == 'yes'",
        textInput("sex_GET", "sex (M or F)", value = "M"),
        textInput(
          "PML_GET",
          "Postmenstrual age (weeks in 23+1/7 format) in CSV",
          value = NA
        ),
        textInput("HC_GET", "head circumference (in cm) in CSV ", value = NA),
        textInput("weight_GET", "weight (in gram) in CSV ", value = NA),
        textInput("length_GET", "length (in cm) in CSV ", value = NA),
        
      ),
      conditionalPanel(
        condition = "(input.advanced == 'no')",
        fileInput(
          'file_excel',
          p('Upload Excel file with custom data.', a('Click here', href = "https://github.com/rmvpaeme/fenton-shiny/raw/main/example_excel.xlsx"),  'to download the template Excel file.'),
          accept = c(".xlsx")
        ), 
        
      )
    ),
  ),
  
  # Show output
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Weight",
        plotOutput("weight", height = "800px", width = "90%"),
        downloadButton("report", "Download PDF"),
        hr(),
        h4("Disclaimer", style = "font-size:12px;"),
        p(
          "This tool has not been extensively tested, caution is advised. Code is available at", a("https://github.com/rmvpaeme/fenton-shiny", href = "https://github.com/rmvpaeme/fenton-shiny", style = "font-size:12px;"), ". Source: Fenton TR, Kim JH. A systematic review and meta-analysis to revise the Fenton growth chart for preterm infants. BMC Pediatr. 2013;13:59. A more extensive application to manually enter growth values can be found at "
          , style = "font-size:12px;",
        a("https://peditools.org/peditools_universal/", href = "https://peditools.org/peditools_universal/", style = "font-size:12px;")),
      ),
      tabPanel("Length", plotOutput("L", height = "800px", width = "90%")),
      tabPanel(
        "Head Circumference",
        plotOutput("HC", height = "800px", width = "90%"),
      ),
      tabPanel("Percentile table", DT::dataTableOutput("table")),
      
      tabPanel(
        "Usage",
        p("Values can be entered through a GET request. Example: ",
        a(
          "http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA", 
          href = "http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA"
        )
      )),
      
    )
  ))
)

# Define server logic 
server <- function(input, output, session) {
  newData <- reactive({
    inFile_excel <- input$file_excel
    
    if (is.null(inFile_excel)) {
      calc <- function(x)
        eval(parse(text = x))
      
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['naam']])) {
        updateTextInput(session, "naam", value = query[['naam']])
      }
      if (!is.null(query[['advanced']])) {
        updateTextInput(session, "advanced", value = query[['advanced']])
      }
      if (!is.null(query[['sex_GET']])) {
        updateTextInput(session, "sex_GET", value = query[['sex_GET']])
      }
      if (!is.null(query[['HC_GET']])) {
        updateTextInput(session, "HC_GET", value = query[['HC_GET']])
      }
      if (!is.null(query[['length_GET']])) {
        updateTextInput(session, "length_GET", value = query[['length_GET']])
      }
      if (!is.null(query[['weight_GET']])) {
        updateTextInput(session, "weight_GET", value = query[['weight_GET']])
      }
      if (!is.null(query[['PML_GET']])) {
        updateTextInput(session, "PML_GET", value = query[['PML_GET']])
      }
      
      
      PML_GET <- c("23+1/7,24+1/7")
      HC_GET <- as.character(c("23,24"))
      length_GET  <- as.character(c("30,35"))
      sex_GET <- as.character(c("M"))
      weight_GET <- as.character(c("400,500"))
      HC_GET <- as.character(input$HC_GET)
      length_GET  <- as.character(input$length_GET)
      weight_GET <- as.character(input$weight_GET)
      PML_GET <- as.character(input$PML_GET)
      sex_GET <- as.character(input$sex_GET)
      
      
      PML_GET <-
        as.character(unlist(strsplit(PML_GET, split = ",")))
      
      wt_GET_split <-
        as.numeric(unlist(strsplit(weight_GET, split = ",")))
      df_wt <-
        tibble(
          value = wt_GET_split,
          PML_GET = PML_GET,
          annotation = "measure",
          type = "weight"
        )
      
      
      length_GET_split <-
        as.numeric(unlist(strsplit(length_GET, split = ",")))
      df_length <-
        tibble(
          value = length_GET_split,
          PML_GET = PML_GET,
          annotation = "measure",
          type = "length"
        )
      
      HC_GET_split <-
        as.numeric(unlist(strsplit(HC_GET, split = ",")))
      df_HC <-
        tibble(
          value = HC_GET_split,
          PML_GET = PML_GET,
          annotation = "measure",
          type = "HC"
        )
      
      df_wt <-
        df_wt %>% rowwise() %>% mutate(PML = calc(PML_GET)) %>% select(-PML_GET)
      df_length <-
        df_length %>% rowwise() %>% mutate(PML = calc(PML_GET))  %>% select(-PML_GET)
      df_HC <-
        df_HC %>% rowwise() %>% mutate(PML = calc(PML_GET)) %>% select(-PML_GET)
    
    if (input$sex_GET == "M" ) {
      sex_label = "boys"
      df <-
        read_csv("./data/boys_all.csv") %>% mutate(PML = Time) %>% select(-Time)
      df <- bind_rows(df, df_wt, df_length, df_HC)
    } else if (input$sex_GET == "F")  {
      sex_label = "girls"
      df <-
        read_csv("./data/girls_all.csv") %>% mutate(PML = Time) %>% select(-Time)
      df <- bind_rows(df, df_wt, df_length, df_HC)
    }
  }
    else if (!is.null(inFile_excel)) {
        #inFile_excel <- tibble( datapath = "/Users/rmvpaeme/fenton/example_excel.xlsx")
        #df_wt <- readxl::read_excel(test, skip = 9, col_names = c("date", "PML", "value", "drop1", "drop2")) %>% select(-drop1, -drop2)
        #sex_excel <- readxl::read_excel(test, range = "B5", col_names = FALSE) %>% pull()
        
        df_wt <- readxl::read_excel(inFile_excel$datapath, skip = 9, col_names = c("date", "PML", "value", "drop1", "drop2")) %>% select(-date, -drop1, -drop2)
        df_wt$annotation <- "measure"
        df_wt$type <- "weight"
        df_wt$value <- as.double(df_wt$value)
        
        df_length <- readxl::read_excel(inFile_excel$datapath, skip = 9, col_names = c("date", "PML", "drop1", "value", "drop2")) %>% select(-date, -drop1, -drop2)
        df_length$annotation <- "measure"
        df_length$type <- "length"
        df_length$value <- as.double(df_length$value)
        
        df_HC <- readxl::read_excel(inFile_excel$datapath, skip = 9, col_names = c("date", "PML", "drop1", "drop2", "value")) %>% select(-date, -drop1, -drop2)
        df_HC$annotation <- "measure"
        df_HC$type <- "HC"
        df_HC$value <- as.double(df_HC$value)
        
        sex_excel <- readxl::read_excel(inFile_excel$datapath, range = "B5", col_names = FALSE) %>% pull()
        sex_excel <- as.character(sex_excel)
        
        
        if (sex_excel == "M" ) {
          sex_label = "boys"
          df <-
            read_csv("./data/boys_all.csv") %>% mutate(PML = Time) %>% select(-Time)
          df <- bind_rows(df, df_wt, df_length, df_HC)
        } else if (sex_excel == "F")  {
          sex_label = "girls"
          df <-
            read_csv("./data/girls_all.csv") %>% mutate(PML = Time) %>% select(-Time)
          df <- bind_rows(df, df_wt, df_length, df_HC)
          
        }
    }

    list(df = df, sex_label = sex_label)
    
    
  })
  
  
  output$table <-  DT::renderDataTable({
    LMS2p <- function(L, M, S, X) {
      Z = (((X / M) ^ L) - 1) / (L * S)
      P <- pnorm(Z)
    }
    df <- newData()$df
    
    df_spread_measure <-
      df %>% filter(annotation == "measure")  %>% spread(key = annotation, value = value) %>% select(-c(L, M, S))
    df_spread_measure$PML_orig <- df_spread_measure$PML
    df_spread_measure$PML <- round(df_spread_measure$PML, 2)
    df_spread_measure$annotation <- "measure"
    df_spread_LMS <-
      df %>% filter(annotation != "measure")  %>% spread(key = annotation, value = value)
    df_spread_LMS$PML <- round( df_spread_LMS$PML, 2)
    df_spread_all <- left_join(df_spread_LMS, df_spread_measure)
    df_spread_all$P_measure <-
      LMS2p(df_spread_all$L,
            df_spread_all$M,
            df_spread_all$S,
            X = df_spread_all$measure)
    df_spread_all <-
      df_spread_all %>% filter(annotation == "measure") %>% select(c(PML_orig, P_measure, measure, type))
    df_spread_all <-
      df_spread_all %>% mutate(measurement = type, GA = PML_orig,
                               Percentile = P_measure,
                               value = measure) %>% select(-c(type, PML_orig, P_measure, measure))
    df_spread_all <-
      df_spread_all %>% filter(value > 0) %>% mutate(Percentile = Percentile*100) %>% arrange(measurement, GA)
    df_spread_all <- df_spread_all %>% mutate_if(is.numeric, ~ round(., 2))
    DT::datatable({
      df_spread_all
    },
    #caption = "Je kan de tabel opslaan via de Excel knop om nadien terug te importeren in de tool om extra waarden toe te voegen. Belangrijk: doe zelf geen aanpassingen aan de Excel.",  
    extensions = 'Buttons',
    
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      dom = 'frtBip',
      buttons = list(
        list(
          extend = "excel", 
          text = "Save as Excel file"
        )
      )
    ),
    rownames = FALSE,
    
    class = "display")
    
    })
  
  Plot_weight <- reactive({
    sex_label <- newData()$sex_label
    df <- newData()$df
    
    ggplot(
      data = df %>% filter(type == "weight", annotation != "measure"),
      aes(
        x = PML,
        y = as.numeric(value),
        linetype = annotation
      )
    )  +
      theme_bw() +
      geom_line() +
      geom_point(
        data = df %>% filter(type == "weight", annotation == "measure", value > 0),
        aes(
          y = as.numeric(value),
          x = PML,
          col = annotation,
          linetype = NULL
        ),
        size = 3
      ) +
      labs(subtitle = paste0("Fenton growth curve, weight for ", sex_label)) +
      theme(
        text = element_text(size = 20),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_blank()
      ) +
      scale_x_continuous(breaks = seq(22, 50, 2), name = "gestational age (weeks)") +
      scale_y_continuous(
        breaks = seq(0, 7200, 400),
        limits = c(0, 7200),
        name = "gram"
      )
  })
  
  Plot_length <- reactive({
    
    sex_label <- newData()$sex_label
    df <- newData()$df
    
    ggplot(
      data = df %>% filter(type  %in% c("length"), annotation != "measure", value > 0),
      aes(
        x = PML,
        y = as.numeric(value),
        linetype = annotation
      )
    )  +
      geom_line() +
      geom_point(
        data = df %>% filter(type == "length", annotation == "measure", value > 0),
        aes(
          y = as.numeric(value),
          x = PML,
          col = annotation,
          linetype = NULL
        ),
        size = 3
      ) +
      theme_bw() +
      labs(subtitle = paste0("Fenton growth curve, length for ", sex_label)) +
      theme(
        text = element_text(size = 20),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_blank()
      ) +
      scale_y_continuous(breaks = seq(18, 70, 4), name = "centimeter") +
      scale_x_continuous(breaks = seq(22, 50, 2), name = "gestational age (weeks)")
  })

  Plot_HC <- reactive({
    sex_label <- newData()$sex_label
    df <- newData()$df
    
    ggplot(
      data = df %>% filter(type  %in% c("HC"), annotation != "measure"),
      aes(
        x = PML,
        y = as.numeric(value),
        linetype = annotation
      )
    )  +
      geom_line() +
      geom_point(
        data = df %>% filter(type == "HC", annotation == "measure", value > 0),
        aes(
          y = as.numeric(value),
          x = PML,
          col = annotation,
          linetype = NULL
        ),
        size = 3
      ) +
      theme_bw() +
      labs(subtitle = paste0("Fenton growth curve, head circumference for ", sex_label)) +
      theme(
        text = element_text(size = 20),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_blank()
      ) +
      scale_y_continuous(breaks = seq(18, 60, 2), name = "centimeter") +
      scale_x_continuous(breaks = seq(22, 50, 2), name = "gestational age (weeks)") #+ facet_wrap(~ type, ncol = 1, scales = "free") #+ ylim(0,5)
    
  })
  output$weight <- renderPlot({
    p <- Plot_weight()
    print(p)
  })
  
  output$L <- renderPlot({
    p <- Plot_length()
    print(p)

  })
  
  output$HC <- renderPlot({
    p <- Plot_HC()
    print(p)
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(w = Plot_weight(), l = Plot_length(), hc = Plot_HC())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)