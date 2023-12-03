library(tidyverse)
library(shinythemes)
library(shiny)

# Define UI for application 
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Fenton groeicurves"),
  sidebarLayout(sidebarPanel(
    tabsetPanel(
      type = "hidden",
      id = "main",
      tabPanel("Main",
               selectInput(
                 "advanced", "Show advanced settings",
                 c("no" = "no",
                   "yes" = "yes")
               )),
      conditionalPanel(
        condition = "input.advanced == 'yes'",
        textInput("sex_GET", "sex (M of F)", value = "M"),
        textInput(
          "PML_GET",
          "Postmenstrual age (weeks in 23+1/7 format) in CSV",
          value = NA
        ),
        textInput("HC_GET", "head circumference (in cm) in CSV ", value = NA),
        textInput("weight_GET", "weight (in gram) in CSV ", value = NA),
        textInput("length_GET", "length (in cm) in CSV ", value = NA),
      ),
    ),
  ),
  
  # Show output
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Weight",
        plotOutput("weight", height = "550px", width = "700px"),
        hr(),
        p(
          "This tool has not been extensively tested, caution is advised. Code is available at https://github.com/rmvpaeme/fenton-shiny. Source: Fenton TR, Kim JH. A systematic review and meta-analysis to revise the Fenton growth chart for preterm infants. BMC Pediatr. 2013;13:59. An application to manually enter growth values can be found at "
        ),
        a("https://peditools.org/peditools_universal/"),
      ),
      tabPanel("Length", plotOutput("L", height = "550px", width = "700px")),
      tabPanel(
        "Head Circumference",
        plotOutput("HC", height = "550px", width = "700px")
      ),
      tabPanel("table", tableOutput("table")),
      
      tabPanel(
        "Documentation",
        p("Values can be entered through a GET request. Examples are"),
        a(
          "http://rubenvp.shinyapps.io/fenton/?advanced=yes&sex_GET=M&PML_GET=23%2B1/7,24%2B1/7,25%2B1/7&weight_GET=400,500,600&HC_GET=23,NA,25&length_GET=34,33,NA"
        )
      )
    )
  ))
)

# Define server logic 
server <- function(input, output, session) {
  newData <- reactive({
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
        annotation = "sample",
        type = "weight"
      )
    df_wt <-
      df_wt %>% rowwise() %>% mutate(PML = calc(PML_GET)) %>% select(-PML_GET)
    
    length_GET_split <-
      as.numeric(unlist(strsplit(length_GET, split = ",")))
    df_length <-
      tibble(
        value = length_GET_split,
        PML_GET = PML_GET,
        annotation = "sample",
        type = "length"
      )
    df_length <-
      df_length %>% rowwise() %>% mutate(PML = calc(PML_GET))  %>% select(-PML_GET)
    
    HC_GET_split <-
      as.numeric(unlist(strsplit(HC_GET, split = ",")))
    df_HC <-
      tibble(
        value = HC_GET_split,
        PML_GET = PML_GET,
        annotation = "sample",
        type = "HC"
      )
    df_HC <-
      df_HC %>% rowwise() %>% mutate(PML = calc(PML_GET)) %>% select(-PML_GET)
    
    
    #        } else {
    #          df_wt <- tibble(value = NA, PML = NA, annotation = NA, type = NA)
    #          df_HC <- tibble(value = NA, PML = NA, annotation = NA, type = NA)
    #          df_length<- tibble(value = NA, PML = NA, annotation = NA, type = NA)
    #        }
    
    if (input$sex_GET == "M") {
      sex_label = "boys"
      df <-
        read_csv("./data/boys_all.csv") %>% mutate(PML = `Compl weeks`) %>% select(-`Compl weeks`)
      df <- bind_rows(df, df_wt, df_length, df_HC)
    } else if (input$sex_GET == "F") {
      sex_label = "girls"
      df <-
        read_csv("./data/girls_all.csv") %>% mutate(PML = `Compl weeks`) %>% select(-`Compl weeks`)
      df <- bind_rows(df, df_wt, df_length, df_HC)
    }
    
    
    
  })
  
  
  output$table <- renderTable({
    LMS2p <- function(L, M, S, X) {
      Z = (((X / M) ^ L) - 1) / (L * S)
      P <- pnorm(Z)
    }
    df <- newData()
    
    df_spread_sample <-
      df %>% filter(annotation == "sample")  %>% spread(key = annotation, value = value) %>% select(-c(L, M, S))
    df_spread_sample$PML_orig <- df_spread_sample$PML
    df_spread_sample$PML <- round(df_spread_sample$PML, 0)
    df_spread_sample$annotation <- "sample"
    df_spread_LMS <-
      df %>% filter(annotation != "sample")  %>% spread(key = annotation, value = value)
    df_spread_all <- left_join(df_spread_LMS, df_spread_sample)
    df_spread_all$P_sample <-
      LMS2p(df_spread_all$L,
            df_spread_all$M,
            df_spread_all$S,
            X = df_spread_all$sample)
    df_spread_all <-
      df_spread_all %>% filter(annotation == "sample") %>% select(c(PML_orig, P_sample, sample, type))
    df_spread_all <-
      df_spread_all %>% mutate(PML = PML_orig,
                               Percentile = P_sample,
                               value = sample) %>% select(-c(PML_orig, P_sample, sample))
    df_spread_all <-
      df_spread_all %>% filter(value > 0) %>% arrange(type, PML)
  })
  
  output$weight <- renderPlot({
    if (input$sex_GET == "M") {
      sex_label = "boys"
    } else if (input$sex_GET == "F") {
      sex_label = "girls"
    }
    
    df <- newData()
    
    ggplot(
      data = df %>% filter(type == "weight", annotation != "sample"),
      aes(
        x = PML,
        y = as.numeric(value),
        linetype = annotation
      )
    )  +
      theme_bw() +
      geom_line() +
      geom_point(
        data = df %>% filter(type == "weight", annotation == "sample", value > 0),
        aes(
          y = as.numeric(value),
          x = PML,
          col = annotation,
          linetype = NA
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
      scale_x_continuous(breaks = seq(22, 42, 1), name = "PML") +
      scale_y_continuous(
        breaks = seq(0, 5000, 400),
        limits = c(0, 5000),
        name = "gram"
      )
    
  })
  
  output$L <- renderPlot({
    
    if (input$sex_GET == "M") {
      sex_label = "boys"
    } else if (input$sex_GET == "F") {
      sex_label = "girls"
    }
    
    df <- newData()
    
    ggplot(
      data = df %>% filter(type  %in% c("length"), annotation != "sample"),
      aes(
        x = PML,
        y = as.numeric(value),
        linetype = annotation
      )
    )  +
      geom_line() +
      geom_point(
        data = df %>% filter(type == "length", annotation == "sample", value > 0),
        aes(
          y = as.numeric(value),
          x = PML,
          col = annotation,
          linetype = NA
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
      scale_y_continuous(breaks = seq(18, 60, 2), name = "centimeter") +
      scale_x_continuous(breaks = seq(22, 42, 1), name = "PML") #+ facet_wrap(~ type, ncol = 1, scales = "free") #+ ylim(0,5)
    
  })
  
  
  output$HC <- renderPlot({
    if (input$sex_GET == "M") {
      sex_label = "boys"
    } else if (input$sex_GET == "F") {
      sex_label = "girls"
    }
    
    df <- newData()
    
    ggplot(
      data = df %>% filter(type  %in% c("HC"), annotation != "sample"),
      aes(
        x = PML,
        y = as.numeric(value),
        linetype = annotation
      )
    )  +
      geom_line() +
      geom_point(
        data = df %>% filter(type == "HC", annotation == "sample", value > 0),
        aes(
          y = as.numeric(value),
          x = PML,
          col = annotation,
          linetype = NA
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
      scale_x_continuous(breaks = seq(22, 42, 1), name = "PML") #+ facet_wrap(~ type, ncol = 1, scales = "free") #+ ylim(0,5)
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
