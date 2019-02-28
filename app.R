# Tableau Zen Parser - shinyapp
# Tony Liu

options(shiny.maxRequestSize = 100 * 1024 ^ 2)

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(rvest)
library(xml2)
library(visNetwork)

#---------------------------------Define UI for application---------------------------------
ui <- fluidPage(
  # css
  tags$head(tags$style(
    HTML(
      '
      #sidebar {
      background-color: transparent;
      border-color: transparent
      }
      
      body, label, input, button, select {
      font-family: "Arial";
      }'
      )
    )),
  
  # Application title
  titlePanel("Tableau Zen Parser v0.1"),
  h4("Tony Liu, Feb 2019"),
  
  # Sidebar with options to update a dataframe (in csv)
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      width = 2,
      # fileInput("input_twb", "upload csv file", width = "360px",
      #           accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      # actionButton("input_demo", "demo"),
      
      # div(
      #   style = "display: inline-block;vertical-align:top;",
        fileInput(
          "input_twb",
          "upload .twb file",
          # width = "300px",
          accept = c(".twb")
        # )
      ),
      
      actionButton("input_demo", "demo", width = "70px"),
      # div(style = "display: inline-block;vertical-align:top; padding-top: 20px;",
      #     actionButton("input_run", "run", width = "70px")),
      # div(style = "display: inline-block;vertical-align:top; padding-top: 20px;",
      #     actionButton("input_demo", "demo", width = "70px")),
      
      br(), 
      br(), 
      numericInput("RANDOM_SEED", "Random seed for layout", 3), 
      checkboxInput("INCLUDE_RAW_VAR", label = "Include raw variable?", value = TRUE),
      checkboxInput("INCLUDE_PARA", label = "Include parameter?", value = TRUE),
      br(), 
      checkboxInput("USE_IGRAPH_LAYOUT", label = "Use igraph layout?", value = FALSE), 
      selectizeInput("IGRAPH_LAYOUT", label = "Choose igraph layout from below", 
                     choices = c("layout.auto", 
                                 "layout_nicely",
                                 "layout_as_star", 
                                 "layout_in_circle", 
                                 "layout.davidson.harel", 
                                 "layout_with_dh", 
                                 "layout_with_gem", 
                                 "layout_with_graphopt", 
                                 "layout.grid", 
                                 "layout.grid.3d", 
                                 "layout_with_mds", 
                                 "layout_components", 
                                 "piecewise.layout", 
                                 "layout.norm", 
                                 "layout_with_sugiyama", 
                                 "layout_on_sphere", 
                                 "layout_with_fr", 
                                 "layout_with_kk", 
                                 "layout_with_lgl", 
                                 "normalize"))
    ),
    
    # Show visNetwork output
    mainPanel(tabsetPanel(
      tabPanel(
        "Data source",
        h2("Data source"),
        DT::dataTableOutput("data_src_tbl"),
        h2("Excel file path"),
        DT::dataTableOutput("excel_src_path")
      ),
      tabPanel(
        "Calculated field visualisation",
        br(),
        p("purple: raw variables", style = "color:#AD8BC9", 
          span("orange: parameters", 
               style = "padding-left:19px; color:#FF9E4A"), 
          span("green: calculated fields (measure)", 
               style = "padding-left:12px; color:#67BF5C"), 
          span("blue: calculated fields (dimension)", 
               style = "padding-left:12px; color:#729ECE")), 
        # br(),
        withSpinner(visNetworkOutput("visNetwork_output", width = "100%", height = "800px"))
      ), 
      tabPanel(
        "Calculated field table", 
        h2("List of calculated fields"),
        DT::dataTableOutput("all_calc_tbl")
      )
    ))
  )
    )

#---------------------------------Define server logic---------------------------------
server <- shinyServer(function(input, output) {
  ##--------------------Load workbook----------------------------------------
  tableau_xml <- reactive({
    if (!input$input_demo)
      req(input$input_twb)
    
    inFile <- input$input_twb
    
    if (input$input_demo)
      dd <-
      read_xml("Ward Population Pyramid - Quinary Age.twb")
    else
      dd <- read_xml(inFile$datapath)
    
    return(dd)
  })
  
  ##----------------------Get data soucce tbl----------------------------------------
  output$data_src_tbl <- DT::renderDataTable({
    if (!input$input_demo)
      req(input$input_twb)
    
    data_src <-
      xml_find_all(tableau_xml(), "//relation[boolean(@table)]") %>%
      xml_attrs() %>%
      do.call(bind_rows, .)
    
    DT::datatable(data_src,
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ))
  })
  
  output$excel_src_path <- DT::renderDataTable({
    if (!input$input_demo)
      req(input$input_twb)
    
    excel_path <- bind_cols(
      xml_find_all(
        tableau_xml(),
        "//named-connection[contains(@name, 'excel-direct')]"
      ) %>%
        xml_attrs() %>%
        do.call(bind_rows, .),
      
      xml_find_all(
        tableau_xml(),
        "//named-connection[contains(@name, 'excel-direct')]//connection"
      ) %>%
        xml_attrs() %>%
        do.call(bind_rows, .)
    )
    
    if (length(excel_path) > 0)
    DT::datatable(excel_path[, 1:7],
                  extensions = 'Buttons',
                  options = list(
                    dom = 'Bfrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                  ))
  })
  
  ##--------------------Get calculated field----------------------------------------
  all_para <- reactive({
    xml_find_all(tableau_xml(),
                 "//column[boolean(@param-domain-type)]") %>%
      xml_attr("caption") %>%
      unique()
  })
  
  all_var_raw <- reactive({
    xml_find_all(tableau_xml(),
                 "//column[boolean(@name) and not(.//calculation)]") %>%
      xml_attrs() %>%
      do.call(bind_rows, .) %>%
      mutate(caption = ifelse(
        is.na(caption),
        str_replace_all(name, "[\\[|\\]]", ""),
        caption
      )) %>%
      distinct()
  })
  
  all_calc <- reactive({
    all_calc <- bind_cols(
      xml_find_all(
        tableau_xml(),
        "//column[boolean(@caption) and .//calculation]"
      ) %>%
        xml_attrs() %>%
        do.call(bind_rows, .),
      
      xml_find_all(
        tableau_xml(),
        "//column[boolean(@caption)]//calculation"
      ) %>%
        xml_attrs() %>%
        do.call(bind_rows, .)
    ) %>%
      distinct() %>%
      arrange(caption)
    
    if (input$INCLUDE_RAW_VAR)
      all_calc <- bind_rows(all_calc, all_var_raw())
    
    if (!input$INCLUDE_PARA)
      all_calc <- all_calc %>% filter(!(caption %in% all_para()))
    
    return(all_calc)
  })
  
  ##--------------------Generate visNetwork graph----------------------------
  output$visNetwork_output <- renderVisNetwork({
    
    all_calc <- all_calc()
    all_para <- all_para()
    all_var_raw <- all_var_raw()
    
    add_back_slash <- function(string) {
      string %>%
        str_replace_all("\\[", "\\\\\\[") %>%
        str_replace_all("\\]", "\\\\\\]") %>%
        str_replace_all("\\(", "\\\\\\(") %>%
        str_replace_all("\\)", "\\\\\\)")
    }
    
    put_in_sq_bracket <- function(string) {
      paste0("[", string, "]")
    }
    
    for (i in 1:nrow(all_calc)) {
      # print(i)
      all_calc$formula <- str_replace_all(
        all_calc$formula,
        all_calc$name[i] %>% 
          add_back_slash(), 
        put_in_sq_bracket(all_calc$caption[i])
      )
    }
    
    all_calc_network <-
      tibble(
        formula = rep(all_calc$formula, each = nrow(all_calc)),
        caption_output = rep(all_calc$caption, each = nrow(all_calc)),
        caption_input = rep(all_calc$caption, nrow(all_calc))
      ) %>%
      mutate(n = ifelse(
        str_detect(formula, 
                   caption_input %>% 
                     put_in_sq_bracket() %>% 
                     add_back_slash()),
        1,0)) %>%
      filter(n > 0) %>%
      filter(caption_output != caption_input) %>%
      filter(caption_input != 0, caption_output != 0)
    
    all_calc_node_count <- all_calc_network %>%
      group_by(caption_input) %>%
      summarise(n = sum(n)) %>%
      arrange(-n)
    
    ##--------------------Visualise the network----------------------------------------
    nodes <- data.frame(
      id = 1:nrow(all_calc),
      label = all_calc$caption,
      value = all_calc_node_count$n[match(all_calc$caption, all_calc_node_count$caption_input)],
      group = ifelse(
        all_calc$caption %in% all_para,
        "parameter",
        ifelse(
          all_calc$caption %in% all_var_raw$caption,
          "raw_variable",
          "calculated field"
        )
      ),
      color = ifelse(
        all_calc$caption %in% all_para,
        rgb(255, 158, 74, max = 255),
        ifelse(
          all_calc$caption %in% all_var_raw$caption,
          rgb(173, 139, 201, max = 255),
          case_when(
            all_calc$role == "measure" ~ rgb(103, 191, 92, max = 255),
            all_calc$role == "dimension" ~ rgb(114, 158, 206, max =
                                                 255)
          )
        )
      ),
      shape = case_when(
        all_calc$datatype == "string" ~ "square",
        all_calc$datatype %in% c("real", "integer") ~ "dot",
        all_calc$datatype == "boolean" ~ "triangle",
        all_calc$datatype %in% c("date", "datetime") ~ "box"
      ),
      title = paste0(
        all_calc$caption,
        " (",
        all_calc$datatype,
        ")",
        "<br><br>",
        all_calc$formula %>%
          str_replace_all("\\n", "<br>") %>%
          str_replace_all("//", "<br>//")
      )
    ) %>%
      mutate(
        value = replace_na(value, .5),
        value = ifelse(shape == "box", 2.5 * value, value),
        value = ifelse(shape == "diamond", .8 * value, value),
        value = ifelse(shape == "ellipse", 2 * value, value),
        value = ifelse(shape == "square", 1.1 * value, value),
        value = ifelse(shape == "triangle", 1.5 * value, value)
      )
    
    edges <-
      data.frame(
        from = (1:nrow(all_calc))[match(all_calc_network$caption_input, all_calc$caption)],
        to = (1:nrow(all_calc))[match(all_calc_network$caption_output, all_calc$caption)],
        length = all_calc_network$n
      )
    nodes <- nodes %>% filter(id %in% c(edges$from, edges$to))
    
    vis_output <- visNetwork(nodes, edges,
                             height = "1600px", width = "100%") %>%
      visNodes(
        font = list(size = 10),
        scaling = list(min = 10, max = 60),
        shadow = TRUE
      ) %>%
      visEdges(color = list(opacity = .6), arrows = "to") %>%
      # visIgraphLayout(layout = "layout.davidson.harel") %>%
      # visPhysics(stabilization = FALSE) %>%
      # visEdges(smooth = FALSE) %>%
      visInteraction(
        hideEdgesOnDrag = TRUE,
        multiselect = TRUE,
        selectConnectedEdges = TRUE,
        hoverConnectedEdges = TRUE,
        hover = TRUE
      ) %>%
      # visConfigure(enabled = TRUE) %>%
      visOptions(
        manipulation = TRUE, 
        highlightNearest = list(
          enabled = TRUE,
          degree = .1,
          hover = TRUE,
          algorithm = "hierarchical"
        ),
        nodesIdSelection = list(
          enabled = TRUE,
          main = "Selecte by variable name",
          style = "width:300px"
        ), 
        selectedBy = "group"
      ) %>%
      visLayout(randomSeed = input$RANDOM_SEED)
    
    if (input$USE_IGRAPH_LAYOUT) vis_output <- vis_output %>% visIgraphLayout(input$IGRAPH_LAYOUT)
    
    return(vis_output)
  })
  
  #------------------------all_calc datatable----------------------------------------
  output$all_calc_tbl <- DT::renderDataTable({

      all_calc() %>% 
        DT::datatable(extensions = 'Buttons', 
                      filter = 'top', 
                      options = list(
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                      ))
    })
})

# Run the application
shinyApp(ui = ui, server = server)
