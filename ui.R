library(ggplot2)

d_compnum <- 1:30     #no. of companies for heat map choice
d_compnum_intl <- 20  #initial no. of companies selected for heat map
d_compname <- sort(unique(sample_agg_data$company))

d_compname_intl <- sort(unique(sample_agg_data$company))[1]
d_peer1_intl <- sort(unique(sample_agg_data$company))[2]
d_peer2_intl <- sort(unique(sample_agg_data$company))[3]
d_peer3_intl <- sort(unique(sample_agg_data$company))[4]
d_peer_syear <- d_syear
d_peer_fyear <- d_fyear

d_compname_top <- sort(unique(top_comp_geog$company))
d_compname_top_intl <- sort(unique(top_comp_geog$company))[dim(top_comp_geog)[1]]

shinyUI(
  
  dashboardPage(
    
  skin = "blue",
  
  dashboardHeader(title = "Shiny Project",
                  titleWidth = 275), 
  
  dashboardSidebar(
    
    width = 275,
    
    tags$head(tags$style(HTML(".content {background-color: white;
                          border-bottom-color: white;
                          border-bottom-color: white;
                          border-left-color: white;
                          border-right-color: white;
                          border-top-color: white}"))),
    
    div(id = "tab_aggsum_sidebar",
        selectInput("syear",
                    "Select Start Year:",
                    choices = d_syear:d_fyear,
                    selected = d_syear),
        
        selectInput("fyear",
                    "Select End Year:",
                    choices = d_syear:d_fyear,
                    selected = d_fyear),
        
        checkboxGroupInput("product",
                           "Select Products to Summarise:",
                           choices = unique(levels(sample_agg_data$product)),
                           selected = unique(levels(sample_agg_data$product))[1:8]),
        
        selectInput("fill",
                    "Select How to Display:",
                    choices = list("Frequency by Year" = 1,
                                   "% Breakdown by Year" = 2),
                    selected = 2)),
    
    shinyjs::hidden(
      div(id = "tab_prodbr_sidebar",
          selectInput("prodbr_state",
                      "Select State:",
                      choices = unique(popdata$state_name),
                      selected = unique(popdata$state_name)[1]),
          
          selectInput("prodbr_year",
                      "Select Year:",
                      choices = d_syear:d_fyear,
                      selected = d_fyear)
      )),
    
    shinyjs::hidden(
      div(id = "tab_heatmap_sidebar",
          selectInput("compnum",
                      "No. of Companies to Include:",
                      choices = d_compnum,
                      selected = d_compnum_intl),
          
          checkboxGroupInput("regionlist",
                        "Regions to Include:",
                        choices = unique(popdata$region),
                        selected = unique(popdata$region)[c(1,3)])
      )),
    
    shinyjs::hidden(
    div(id = "tab_geogrcomp_sidebar",
        
        selectInput("geogrcomp_var",
                    "Select Outcome:",
                    choices = c("Total complaints", "% Timely Resolution"),
                    selected = "Total complaints"),
        
        selectInput("geogrcomp_comp",
                    "Select Company:",
                    choices = d_compname_top,
                    selected = d_compname_top_intl)))
  ),

  dashboardBody(
    
    tags$head(tags$style(
      HTML('
           body, label, input, button, select { 
           font-family: "Optima";
           }
           
           .main-header .logo {
           font-family: "Optima";
           
           font-size: 24px;
           }'))),
    
    tags$style(".content {background-color: white;}"),
    
    useShinyjs(),
    tabsetPanel( #tabItems
      
      id = "navbar",
      tabPanel(title = "Aggregate Summary", #tabItem
               id = "tab_aggsum",
               value = "tab_aggsum_val",
               plotOutput("agg_tseries", width = 700, height = 550)),
      
      tabPanel(
        tags$style(".content {background-color: white;}"),
        title = "Product Breakdown",
               id = "tab_prodbr",
               value = "tab_prodbr_val",
              
               column(width = 5,
                      style = "background-color: white",
                      plotOutput("usa_prodbr", width = 500, height = 550)),
               
               column(width = 7,
                      style = "background-color: white;",
                      plotOutput("state_prodbr", width = 500, height = 550))),
      
      tabPanel(title = "Heatmap", 
               id = "tab_heatmap",
               value = "tab_heatmap_val",
               plotOutput("heatmap", width = 900, height = 550)),
      
      tabPanel(title = "Spatial Analysis", 
               id = "tab_geogrcomp",
               value = "tab_geogrcomp_val",
               plotOutput("geogrcomp_comp", width = 1000, height = 500)),
      
      tabPanel(title = "Responses", 
               id = "tab_responses",
               value = "tab_responses_val",
               dataTableOutput("response_data"))
    )
  )
))
