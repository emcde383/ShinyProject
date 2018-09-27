library(treemapify)
library(tidyr)

usa_agg_prod <- read.csv("./data/output/usa_agg_prod.csv", header = TRUE)
state_agg_prod <- read.csv("./data/output/state_agg_prod.csv", header = TRUE)

shinyServer(function(input, output, session) {
  
  #make widgets responsive to tab selection, start:
  observeEvent(input$navbar, {
    toggle("tab_aggsum_sidebar",  condition = input$navbar == "tab_aggsum_val")
    toggle("tab_prodbr_sidebar",  condition = input$navbar == "tab_prodbr_val")
    toggle("tab_heatmap_sidebar", condition = input$navbar == "tab_heatmap_val")
    toggle("tab_geogrcomp_sidebar", condition = input$navbar == "tab_geogrcomp_val")
    toggle("tab_responses_sidebar", condition = input$navbar == "tab_responses_val")
    
  })
  #make widgets responsive to tab selection, end
  
  #make agg summary widget responsive to start year, start:
  observe({
    x <- input$syear
    
    updateSelectInput(session,
                      "fyear",
                      "Select End Year:",
                      choices = x:d_fyear,
                      selected = d_fyear)
  })
  #make agg summary widget responsive to start year, end
  
  ####_agg_sum_sidebar, start:
  #make data respond to user input
  select_function <- reactive({
    
    sample_agg_data %>% #t
      filter(year >= input$syear, year <= input$fyear) %>% #time filter
      filter(product %in% input$product) %>% #product filter
      group_by(product) %>%  
      ggplot(aes(x = year))  
  })
  
  observe({
    if (input$fill == 1) {
      
      output$agg_tseries <- renderPlot({
        
        p <- select_function() +
          geom_bar(aes(fill = product)) + 
          theme_bw() + 
          scale_fill_brewer(palette = "PuBuGn") +
          theme(plot.title = element_text(hjust = 0.5, size = 16),
                axis.title.x = element_text(size = 15),
                axis.title.y = element_text(size = 15),
                legend.position = "bottom",
                legend.text = element_text(size = 10),
                legend.title = element_blank(),
                panel.background = element_rect(fill = "transparent"),
                text = element_text(family = "Optima")) +
          labs(title = "Composition of Complaints Over Time",
               x = "Year",
               y = "Number of Complaints"
          )
        
        print(p)
        
      })
    
    } else {
      
      output$agg_tseries <- renderPlot({
        
        p <- select_function() +
          geom_bar(aes(fill = product), position = "fill") + 
          theme_bw() + 
          scale_fill_brewer(palette = "PuBuGn") +
          theme(plot.title = element_text(hjust = 0.5, size = 16),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                legend.position = "bottom",
                legend.text = element_text(size = 10),
                legend.title = element_blank(),
                text = element_text(family = "Optima")) + 
          labs(title = "Breakdown of Complaints Over Time",
               x = "Year",
               y = "Complaints (%)"
          )
        
        print(p)
        
      })    
    }
    
    })
  ####_aggsum_sidebar, end
  
  ####_prodbr_sidebar, start:
  #start product comparison
  output$usa_prodbr <- renderPlot({
    usa_agg_prod %>% 
      filter(year == input$prodbr_year) %>% 
      mutate(Untimely = untimely) %>% 
      ggplot(aes(area = count, label = product, fill = Untimely)) +
      geom_treemap() +
      geom_treemap_text(color = "black",
                        grow = TRUE,
                        fontface = "italic",
                        reflow = TRUE,
                        family = "Optima") +
      labs(title = "Distribution of Complaints Across Product Types") +
      scale_fill_distiller(palette = "Greens") +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            legend.title = element_text(vjust = 0.8, size = 12),
            text = element_text(family = "Optima")) +
      scale_fill_continuous(breaks = c(0.05, 0.10),
                            labels = c("5%", "10%"))
    
  })
  
  #needs to be reactive
  output$state_prodbr <- renderPlot({
    state_agg_prod %>% 
      left_join(state_to_name, by = "state") %>% 
      filter(year == input$prodbr_year, state_name == input$prodbr_state) %>% 
      mutate(Untimely = untimely) %>% 
      ggplot(aes(area = count, label = product, fill = Untimely)) +
      geom_treemap() +
      geom_treemap_text(color = "black",
                        grow = TRUE,
                        fontface = "italic",
                        reflow = TRUE,
                        family = "Optima") +
      labs(title = paste("Complaints Across Product Types - ", input$prodbr_state)) +
      scale_fill_distiller(palette = "Greens") +
      theme(plot.title = element_text(hjust = 0.5, size = 16),
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            legend.title = element_text(vjust = 0.8, size = 12),
            text = element_text(family = "Optima")) +
      scale_fill_continuous(breaks = c(0.05, 0.10),
                            labels = c("5%", "10%"))
  })
  ####_prodbr_sidebar, end
  
  ####_heatmap_sidebar, start:
  output$heatmap <- renderPlot({
    
    top_companies <- sample_agg_data %>%
      group_by(company) %>% 
      summarise(count = n()) %>% 
      top_n(as.numeric(input$compnum)) %>% 
      select(company) %>% 
      mutate(top_company = 1, company = as.character(company))
    
    t_hmap_data <- sample_agg_data %>% 
      group_by(company, state) %>% 
      summarise(count = n()) %>% 
      left_join(state_to_name, by = "state") %>% 
      left_join(top_companies, by = "company") %>% #ch
      filter(top_company == 1) %>% 
      select(company, state_name, count, region) %>% 
      filter(region != "NA", region %in% input$regionlist) %>% 
      select(-region) %>% 
      spread(state_name, count) 
    
    t_hmap_data[is.na(t_hmap_data)] <- 0
    
    colnames(t_hmap_data)[2]
    
    hmap_data <- t_hmap_data %>% 
      gather(key = "state_name", value = "count", colnames(t_hmap_data)[2:dim(t_hmap_data)[2]]) %>% 
      left_join(popdata, by = "state_name") %>% 
      filter(is.na(state_name) == FALSE) %>% 
      mutate(compl_per_capita = (count/pop_2010)*1000000)
    
    #clean up company names for chart
    hmap_data$company <- gsub("COMPANY", "", hmap_data$company)
    hmap_data$company <- gsub("NATIONAL ASSOCIATION", "", hmap_data$company)
    hmap_data$company <- gsub("DELAWARE", "", hmap_data$company)
    hmap_data$company <- gsub("CORPORATION", "", hmap_data$company)
    hmap_data$company <- gsub("N.A.", "", hmap_data$company)
    hmap_data$company <- gsub("INC.", "", hmap_data$company)
    hmap_data$company <- gsub("INC", "", hmap_data$company)
    hmap_data$company <- gsub("LLC", "", hmap_data$company)
    hmap_data$company <- gsub("L.P.", "", hmap_data$company)
    hmap_data$company <- gsub("US HOLDING", "", hmap_data$company)
    hmap_data$company <- gsub("USA HOLDINGS", "", hmap_data$company)
    hmap_data$company <- gsub("HOLDINGS,", "", hmap_data$company)
    hmap_data$company <- gsub("&", "", hmap_data$company)
    hmap_data$company <- gsub(",", "", hmap_data$company)
    hmap_data$company <- gsub("CO.", "", hmap_data$company)
    
    hmap_data$company = str_trim(hmap_data$company)
    
    hmap_data$company[hmap_data$company == "BARCLAYS BANK"] <- "BARCLAYS"
    hmap_data$company[hmap_data$company == "JPMORGAN CHASE"] <- "JP MORGAN CHASE"
    hmap_data$company[hmap_data$company == "PORTFOLIO REERY ASSOCIATES"] <- "PORTFOLIO RECOVERY ASSOCIATES" 
    hmap_data$company[hmap_data$company == "CITIZENS FINANCIAL GROUP"] <- "CITIZENS FINANCIAL"
    hmap_data$company[hmap_data$company == "ENE CAPITAL GROUP"] <- "ENE CAPITAL" 
    hmap_data$company[hmap_data$company == "EXPERIAN INFORMATION SOLUTIONS"] <- "EXPERIAN"
    hmap_data$company[hmap_data$company == "PAYPAL HOLDINGS"] <- "PAYPAL"
    hmap_data$company[hmap_data$company == "RESURGENT CAPITAL SERVICES"] <- "RESURGENT CAPITAL"
    hmap_data$company[hmap_data$company == "SUNTRUST BANKS"] <- "SUNTRUST" 
    hmap_data$company[hmap_data$company == "TRANSUNION INTERMEDIATE"] <- "TRANSUNION"
    hmap_data$company[hmap_data$company == "CAPITAL ONE FINANCIAL"] <- "CAPITAL ONE"
    hmap_data$company[hmap_data$company == "HSBC NORTH AMERICA HOLDINGS"] <- "HSBC"
    hmap_data$company[hmap_data$company == "NAVIENT SOLUTIONS ."] <- "NAVIENT SOLUTIONS"  
    hmap_data$company[hmap_data$company == "SADER SUMER"] <- "SANTANDER" 
    hmap_data$company[hmap_data$company == "U.S. BANP"] <- "U.S. BANCORP" 
    
    toproper <- function(x) {
      sapply(x, function(strn)
      { s <- strsplit(strn, "\\s")[[1]]
      paste0(toupper(substring(s, 1,1)), 
             tolower(substring(s, 2)),
             collapse=" ")}, USE.NAMES=FALSE)
    }
    hmap_data$company <- toproper(hmap_data$company)
    
    hmap_data$company[hmap_data$company == "Aes/pheaa"] <- "AES/PHEAA"
    hmap_data$company[hmap_data$company == "Erc"] <- "ERC"
    hmap_data$company[hmap_data$company == "Jp Morgan Chase"] <- "JP Morgan Chase"
    hmap_data$company[hmap_data$company == "Td Bank"] <- "TD Bank"
    hmap_data$company[hmap_data$company == "Bbt"] <- "BBT"
    hmap_data$company[hmap_data$company == "Ene Capital"] <- "ENE Capital"
    hmap_data$company[hmap_data$company == "Hsbc"] <- "HSBC"
    hmap_data$company[hmap_data$company == "Pnc Bank"] <- "PNC Bank"
    hmap_data$company[hmap_data$company == "U.s. Bancorp"] <- "U.S. Bancorp"
    
    ggplot(hmap_data, aes(x = company, y = state_name)) +
      geom_raster(aes(fill = compl_per_capita)) + 
      coord_flip() + 
      theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(vjust = -0.5, size = 16),
            axis.title.y = element_text(size = 16),
            plot.title = element_text(hjust = 0.5, size = 16),
            text = element_text(family = "Optima")) +
      labs(title = "Complaints Scaled by State Population",
           x = "Company",
           y = "State") +
      guides(fill=guide_legend(title="Complaint Density"))
  })
  ####_heatmap_sidebar, end
  
  ####_geogrcomp_sidebar, start:
  observe({
    
    if (input$geogrcomp_var == "Total complaints") {
      
      output$geogrcomp_comp <- renderPlot({
        
        t_agg <- spatial_comp_data %>% 
          mutate(group = "All Banks",
                 ln_dist = log(dist),
                 ln_count = log(count),
                 distcat =  cut(dist, 
                                breaks = quantile(dist, 
                                                  probs = seq(0, 1, by = 0.25)), 
                                labels = 1:4)) %>% 
          filter(distcat %in% 1:4) %>% 
          select(group, distcat, ln_count)
        
        t_com <- spatial_comp_data %>% 
          filter(company == input$geogrcomp_comp) %>%  
          mutate(group = input$geogrcomp_comp,        
                 ln_dist = log(dist),
                 ln_count = log(count),
                 distcat =  cut(dist, 
                                breaks = quantile(dist, 
                                                  probs = seq(0, 1, by = 0.25)), 
                                labels = 1:4)) %>% 
          filter(distcat %in% 1:4) %>% 
          select(group, distcat, ln_count)
        
        t_dat <- rbind(t_agg, t_com)
        
        t_dat %>%
          ggplot(aes(distcat, ln_count, color = group)) +
          geom_boxplot() +
          facet_wrap( ~ group) +
          theme_bw() +
          labs(title = "Complaints and Distance from Company HQ",
               x = "Distance Category",
               y = "Complaints (in natural log)",
               color = "Group") +
          theme(plot.title = element_text(hjust = 0.5, size = 16),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                legend.title = element_text(size = 16),
                legend.position = "bottom",
                legend.text = element_text(size = 14),
                axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 18),
                strip.text = element_text(size = 14),
                strip.background = element_rect(fill = "aquamarine4"),
                text = element_text(family = "Optima")) +
          scale_color_manual(values = c("gray25", "dodgerblue4"))
        
      })
      
    } else {
      
      output$geogrcomp_comp <- renderPlot({
        
        t_agg <- spatial_comp_data %>% 
          mutate(group = "All Banks",
                 ln_dist = log(dist),
                 distcat =  cut(dist, 
                                breaks = quantile(dist, 
                                                  probs = seq(0, 1, by = 0.2)), 
                                labels = 1:5)) %>% 
          filter(distcat %in% 1:5) %>% 
          select(group, distcat, untimely)
        
        t_com <- spatial_comp_data %>% 
          filter(company == input$geogrcomp_comp) %>%  
          mutate(group = input$geogrcomp_comp,         
                 ln_dist = log(dist),
                 distcat =  cut(dist, 
                                breaks = quantile(dist, 
                                                  probs = seq(0, 1, by = 0.2)), 
                                labels = 1:5)) %>% 
          filter(distcat %in% 1:5) %>% 
          select(group, distcat, untimely)
        
        t_dat <- rbind(t_agg, t_com)
        
        t_dat %>%
          ggplot(aes(distcat, untimely, color = group)) +
          geom_boxplot() +
          facet_wrap( ~ group) +
          theme_bw() +
          labs(title = "Complaint Response and Distance from Company HQ",
               x = "Distance Category",
               y = "% Not Resolved in Timely Manner",
               color = "Group") +
          theme(plot.title = element_text(hjust = 0.5, size = 16),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                legend.title = element_text(size = 16),
                legend.position = "bottom",
                legend.text = element_text(size = 14),
                axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 18),
                strip.text = element_text(size = 14),
                strip.background = element_rect(fill = "aquamarine4"),
                text = element_text(family = "Optima")) +
          scale_color_manual(values = c("gray25", "dodgerblue4"))
          
      })    
    }
  })
  ####_geogrcomp_sidebar, end

  ####_responses_sidebar, start:
  output$response_data <- renderDataTable({
    
   t <- agg_data %>%  
      mutate(response = company.response.to.consumer) %>% 
      group_by(state, year, response) %>% 
      select(state, year, response) %>% 
      summarise(count = n()) %>% 
      filter(state != "") %>% 
      left_join(popdata, by = "state") %>% 
      select(-state) %>% 
      filter(state_name != "", response != "Closed") %>% 
      select(State = state_name, Year = year, Response = response, Total = count) 
   
   t[, 2:dim(t)[2]]
    
  }, options = list(aLengthMenu = c(10, 20, 30), iDisplayLength =
                      10))
  ####_responses_sidebar, end

})