library(shinyWidgets)
library(ggforce)
library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(ggplot2)

library(sf)
library(spData)
library(tidyverse)
library(dplyr)
library(scales)
#library(lubridate)

library(ggrepel)
library(RColorBrewer)
library(forecast)
#library(plotly)

conn <- dbConnect(RSQLite::SQLite(), "DB_ShinyProject.sqlite")

# define elements for ui
header <- dashboardHeader(title = span(tagList(icon("education", lib = 'glyphicon')))
  )
df_field <- dbReadTable(conn, "tab_field_of_study")
df_region <- dbReadTable(conn, "tab_region")
df_total <- dbReadTable(conn, "tab_total")

list_field <- sort(unique(df_field$field_of_study))
list_region <- sort(unique(df_region$region))
#list_provider <- unique(df_field$provider_type)
minyear_seg <- min(df_field$year)

custom_theme <- theme(axis.text.x = element_text(size=11, color="grey12"),
                          axis.text.y = element_text(size=12, color="grey12"),
                          axis.title=element_text(size=12),
                      legend.text = element_text(size=11),
                      strip.text = element_text(size=12))
custom_y <- scale_y_continuous(labels = comma_format(big.mark = "."))
custom_x <- scale_x_continuous(breaks= pretty_breaks()) 
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#

siderbar <- dashboardSidebar(
  sidebarMenu(
    id = 'sidebar',
    style = "position: fixed; overflow: visible;", #fixed
    sliderInput(inputId = "year", label = "Year", min = 2003, max = 2022, value = c(2003, 2022), step = 1, width="230px",sep = ""),
    ## 1. Overall
    menuItem("Overall Dashboard", tabName = 'dashboard', icon = icon('dashboard'), badgeColor = "green"),
    ## 2. Fields
    menuItem("Field of Study", tabName = 'field', icon = icon('education', lib = 'glyphicon') ),
    ## 3. Region
    menuItem("Region", tabName = 'rg', icon = icon('globe')),
    #
    conditionalPanel("input.sidebar === 'rg'", 
                     checkboxInput("exclAuck", "Exclude Auckland", FALSE, width="230px")),
    ## 4. Setting
    menuItem("Custom Setting", tabName = 'st', icon = icon('cog')),
    #conditionalPanel("input.sidebar === 'st'", selectizeInput("select_provider", "Select provider type", choices =  list_provider, selected = "None",  width = "230px", multiple = T)),
    conditionalPanel("input.sidebar === 'st'", 
                     checkboxInput("colorblindfriendly", "Color-Blind Friendly", FALSE, width="230px")),
    ## 5. Information
    menuItem("Information", tabName = 'info', icon = icon('info-sign', lib = 'glyphicon'))
  )
)

body <- dashboardBody( 
  tabItems(
    ## 1 Main dashboard ----------------------------------------------------------
    tabItem( tabName = 'dashboard',
             ## contents for the dashboard tab
             h1(textOutput("md_year"), align='center'),
             fluidRow(
               valueBoxOutput("md_box_curr"),
               valueBoxOutput("md_box_increase"),
               valueBoxOutput("md_box_pct_online")
             ),
             fluidRow(
               column( width = 6,h4("Top 5 Fields and The rest", align = 'center'), 
                       plotOutput("md_pie_field")),
               column( width = 6,h4("Top 5 Region and The rest", align = 'center'), 
                       plotOutput("md_pie_region"))
             ),
             fluidRow(
               column( width = 6,h4("Provider Types", align = 'center'), 
                       plotOutput("md_pie_provider")),
               column( width = 6,h4("Provider Types", align = 'center'), 
                       plotOutput("md_col_yoy_provider"))
             ),
             h2(" .", align="center",style={'background-color:steelblue; color: steelblue;'}),

             h2(textOutput("md_year_trend"), align='center'),
             
             fluidRow( column( width = 2, checkboxInput("forecast", "Forecast Trend", FALSE), offset=4),
                       column( width = 3, checkboxInput("showfitted", "Show fitted values", FALSE))
             ),
             fluidRow(  column( width=4, sliderInput(inputId = "yearforecast", label = "Select number of years to forecast",
                                                     min = 5, max = 30, value = 5, step = 1), offset=4)
             ),
             
             fluidRow( column( width = 12,h4("Number of International Student over years", align = 'center'), 
                               plotOutput('md_col_num_all')) #plotlyOutput
             ),
             fluidRow( 
                       column( width = 12,h4("Comparing with Previous Year", align = 'center'), 
                               plotOutput("md_col_yoy_all"))
             ),
             
             h2(textOutput("md_seg_trend"), align='center'),
             fluidRow(
                       column( width = 6,h4("Distribution by Field over years", align = 'center'), 
                               plotOutput("md_col_field_dist")),
                       column( width = 6,h4("Distribution by Region over years", align = 'center'), 
                               plotOutput("md_col_region_dist"))
             ),
             p(textOutput("md_seg_note_color"), align='center', color="grey60", align = "center")
    ),
    ## 2 Field of study dashboard ----------------------------------------------------------
    tabItem( tabName = 'field',
             setBackgroundColor(color="ghostwhite"),
             h2(textOutput("fd_year_headline"), align="center"),
             fluidRow(
               column( width = 8,h4("Fields", align = 'center'), 
                       plotOutput("fd_col_single_year")),
               column( width = 4,h4("Compare with previous year", align = 'center'), 
                       plotOutput("fd_col_yoy"))
             ),
             h2(" .", align="center",style={'background-color:steelblue; color: steelblue;'}),
             h2("Fields Comparison", align="center"),
             fluidRow(
               column(10, selectizeInput("select_field",
                                                  "Select Fields to compare", 
                                                  choices =  list_field, 
                                                  selected = c("Architecture and Building", "Education"),  width = "200px",
                                                  multiple = T), offset = 5)
             ),
             fluidRow(
               column( width = 12,h4("Trend by Fields over years", align = 'center'), 
                       plotOutput("fd_line_compare_abs"))
             ),
             fluidRow(
               column( width = 12, height=24,h4("YoY by Fields over years", align = 'center'), 
                       plotOutput("fd_facet_compare_yoy")),
               column( width = 12,h4(textOutput("fd_year_provider_h"), align = 'center'), 
                       plotOutput("fd_facet_compare_provider"))
             )
             
    ),
    ## 3 region dashboard ----------------------------------------------------------
    tabItem( tabName = 'rg',
             h2(textOutput("rg_year_headline"), align="center"),
             fluidRow(
               column( width = 6,h4("Region", align = 'center'), 
                       plotOutput("rg_col_year")),
               column( width = 6,h4("Number of IS by Region", align = 'center'), 
                       plotOutput("rg_nzmap"))
             ),
             h2(" .", align="center",style={'background-color:steelblue; color: steelblue;'}),
             h2("Regions Comparison", align="center"),
             fluidRow(
               column(10, selectizeInput("select_region",
                                         "Select regions to compare", 
                                         choices =  list_region,
                                         selected = c("Wellington", "Waikato"),  width = "200px",
                                         multiple = T), offset = 5)
             ),
             fluidRow(
               column( width = 12,h4("Region", align = 'center'), 
                       plotOutput("rg_line_compare_abs")),
               column( width = 12,h4("YoY by Region over years", align = 'center'), 
                       plotOutput("rg_facet_compare_yoy")),
               column( width = 12,h4(textOutput("rg_year_provider_h"), align = 'center'), 
                       plotOutput("rg_facet_provider"))
             )
    ),
    ## 5 Information ----------------------------------------------------------
    tabItem( tabName = 'info',
             h3("Data Source"),
             a("https://catalogue.data.govt.nz/dataset/international-students-studying-in-new-zealand", 
               href="https://catalogue.data.govt.nz/dataset/international-students-studying-in-new-zealand",
               target="_blank"),
             h3("Definition and Information"),
             h4("Provider type:"),
             p("In the education system of New Zealand, a wānanga is a publicly-owned tertiary institution that provides education in a Māori cultural context."),
             p("A PTE may be a privately owned or publicly listed company, a trust, an incorporated society, or another such entity that offers post-school education or vocational training. 
               They offer all sorts of tertiary study and training, including industry training, professional qualifications, degrees and postgraduate study. 
               "),
             p("Te Pūkenga is New Zealand Institute of Skills and Technology, providing applied and vocational higher education and training experiences that are fit for the future"),
             p("There are 16 polytechnics and institutes of technology, 3 wānanga,
                8 universities, and more than 400 private training establishments in New Zealand"),
             h4("Region"),
             p("Students studying from offshore locations are included in the Extramural")
             
    )
  )
)

# UI
## put UI together --------------------
ui <- dashboardPage(title = "Tertiary Interational Student", header, siderbar, body )

server <- function(input, output){
  # set global year filter value
  formulaText_g <- reactive({
    max(input$year)
  })
  output$g_year <- renderText({
    formulaText_g()
  })
  
  # Reactive Labels for presentation headlines
  formulaText_year <- reactive({
    paste("Tertiary Internation Student in Year ", max(input$year))
  })
  output$md_year <- renderText({
    formulaText_year()
  })
  
  formulaText_year_trend <- reactive({
    paste("Overall trend from ", min(input$year), " to ", max(input$year))
  })
  output$md_year_trend <- renderText({
    formulaText_year_trend()
  })
  
  formulaText_seg_trend <- reactive({
    paste("Trend by Segment from ", max(min(input$year), minyear_seg), " to ", max(input$year))
  })
  output$md_seg_trend <- renderText({
    formulaText_seg_trend()
  })
  
  formulaText_seg_note_color <- reactive({
    ifelse(input$colorblindfriendly == "TRUE", "(Note: only 8 colors available, colors' order follows from top to bottom for both graph and legend accordingly)", "")
  })
  output$md_seg_note_color <- renderText({
    formulaText_seg_note_color()
  })
  
  #Dynamic headling for field tab
  formulaText_year_fd <- reactive({
    paste0("All Fields of Study in Year ", max(input$year))
  })
  output$fd_year_headline <- renderText({
    formulaText_year_fd()
  })
  
  # Dynamic headline for fields (provider pies)
  
  formulaText_provider_h <- reactive({
    paste("Provider Types by Field of Study in ", max(input$year))
  })
  output$fd_year_provider_h <- renderText({
    formulaText_provider_h()
  })
  
  # Headline for region tab
  formulaText_year_rg <- reactive({
    paste0("All Regions in Year ", max(input$year), ifelse(input$exclAuck =="TRUE", "(Exclude Auckland)", ""))
  })
  output$rg_year_headline <- renderText({
    formulaText_year_rg()
  })
  
  formulaText_compare_rg <- reactive({
    paste("Compare Regions in Year ", input$year)
  })
  output$rg_compare_headline <- renderText({
    formulaText_compare_rg()
  })
  
  
  # Dynamic headline for fields (provider pies)
  
  formulaText_rg_provider_h <- reactive({
    paste("Provider Types by Region in ", max(input$year))
  })
  output$rg_year_provider_h <- renderText({
    formulaText_rg_provider_h()
  })
  # ************************************************************************************************************************
  # Output list for main dashboard (prefix = md_)
  
  # Boxes
  
  output$md_box_curr <- renderValueBox({
    outcome <- df_total %>% filter(year == max(input$year), education_sector=="Tertiary education organisations") %>%
      group_by(year) %>% summarise(no_student = sum(number_ifp_student, na.rm=T))
    
    valueBox(format(outcome$no_student, big.mark = ','),
             subtitle = "Total Number",
             icon = icon('education', lib = 'glyphicon'),
             color = "light-blue"
    )
  })
  
  output$md_box_increase <- renderValueBox({
    outcome <- df_total %>% filter(year <= max(input$year), year >= max(input$year)-1, education_sector=="Tertiary education organisations") %>%
      group_by(year) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      mutate(no_student_prev_y = lag(no_student, order_by=year)) %>%
      mutate(rate_prev_y = round((no_student / no_student_prev_y - 1) * 100, 1)) %>%
      filter(year == max(input$year))
    
    valueBox(paste0(outcome$rate_prev_y, "%" ),
             subtitle = "YOY Rate",
             icon = icon(ifelse(outcome$rate_prev_y >0, 'export', "import"), lib = 'glyphicon'),
             color = ifelse(is.na(outcome$rate_prev_y), "black", ifelse(outcome$rate_prev_y >0, "green", "yellow"))
    )
  })
  
  output$md_box_pct_online <- renderValueBox({
    outcome <- df_region %>% filter(year == max(input$year), education_sector=="Tertiary education organisations") %>%
      group_by(year, region) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      group_by(year) %>% mutate(total = sum(no_student, na.rm=T)) %>% ungroup() %>%
      filter(region == "Extramural") %>% mutate(rt = round(no_student/total*100, 1))
    
    valueBox(paste0(format(outcome$rt, big.mark = ','), "%"),
             subtitle = "% Online",
             icon = icon('globe', lib = 'glyphicon'),
             color = "teal"
    )
  })
  
  # Pies
  
  output$md_pie_field <- renderPlot({
    agg <- df_field %>% filter(year == max(input$year)) %>%
      group_by(field_of_study) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      arrange(desc(no_student)) %>% mutate(rank = row_number()) %>%
      mutate(field = ifelse(rank > 5, "The rest", field_of_study)) %>%
      group_by(field) %>% summarise(ifp_student = sum(no_student, na.rm=T), mrank = min(rank)) %>% arrange(desc(mrank))
    
    sum_student <- sum(agg$ifp_student)
    agg$pct <- round(agg$ifp_student / sum_student*100, 0)
    
    pl <- ggplot(agg, aes(x = "", y = ifp_student, fill = fct_inorder(field))) +
      geom_col() +
      geom_text(aes(label = paste0(pct, "%")), 
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y")+
      labs(x = NULL, y = NULL, fill = NULL) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_void() +
      theme(legend.text = element_text(size=11))
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_fill_manual(values=cbPalette)
    } else {
      pl + scale_fill_brewer(palette = "Blues")
    }
  })
  
  output$md_pie_region <- renderPlot({
    agg <- df_region %>% filter(year == max(input$year), education_sector=="Tertiary education organisations") %>%
      group_by(region) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      arrange(desc(no_student)) %>% mutate(rank = row_number()) %>%
      mutate(region2 = ifelse(rank > 5, "The rest", region)) %>%
      group_by(region2) %>% summarise(ifp_student = sum(no_student, na.rm=T), mrank = min(rank)) %>% arrange(desc(mrank))
    
    sum_student <- sum(agg$ifp_student)
    agg$pct <- round(agg$ifp_student / sum_student*100, 0)
    
    pl <- ggplot(agg, aes(x = "", y = ifp_student, fill = fct_inorder(region2))) +
      geom_col() +
      geom_text(aes(label = paste0(pct, "%")), 
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y")+
      labs(x = NULL, y = NULL, fill = NULL) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_void() +
      theme(legend.text = element_text(size=11))
    
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_fill_manual(values=cbPalette)
    } else {
      pl + scale_fill_brewer(palette = "PuBu")
    }
  })
  
  output$md_pie_provider <- renderPlot({
    agg <- df_total %>% filter(year == max(input$year), education_sector=="Tertiary education organisations") %>%
      group_by(provider_type) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(desc(no_student))
    
    sum_student <- sum(agg$no_student)
    agg$pct <- round(agg$no_student / sum_student*100, 1)
    #agg <- filter(agg, pct >= 1)
    agg$pct_label <- ifelse(agg$pct < 1, "", paste0(agg$pct, "%"))
    
    agg$ymax <- cumsum(agg$pct)
    agg$ymin <- c(0, head(agg$ymax, n=-1))
    agg$labelPosition <- (agg$ymax + agg$ymin) / 2
    
    pl <- ggplot(agg, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=provider_type)) +
      geom_rect() +
      geom_text(x=3.5, aes(y=labelPosition, label=pct_label), size=4) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      guides(fill = guide_legend(title = "Provider")) +
      theme_void() +
      theme(legend.text = element_text(size=11))
    
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_fill_manual(values=cbPalette)
    } else {
      pl + scale_fill_brewer(palette = 4)
    }

  })
  
  output$md_col_yoy_provider <- renderPlot({
    agg <- df_total %>% filter(year <= input$year[2], education_sector=="Tertiary education organisations") %>%
      group_by(year, provider_type) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% 
      #filter(no_student > 0) %>% 
      arrange(provider_type, year) %>%
      group_by(provider_type) %>% mutate(no_student_prev_y = lag(no_student, order_by=year)) %>% ungroup() %>%
      filter(year == input$year[2]) %>%
      mutate(rate_prev_y = round((no_student / no_student_prev_y - 1) * 100, 3)) %>%
      mutate(pos = ifelse(rate_prev_y<0, "Descrease", "Increase"))
    print(agg)
    ggplot(agg, aes(x=provider_type, y=rate_prev_y, fill = pos)) +
      geom_col() +
      geom_text(
        label=paste0(round(agg$rate_prev_y, 1), "%"), 
        nudge_x = 0.25, nudge_y = 0.25, 
        check_overlap = T
      )+
      guides(fill=F) +
      theme_classic() +
      labs(x = "Provider Type", y="YOY %") + 
      custom_theme + custom_y + coord_flip()
  })
  
  # Bar graphs
  
  output$md_col_num_all <- renderPlot({ #renderPlotly
    agg <- df_total %>% filter(year >= input$year[1], year <= input$year[2], education_sector=="Tertiary education organisations") %>%
      group_by(year) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(year)
    
    if (input$forecast == "TRUE") {
      agg_ts <- ts(agg[2], frequency=1, start=input$year[1], end = input$year[2])
      arima_fit = auto.arima(agg_ts)
      arima_forecast = forecast(arima_fit, h = input$yearforecast)
      
      observed_data<- data.frame(cbind(year = start(arima_forecast$x)[1]:end(arima_forecast$x)[1], 
                                       val = arima_forecast$x,
                                       fitted = arima_forecast$fitted))
      
      forecast_data<- data.frame(cbind(year = start(arima_forecast$mean)[1]:end(arima_forecast$mean)[1], 
                                       val = arima_forecast$mean,
                                       lower80 = arima_forecast$lower[,1],
                                       upper80 = arima_forecast$upper[,1],
                                       lower95 = arima_forecast$lower[,2],
                                       upper95 = arima_forecast$upper[,2]))
      
      pl <- ggplot(observed_data, aes(x = year, y = val))
      
      if (input$showfitted == "TRUE") {
        pl <- pl + geom_point(aes(x=year, y=fitted), color="darkorange")
      }
      pl +
          geom_line(colour="steelblue")+
          geom_point(aes(x=year, y=val), color="steelblue") +
          geom_smooth(aes(x=year, y=val, ymax=upper95, ymin=lower95), 
                     colour='red', data=forecast_data, stat='identity') +
          geom_point(aes(x=year, y=val), color="red", data=forecast_data) +
          theme_classic()+
          guides(size=F)+
          labs(x = "Year", y="Number of Student") + 
          theme(axis.text.x = element_text(size=12, angle=60, hjust=1, color="grey12"),
                axis.text.y = element_text(size=12, color="grey12"),
                axis.title=element_text(size=12),
                plot.title = element_text(hjust = 0.5, size = 16)) +
          scale_y_continuous(labels = comma_format(big.mark = ".")) + 
        scale_x_continuous(breaks = start(arima_forecast$x)[1]:end(arima_forecast$mean)[1]) +
          theme(legend.position = "right")+
          ggtitle(paste0("And forecast in next ", input$yearforecast, " years with 95% confident interval"))
      
    } else {
      pl <- ggplot(agg, aes(x=(year), y=no_student)) +
        geom_point(aes(size = 1), color="steelblue") +
        geom_line(color="steelblue", size = 1) +
        theme_classic() +
        guides(size=F) +
        labs(x = "Year", y="Number of Student") + 
        custom_theme + custom_y + scale_x_continuous(breaks = input$year[1]:input$year[2])
      pl
      #ggplotly(pl)
      
    }
  })
  
  output$md_col_yoy_all <- renderPlot({
    agg <- df_total %>% filter(year >= input$year[1], year <= input$year[2], education_sector=="Tertiary education organisations") %>%
      group_by(year) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      mutate(no_student_prev_y = lag(no_student, order_by=year)) %>%
      mutate(rate_prev_y = round((no_student / no_student_prev_y - 1) * 100, 3)) %>%
      mutate(pos = ifelse(rate_prev_y<0, "Descrease", "Increase"))
    
    ggplot(agg, aes(x=factor(year), y=rate_prev_y, fill = pos)) +
      geom_col() +
      guides(fill=F) +
      theme_classic() +
      labs(x = "Year", y="YOY %") + 
      custom_theme + custom_y #+ scale_x_discrete()
  })
  
  output$md_col_field_dist <- renderPlot({
    agg <- df_field %>% filter(year >= input$year[1], year <= input$year[2]) %>%
      group_by(year, field_of_study) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(year, field_of_study)
    
    colourCount = length(unique(agg$field_of_study))
    getPalette = colorRampPalette(brewer.pal(9, "Set2"))
    
    pl<- ggplot(agg, aes(x=factor(year), y=no_student, fill=field_of_study)) +
      geom_bar(position = "fill", stat="identity") +
      theme_classic() +
      guides(fill = guide_legend(title = " ")) +
      labs(x = "Year", y="Proportion of Student ") + 
      theme(axis.text.x = element_text(size=12, angle=45, hjust=1, color="grey12"),
            axis.text.y = element_text(size=12, color="grey12"),
            axis.title=element_text(size=12)) +
      custom_y
    
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_fill_manual(values=rep(cbPalette, ceiling(colourCount/length(cbPalette))))
    } else {
      pl + scale_fill_manual(values=getPalette(colourCount))
    }
  })
  
  output$md_col_region_dist <- renderPlot({
    agg <- df_region %>% filter(year >= input$year[1], year <= input$year[2], education_sector=="Tertiary education organisations") %>%
      group_by(year, region) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(year, region)
    
    colourCount = length(unique(agg$region))
    getPalette = colorRampPalette(brewer.pal(9, "Set3"))
    
    pl <- ggplot(agg, aes(x=factor(year), y=no_student, fill=region)) +
      geom_bar(position = "fill", stat="identity") +
      theme_classic() +
      guides(fill = guide_legend(title = " ")) +
      labs(x = "Year", y="Proportion of Student ") + 
      theme(axis.text.x = element_text(size=12, angle=45, hjust=1, color="grey12"),
            axis.text.y = element_text(size=12, color="grey12"),
            axis.title=element_text(size=12)) +
      custom_y
    
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_fill_manual(values=rep(cbPalette, ceiling(colourCount/length(cbPalette))))
    } else {
      pl + scale_fill_manual(values=getPalette(colourCount))
    }
  })
  
  
  # ************************************************************************************************************************
  # Output list for field tab (prefix = _fd)
  
  output$fd_col_single_year <- renderPlot({
    agg <- df_field %>% filter(year == input$year[2]) %>%
      group_by(field_of_study) %>% summarise(no_student = sum(number_ifp_student, na.rm=T))
    
    ggplot(agg, aes(x=reorder(field_of_study, -no_student), y=no_student)) +
      geom_col(fill="steelblue") +
      theme_classic()+
      labs(x = "Field of Study", y="Number of Student")+
      custom_theme + 
      coord_flip()
  })
  
  output$fd_col_yoy <- renderPlot({
    agg <- df_field %>% filter(year >= input$year[2]-1, year <= input$year[2]) %>%
      group_by(year, field_of_study) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(field_of_study, year) %>%
      group_by(field_of_study) %>% mutate(no_student_prev_y = lag(no_student, order_by=year)) %>% ungroup() %>% 
      filter(year == max(input$year)) %>% mutate(rate_prev_y = round((no_student / no_student_prev_y - 1) * 100, 3)) %>%
      mutate(pos = ifelse(rate_prev_y<0, "Descrease", "Increase"))
    
    ggplot(agg, aes(x=reorder(field_of_study, -no_student))) +
      geom_col(aes(y=rate_prev_y, fill = pos))+
      theme_classic()+ 
      guides(fill = guide_legend(title = " ")) +
      labs(y="YOY rate", x = "")+
      theme(axis.text.x = element_text(size=11, color="grey12"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title=element_text(size=12)) +
      coord_flip()
  })
  
  output$fd_line_compare_abs <- renderPlot({
    
    agg <- df_field %>% filter(year >= input$year[1], year <= input$year[2]) %>%
      filter((field_of_study %in% input$select_field) | is.null(input$select_field)) %>% 
      #filter(!(field_of_study %in% input$select_field_excl) | is.null(input$select_field_excl)) %>%
      group_by(year, field_of_study) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(year, field_of_study)
    
    colourCount = length(unique(agg$field_of_study))
    getPalette = colorRampPalette(brewer.pal(9, "Paired"))
    
    pl<- ggplot(agg, aes(x=year, y=no_student, colour=field_of_study)) +
      geom_point(aes(size=0.5)) + 
      geom_line(size = 2) +
      theme_classic() +
      labs(x = "Year", y = "Number of International Student") +
      custom_theme + custom_y + scale_x_continuous(breaks = unique(agg$year)) + 
      guides(col = guide_legend(title = "Field of Study"), size=F)
    
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_color_manual(values=rep(cbPalette, ceiling(colourCount/length(cbPalette))))
    } else {
      pl + scale_color_manual(values=getPalette(colourCount))
    }
  })
  
  output$fd_facet_compare_yoy <- renderPlot({
    
    agg <- df_field %>% filter(year >= input$year[1], year <= input$year[2]) %>%
      filter((field_of_study %in% input$select_field) | is.null(input$select_field)) %>%
      #filter(!(field_of_study %in% input$select_field_excl) | is.null(input$select_field_excl)) %>%
      group_by(year, field_of_study) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(field_of_study, year) %>%
      group_by(field_of_study) %>% mutate(no_student_prev_y = lag(no_student, order_by=year)) %>% ungroup() %>%
      mutate(rate_prev_y = round((no_student / no_student_prev_y - 1) * 100, 3)) %>%
      mutate(pos = ifelse(rate_prev_y<0|is.na(rate_prev_y), "Descrease", "Increase"))
    
    ggplot(agg, aes(x=factor(year), y=rate_prev_y, fill = pos)) +
      geom_col(position="dodge") +
      theme_classic() +
      labs(x = "Year", y = "YOY") +
      custom_theme + custom_y + 
      guides(fill = guide_legend(title = " ")) +
      facet_wrap(~field_of_study)
  })
  
  output$fd_facet_compare_provider <- renderPlot({
    
    agg <- df_field %>% filter(year == input$year[2]) %>%
      filter((field_of_study %in% input$select_field) | is.null(input$select_field)) %>%
      #filter(!(field_of_study %in% input$select_field_excl) | is.null(input$select_field_excl)) %>%
      group_by(field_of_study, provider_type) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      group_by(field_of_study)%>% mutate(no_student_rg = sum(no_student, na.rm=T)) %>% ungroup() %>% 
      mutate(pct = round(no_student / no_student_rg*100, 1)) %>% filter(pct >= 1) %>% arrange(desc(pct))
    
    cp <- coord_polar(theta = "y")
    cp$is_free <- function() TRUE
    
    pl <- ggplot(agg, aes(x = factor(1), y = no_student, fill =factor(provider_type))) +
      geom_bar(stat="identity", width = 1) + 
      cp +
      geom_text(aes(label = paste0(pct, "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(x = NULL, y = NULL, fill = NULL) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_classic() + 
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            strip.text = element_text(size=12),
            legend.text = element_text(size=11),
            axis.line.x = element_blank(),
            axis.line.y = element_blank()) +
      facet_wrap(~field_of_study, scales = "free") +
      theme(aspect.ratio = 1)
    
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_color_manual(values=cbPalette)
    } else {
      pl + scale_fill_brewer(palette = "Pastel2")
    }
  })
  
  # ****************************************
  # Output list for region tab (prefix = _rg)
  
  output$rg_col_year <- renderPlot({
    
    agg <- df_region %>% filter(year == input$year[2], education_sector=="Tertiary education organisations") %>%
      filter((input$exclAuck =="TRUE" & !region %in% c("Auckland")) | input$exclAuck == "FALSE") %>%
      group_by(region) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(desc(no_student))
    
    colourCount = length(unique(agg$region))
    getPalette = colorRampPalette(brewer.pal(9, "Set1"))
    
    ggplot(agg, aes(x=reorder(region, -no_student), y=no_student)) +
      geom_col(fill="steelblue") +
      scale_fill_manual(values=getPalette(colourCount))+
      theme_classic() +
      labs(x = "Region", y = "Number of International Student") +
      theme(axis.text.x = element_text(size=12, angle=60, hjust=1, color="grey12"),
            axis.text.y = element_text(size=12, color="grey12"),
            axis.title=element_text(size=12)) +
      custom_y
  })
  
  output$rg_nzmap <- renderPlot({
    
    val <- df_region %>% filter(year == input$year[2], education_sector=="Tertiary education organisations") %>%
      filter((input$exclAuck =="TRUE" & !region %in% c("Auckland")) | input$exclAuck == "FALSE") %>%
      group_by(region) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      mutate(region = tolower(gsub(" ", "_" , region)))
    
    nz$region <- gsub(" ", "_", gsub("'", "", tolower(nz$Name)))
    nz1 <- left_join(nz, val, by="region")
    
    ggplot(nz1) + 
      geom_sf(aes(fill = no_student)) +
      theme_void() +
      scale_fill_distiller(name = "Number of Student", palette = "Spectral")+ 
      geom_sf_text(data = nz1, aes(label = Name), fun.geometry = sf::st_centroid, size=3.5, col="grey20")
    })
  
  output$rg_line_compare_abs <- renderPlot({
    
    agg <- df_region %>% filter(year >= input$year[1], year <= input$year[2], education_sector=="Tertiary education organisations") %>%
      filter((input$exclAuck =="TRUE" & !region %in% c("Auckland")) | input$exclAuck == "FALSE") %>%
      group_by(year, region) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      filter((region %in% input$select_region) | is.null(input$select_region)) %>% arrange(year)
    
    colourCount = length(unique(agg$region))
    getPalette = colorRampPalette(brewer.pal(9, "Paired"))
    
    pl <- ggplot(agg, aes(x=year, y=no_student, col=region)) +
      geom_point(aes(size=0.5)) + 
      geom_line(size = 2) +
      theme_classic() +
      guides(col = guide_legend(title = "Region"), size=F) +
      labs(x = "Year", y = "Number of International Student") +
      custom_theme + custom_y + scale_x_continuous(breaks = unique(agg$year)) 
    
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_color_manual(values=rep(cbPalette, ceiling(colourCount/length(cbPalette))))
    } else {
      pl + scale_color_manual(values=getPalette(colourCount))
    }
  })
  
  output$rg_facet_compare_yoy <- renderPlot({
    
    agg <- df_region %>% filter(year >= input$year[1], year <= input$year[2], education_sector=="Tertiary education organisations") %>%
      filter((input$exclAuck =="TRUE" & !region %in% c("Auckland")) | input$exclAuck == "FALSE") %>%
      filter((region %in% input$select_region) | is.null(input$select_region)) %>%
      group_by(year, region) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>% arrange(region, year) %>%
      group_by(region) %>% mutate(no_student_prev_y = lag(no_student, order_by=year)) %>% ungroup() %>%
      mutate(rate_prev_y = round((no_student / no_student_prev_y - 1) * 100, 3)) %>%
      mutate(pos = ifelse(rate_prev_y<0|is.na(rate_prev_y), "Descrease", "Increase"))
    
    ggplot(agg, aes(x=factor(year), y=rate_prev_y, fill = pos)) +
      geom_col(position="dodge") +
      theme_classic()+
      labs(x = "Year", y = "YOY") +
      custom_theme + custom_y +
      guides(fill = guide_legend(title = " ")) +
      facet_wrap(~region)
  })
  
  output$rg_facet_provider <- renderPlot({
    
    agg <- df_region %>% filter(year == input$year[2], education_sector=="Tertiary education organisations") %>%
      filter((input$exclAuck =="TRUE" & !region %in% c("Auckland")) | input$exclAuck == "FALSE") %>%
      filter((region %in% input$select_region) | is.null(input$select_region)) %>%
      group_by(region, provider_type) %>% summarise(no_student = sum(number_ifp_student, na.rm=T)) %>%
      group_by(region)%>% mutate(no_student_rg = sum(no_student, na.rm=T)) %>% ungroup() %>% 
      mutate(pct = round(no_student / no_student_rg*100, 1)) %>% filter(pct >= 1) %>% arrange(desc(pct))
    
    cp <- coord_polar(theta = "y")
    cp$is_free <- function() TRUE
    
    pl <- ggplot(agg, aes(x = factor(1), y = no_student, fill =factor(provider_type))) +
      geom_bar(stat="identity", width = 1) + 
      cp +
      geom_text(aes(label = paste0(pct, "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(x = NULL, y = NULL, fill = NULL) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme_classic() + 
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            strip.text = element_text(size=12),
            legend.text = element_text(size=11),
            axis.line.x = element_blank(),
            axis.line.y = element_blank()) +
      facet_wrap(~region, scales = "free") +
      theme(aspect.ratio = 1)
    
    if (input$colorblindfriendly == "TRUE") {
      pl + scale_color_manual(values=cbPalette)
    } else {
      pl + scale_fill_brewer(palette = "Pastel2")
    }
  })
  
}

shinyApp(ui, server)

#ui
# numericInput()
# textInput()
# selectInput()
# sliderInput()
# checkboxInput()
# actionButton()
# main param: inputId, label, value, step, width

