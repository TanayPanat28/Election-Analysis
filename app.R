
alliance_seats = alliance_data %>% 
  group_by(Alliance,Year) %>% 
  mutate(seats_by_alliance = sum(Won))

top5_parties <- alliance_seats %>%
  group_by(Year, Alliance) %>%
  top_n(5, wt = Won) %>%
  ungroup()

alliance_dist <- top5_parties %>% 
  group_by(Alliance, Year) %>% 
  mutate(Percentage =  round((Won / seats_by_alliance * 100),2))

NDA <- subset(alliance_dist, Alliance == "NDA")
UPA <- subset(alliance_dist, Alliance == "UPA")
THIRD <- subset(alliance_dist, Alliance == "THIRD FRONT")



ui <- navbarPage(
  theme = shinythemes::shinytheme('cosmo'),
  title = "",
  tabPanel(
    title = "Home Page",
    fluidPage(
      fluidRow(
        column(
          width = 12, h1("ANALYSIS OF ELECTIONS IN WORLD'S LARGEST DEMOCRACY"), style = "text-align:center;"
        ),
        
      ),
      tags$img(src = "Parliament.jpg", style = "width:100%; height:625px; border: #000000 solid"),
    ),
  ),
  tabPanel(
    title = "Introduction",
    fluidPage(
      fluidRow(
        column(
          width = 12, h1("Lok Sabha Elections"), style = "text-align:center;"
        ),
        column(
          width = 6,
          tags$img(src = "Lok_Sabha.jpg", style = "width:100%; height:625px; border: #000000 solid")
        ),
        column(
          width = 6,
          
          style = ("
             font-size: 20px;
             font-family: verdana;
             outline-style: inset;
             padding 10px 10px 10px 10px;
             background-color: white;
             opacity: 0.6;
             color: black;
             font-weight: bold;
             border-radius: 6px;
             "),
          HTML("The Lok Sabha is composed of representatives of people chosen by direct election on the basis of Universal Adult Suffrage. 
               The Constitution of India allows for a maximum of 550 members in the House, with 530 members representing the States and 20 representing the Union Territories. <br><br>
               At present, the Lok Sabha has 543 seats filled by elected representatives. The term of the Lok Sabha, unless dissolved, is five years from the date appointed for its first meeting. <br><br>
               Indian general elections are the largest election in the world as around 1 Billion people cast their votes via Electronic Voting Machine. <br><br>
               As a result of the Lok Sabha elections people of India choose its Prime Minister.
               Each seat in the house represents a constituency, candidate who are elected then form the Parliament.<br><br>
               During the elections, members from 6 National parties and 54 state parties compete against each other to get elected in the parliament.")
        )
      )
    )
  ),
  
  tabPanel(
    setBackgroundColor(
      color = c("#FFDBAC"),
    ),
    title = "General Analysis",
    fluidPage(
      tags$style(HTML("
                      .my-plot-box {
                      border: 2px solid black;
                      padding: 5px;
                      }
                    ")),

      fluidRow(
        column(
          width = 1,
          selectInput("Year","Year", unique(image_data$Year), selected = 2019)
        ),
        column(

          width = 6,
          
          div(uiOutput("image_display",height = 320), class = "my-plot-box")
          
        ),
        column(

          width = 4,
          div(plotlyOutput("plt2", height = 320), class = "my-plot-box")
      )

        
      ) ,
      br(),
      fluidRow(
        column(
          offset = 1,
          width = 6,
          div(plotlyOutput("plt3", height = 320), class = "my-plot-box")
        ),
          column(
            width = 4,
            div(plotlyOutput("plt4", height = 320), class = "my-plot-box")
          )
        )
      )
    
    
  ),
  tabPanel(
    title = "Party Information",
    fluidPage(
      fluidRow(
        column(
          12,
          style = ("
              font-size: 20px;
             font-family: verdana;
             outline-style: inset;
             padding 10px 10px 10px 10px;
             background-color: white;
             opacity: 0.6;
             color: black;
             font-weight: bold;
             border-radius: 6px;
          "),
          
          HTML('<u>Biggest National Parties in India:</u>'), 
          br(),br(),
          strong("Bhartiya Janta Party (BJP)"),
          br(),
          tags$img(src = "bjp.png", height = "100", width = "150"),
          br(),
          strong("BJP is the world's largest political party. It is one of the two largest political parties in India along side Indian National Congress.
                 It was founded in 1980, in its short life of 43 years BJP has formed government on 3 occasions. 
                 In 2014 Indian General Elections BJP won 282 seats, this was the first instance since 1984 of a single party achieving an outright majority in the Indian Parliament and the first time that it achieved a majority in the Lok Sabha on its own strength.
                 Under the administration of BJP India is slowly becoming world leader, rising to 5th largest economy in the world."),
          br(),
          br(),
          br(),
          strong("Indian National Congress (INC)"),
          br(), 
          tags$img(src = "INC.png", height = "100", width = "150"),
          br(),
          strong("Founded in 1885, it was the first modern nationalist movement to emerge in the British Empire in Asia and Africa.
                 It is a 'big tent' party whose platform is generally considered to lie in the centre to centre-left of Indian politics. It has formed the government 
                 10 times since its existance, but in the recent times due to its corrupt politicians has seen a downfall. In the last 2 elections they haven't even managed to
                 secure 60 seats.")
        )
      )
    )
  ),
  tabPanel(title = "Partywise Analysis",
           fluidPage(
             tags$style(HTML("
                      .my-plot-box {
                      border: 2px solid black;
                      padding: 5px;
                      }
                    ")),
             fluidRow(
               column(
                 offset = 1,
                 width = 5,
                 div(plotOutput("plt7"),class = "my-plot-box")
                 
               ),
               column(
                      
                      width = 5, 
                      div(plotlyOutput("plt8"), class = "my-plot-box")
                          
              )
             ),
             br(),
             fluidRow(
               style = ("
              font-size: 20px;
             font-family: verdana;
             outline-style: inset;
             padding 10px 10px 10px 10px;
             background-color: white;
             opacity: 0.6;
             color: black;
             font-weight: bold;
             border-radius: 6px;
          "),
          column(
              width = 12,
              HTML("As highlighted by the Red box, INC achieved a historically low votes tally of 44 seats in 2014 Lok-Sabha Election.<br>
                   This was due to a slew of massive corruption scandals involving public funds and resources battered the Congress-led coalition.<br>
                   India's once-impressive economic growth, averaging above 8 per cent over 2002-2010, 
                   slowed to below 5 per cent 2013 while inflation rocketed into double digits.<br>
                   <br>
                   After the victory in 2014 BJP strengthened its position by doing infrastructural and technological development in the country,
                   as a result of the development in 2019 BJP won with a record mandate of 37.70 percentage of votes. 
                  ")
          )
          )
           )
           ),
  tabPanel(
    title = "Statewise Seat Distribution",
    fluidPage(
      tags$style(HTML("
                      .my-plot-box {
                      border: 2px solid black;
                      padding: 5px;
                      }
                    ")),
      fluidRow(
        column(
          width = 8,
          div(dataTableOutput("table"), class = "my-plot-box")
        ),
        column(
          width = 4,
          style = ("
              font-size: 20px;
             font-family: verdana;
             outline-style: inset;
             padding 10px 10px 10px 10px;
             background-color: white;
             opacity: 0.6;
             color: black;
             font-weight: bold;
             border-radius: 6px;
          "),
          HTML("1) Uttar Pradesh has the highest population density in India, becuase of this reason 80 members of parliament are elected from UP.<br> <br>
                2) 6 constituencies are represented by Scheduled Tribe representatives, which is the highest among all the states.<br><br>
                3) 7 members of parliament are selected form the capital city of Delhi. <br><br>
                4) West Bengal has the highest percentage of Scheduled Caste members of parliament representations, almost 25% representatives from this state belong to Scheduled Caste.")
        )
      )
    )
  ),
  tabPanel(title = "Statewise Analysis",
           fluidPage(
             tags$style(HTML("
                      .my-plot-box {
                      border: 2px solid black;
                      padding: 5px;
                      }
                    ")),
             
             fluidRow(
               column(
                 width = 2,
                 selectInput("State","State", unique(state_result$State), selected = "Maharashtra")
               ),
               column(
                 width = 4,
                 div(highchartOutput("plt5"), class = "my-plot-box")
               ),
               column(
                 width = 4,
                 div(plotlyOutput("plt6"), class = "my-plot-box")
               )
             ),
             br(),
             
             fluidRow(
               style = ("
                              font-size: 20px;
                              font-family: verdana;
                              outline-style: inset;
                              padding 10px 10px 10px 10px;
                              background-color: white;
                              opacity: 0.6;
                              color: black;
                              font-weight: bold;
                              border-radius: 6px;
          "),
               column(
                 12,
                    HTML("BJP Strong Hold States: <br>
                           In the last two general elections BJP has won more than 90% of seats in Gujarat, Rajasthan, Delhi and Madhya Pradesh.<br><br>
                         Congress Strong Hold States: <br>
                         In the last 2 general elections INC has won more that 60% seats in Kerala and gained momentum in states like Punjab and made important 
                         coalition with DMK in Tamil Nadu.<br><br>
                         Swing States: <br>
                         Historically Uttar Pradesh,Karnataka, Assam, Harayana and Maharashtra have been the swing states in Indian Politics.<br>
                           ")
               )
             )
           )
           
           ),
  tabPanel(title = "Alliance-Wise Analysis",
           
           fluidPage(
             tags$style(HTML("
                      .my-plot-box {
                      border: 2px solid black;
                      padding: 5px;
                      }
                    ")),
             
             fluidRow(
               column(
                 offset = 2,
                 width = 7,
                 div(plotlyOutput("linechart", height = 320), class = "my-plot-box")
               ),
               column(
                 width = 2,
                 strong("Please click the points on the linechart.")
               )
                  ),
             br(),
             fluidRow(
               column(
                 offset = 3,
                 width = 5,
                 div(plotlyOutput("dchart", height = 320), class = "my-plot-box")
               )
             )
           
           )
        ),
  tabPanel( title = "Chloropeth Map",
            fluidPage(
              tags$style(HTML("
                      .my-plot-box {
                      border: 2px solid black;
                      padding: 5px;
                      }
                    ")),
              fluidRow(
                column(
                  width = 2,
                  selectInput("year_map","Year", c(2014,2019), selected = 2014)
                ),
                column(
                  width = 8,
                  div(withSpinner(plotlyOutput("chart_india_1419",height = 500)), class = "my-plot-box"),
                  strong("Loading graph may require 2 minutes, thankyou for waiting."), style = "text-align:center;"
                )
              )
    
            )
        ),
  
  tabPanel(
    title = "Results",
    tags$style(HTML("
                      .my-plot-box {
                      border: 2px solid black;
                      padding: 5px;
                      }
                    ")),
    fluidPage(
      fluidRow(
        column(
          width = 12,
          div(dataTableOutput("results_table"), class = "my-plot-box")
        )
      )
    )
  ),
  tabPanel(
    title = "Refrences",
    fluidPage( 
      h1("Reference Links"), style = "text-align:center;",
      fluidRow(
        
        
        style = ("
              font-size: 20px;
             font-family: verdana;
             outline-style: inset;
             padding 10px 10px 10px 10px;
             background-color: white;
             opacity: 0.6;
             color: black;
             font-weight: bold;
             border-radius: 6px;
                       "),
        column(
          width = 12,
          tags$a(href = "https://www.reuters.com/graphics/INDIA-ELECTION-RESULTS/010092FN33P/index.html", 
                 "https://www.reuters.com/graphics/INDIA-ELECTION-RESULTS/010092FN33P/index.html"),br(),br(),
          tags$a(href = "https://medium.com/analytics-vidhya/election-visualization-with-r-8b790250d889",
                 "https://medium.com/analytics-vidhya/election-visualization-with-r-8b790250d889"),br(),br(),
          tags$a(href = "https://www.nytimes.com/interactive/2019/12/13/world/europe/uk-general-election-results.html",
                 "https://www.nytimes.com/interactive/2019/12/13/world/europe/uk-general-election-results.html"),br(),br(),
          tags$a(href = "https://www.indiavotes.com/",
                 "https://www.indiavotes.com/"),br(),br(),
          tags$a(href = "https://www.bjp.org/",
                 "https://www.bjp.org/"),br(),br(),
          tags$a(href = "https://www.inc.in/",
                 "https://www.inc.in/"),br(),
          tags$a(href = "https://eci.gov.in",
                 "https://eci.gov.in"),br(),
          tags$a(href = "https://en.wikipedia.org/wiki/Lok_Sabha",
                 "https://en.wikipedia.org/wiki/Lok_Sabha")
          
        )
      )
      
  )

  )
)

# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$linechart <- renderPlotly(
    {
      
      lp <- ggplot(data = top5_parties, aes(x = Year, y = seats_by_alliance, group = Alliance,
                                            text = paste("Year:",Year,
                                                         "\nSeats Won:", seats_by_alliance))) +
        geom_line(aes(color = Alliance)) +
        geom_point(aes(color = Alliance),size = 3) +
        labs(x = "Year", y = "Seats by Alliance") +
        theme_clean()
      
      color_palette <- c("darkorange", "green", "blue")
      lp <- lp + scale_color_manual(values = color_palette, labels = c("NDA", "UPA", "Third Front"))
      
      pl <- ggplotly(lp, tooltip = c("text","Party"),source = "linechart") %>% layout(plot_bgcolor='transparent', paper_bgcolor='transparent')
      
      pl %>% event_register("plotly_click") 
      
    }
  )
  
  
  output$dchart <- renderPlotly({
    
    c1 <- event_data("plotly_click", source = "linechart")
    
    if(is.null(c1)) return(NULL)
    
    y <- c1$x
    
    type <- c1$curveNumber
    
    df4 <- if(type == 3) {NDA} else if(type == 5) {UPA} else {THIRD}
    
    df4 <- df4 %>% 
      filter(Year == y)
    
    
    plot_ly(df4, values = ~Percentage, labels = ~Party,
            hoverinfo = "text",
            hovertext = paste("Party:",df4$Party,
                              "\nSeats:",df4$Won)) %>%
      add_pie(hole = 0.6) %>%
      layout(
        title = "Alliance Analysis",
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ) %>% layout(plot_bgcolor='transparent', paper_bgcolor='transparent')
  })
  
  filtered <- reactive({
      final.data_1419 %>%
      filter(Year == input$year_map)
  }
  )
  
  output$chart_india_1419 <- renderPlotly(
    {
     Sys.sleep(1.5) 
      p <- ggplot() +
        geom_polygon(data = filtered(),aes(x = long, y = lat, group = group,
                                                fill = Party, color = id,text = paste("Constituiency:",id,
                                                                                      "\nWinning Candidate Name:",Winning_Candidate,
                                                                                      "\nVotes:",Votes)),
                     color = "black",linewidth = 0.25) +
        coord_map() +
        xlab('Longitude')+
        ylab('Latitude') +
        labs(title = "Indian Elections Results") +
        scale_fill_manual(name = 'id', values = cols_2019) +
        theme(legend.title = element_blank())
      
      plotly::ggplotly(p, tooltip = "text") %>% layout(plot_bgcolor='transparent', paper_bgcolor='transparent')
    }
    
  )
  

  filtered_data_3 <- reactive({
    image_data %>%
      
      filter(Year == input$Year)
  }
  )
  filtered_data_5 <- reactive({
    vote_share %>%
      
      filter(Year == input$Year)
  }
  )
  output$image_display <- renderUI({
    image_path <- filtered_data_3()$Image_Source
    image_tag <- tags$img(src = image_path,height = "330px", width = "710px")
    image_tag
  })
  
  output$plt2 <- renderPlotly(
    {
      plot_ly(filtered_data_5(), values = ~Vote_perc, labels = ~Party,
              hoverinfo = "text",
              hovertext = paste("Party:",filtered_data_5()$Party),
              marker = list(colors = cols_2019)) %>%
        add_pie(hole = 0.6) %>%
        layout(
          title = "Partywise Voteshare",
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        ) %>% layout(plot_bgcolor='transparent', paper_bgcolor='transparent')
    }
  )
  
  filtered_data_6 <- reactive({
    category_wise %>%
      
      filter(Year == input$Year)
  }
  )
  
  
  output$plt3 <- renderPlotly(
    {
      pn <- ggplot(filtered_data_6(),aes(x = Type, y = n,  fill = Type,
                                         text = paste("Category:", Type,
                                                      "\nSeats Represented:",n))) +
        geom_col(show.legend = FALSE) + 
        labs(x = "Year", y = "Seats Represented", title = "Number of seats won by Category") +
        scale_fill_manual(name = "Category",
                          values = c("GEN" = "darkgreen", "SC" = "green", "ST" = "lightgreen"),
                          labels = c("GEN" = "General", "SC" = "Scheduled Castes", "ST" = "Scheduled Tribes"))+
        ylim(c(0,420)) +
        theme_classic()
      
      plotly::ggplotly(pn, tooltip = "text") %>% layout(plot_bgcolor='transparent', paper_bgcolor='transparent')
    }
    
  )
  
  
  filtered_data_4 <- reactive({
    long_df2 %>%
      filter(Year == input$Year)
  }
  )
  
  output$plt4 <- renderPlotly({
    custom_colors <- c("purple", "blue")
    plot_ly(data = filtered_data_4(), labels = ~Sex, values = ~Percentage, type = "pie",
            textinfo = "label+value", insidetextfont = list(color = "#FFFFFF"),
            hovertemplate = "Count: %{text}",
            text = ~Value,
            marker = list(colors = custom_colors)) %>% 
      layout(title = "Percentage Distribution by Sex",
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = TRUE, plot_bgcolor='transparent', paper_bgcolor='transparent'
            )
             
  })
  
  filtered_data_7 <- reactive({
    state_result %>% 
      filter(State == input$State)
  })
  

  output$plt5 <- renderHighchart({
    doutplt <- data_to_hierarchical(filtered_data_7(), c(Year, Party),n)
    
    plthc <- highchart() %>% 
      hc_chart(type = "sunburst",backgroundColor = "#FFDBAC") %>% 
      hc_title(text = "Statewise Results over 6 Elections") %>% 
      hc_add_series(name = "pop",data = doutplt) %>% 
      hc_add_theme(hc_theme_bloom())
    plthc
  })
  
  output$plt6 <- renderPlotly({
    ggplotly(
      ggplot(filtered_data_7(),aes(x = Year, y = Avg_Turnout)) +
        geom_point() +
        geom_line() +
        scale_x_continuous(breaks = unique(state_result$Year)) +
        ylim(c(30,90)) +
        labs(x = "Year", y = "Turnout %", title = "Statewise Turnout") +
        theme_clean()
    ) %>% layout(plot_bgcolor='transparent', paper_bgcolor='transparent')
  })
  
  output$plt7 <- renderPlot({
      p6 <- ggplot(partiwise_result, aes(x = Year, y = n, color = Party)) +
        geom_ribbon(aes(ymin = 0, ymax = n, fill = Party, )
                    , alpha = 0.4) +
        geom_line(aes(group = Party), size = 1, color = "black") +
        geom_point(size = 2, color = "black") +
        scale_fill_manual(values = c("darkorange", "blue")) +
        scale_shape_manual(values = c("BJP" = 17, "INC" = 16)) +
        ylim(c(0,320)) +
        geom_text(aes(label = n), vjust = -1, color = "black") +
        scale_x_continuous(breaks = unique(partiwise_result$Year)) +
        labs(title = "BJP vs INC - Election Results",
             x = "Year",
             y = "Count",
             fill = "Party",
             color = "Party") +
        theme_classic() +
        theme( panel.background = element_rect(fill = "#FFDBAC", color = "#FFDBAC"),
               plot.background = element_rect(fill = "#FFDBAC", color = "#FFDBAC"))
              
      p6 + geom_rect(xmin = 2012, xmax = 2016, ymin = 30, ymax = 310, color = "red", alpha = 0)
      
  })
  
  output$table <- renderDataTable({
    datatable(state_seat_data, rownames = FALSE)
  })
  
  output$plt8 <- renderPlotly({
    ggplotly(
      ggplot(vs_Party, aes(x = Year, y = Vote_perc, fill = Party)) +
        geom_col(position = "dodge") +
        scale_x_continuous(breaks = unique(vs_Party$Year)) +
        scale_fill_manual(values = c("BJP" = "orange", "INC" = "blue")) +
        theme_classic() +
        labs(x = "Year", y = "Vote Percentage", title = "BJP vs INC - Vote Share")
    ) %>% layout(plot_bgcolor='transparent', paper_bgcolor='transparent')
  })
  
  results_data <- data.frame(
    Party = c('<img src = "inc.png" height="50" width="50"> <br> INC',
              '<img src = "bjp.png" height="50" width="50"> <br> BJP',
              '<img src = "inc.png" height="50" width="50"> <br> INC',
              '<img src = "inc.png" height="50" width="50"> <br> INC',
              '<img src = "bjp.png" height="50" width="50"> <br> BJP',
              '<img src = "bjp.png" height="50" width="50"> <br> BJP'),
    Year = c(1996,1999,2004,2009,2014, 2019),
    Legislature = c(12,13,14,15,16, 17),
    Prime_minister = c('<img src = "deve.png" height="50" width="50"> <br> H. D. Deve Gowda', 
                       '<img src = "Atal.png" height="50" width="50"> <br> Atal Bihari Vajpayee',
                       '<img src = "manmohan.png" height="50" width="50"> <br> Dr. Manmohan Singh',
                       '<img src = "manmohan.png" height="50" width="50"> <br> Dr. Manmohan Singh',
                       '<img src = "Modi.png" height="50" width="50"> <br> Narendra Damodardas Modi',
                       '<img src = "Modi.png" height="50" width="50"> <br> Narendra Damodardas Modi'
                       ),
    Seats_won_by_winning_alliance = c(273, 299, 225, 262, 336, 351),
    Perc_of_Votes_Winners = c("28.8%", "23.8%", "26.5%","28.5%", "31.3%", "37.7%" ),
    Opposition_Leader = c('<img src = "Atal.png" height="50" width="50"> <br> Atal Bihari Vajpayee',
                          '<img src = "sonia.png" height="50" width="50"> <br> Sonia Gandhi',
                          '<img src = "lk_advani.png" height="50" width="50"> <br> LK Advani',
                          '<img src = "lk_advani.png" height="50" width="50"> <br> LK Advani',
                          '<img src = "rahul.png" height="50" width="50"> <br> Rahul Gandhi',
                          '<img src = "rahul.png" height="50" width="50"> <br> Rahul Gandhi'),
    Seats_won_by_opposition = c(187,139,189,159,59,90),
    Perc_of_Votes_Opposition = c("20.3%","28.3%","22.2%","18.8%","19.5%","19.7%")
  )
  
  output$results_table <- renderDataTable({
    DT::datatable(results_data, escape = FALSE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
