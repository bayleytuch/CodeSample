## BAYLEY TUCH SAMPLES

library(pander)
library(dplyr)
library(ggplot2)
library(stringr)
library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(plotly)
library(shiny)
library(rio)
library(dplyr)
library(tidyr)
require(weights)
library(plotly)

server <- function(input, output) {
  ################
  ###    MAP   ###
  ################
  
  risk <- read.csv("philly-youth-risk.csv")
  
  #reading  in the shapefile
  philly <-
    readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")
  philly$CODE <- as.integer(as.character(philly$CODE))
  
  # using a left_join because
  philly@data <- left_join(philly@data, risk, by = "CODE")
  
  # create popup
  state_popup <- paste0(
    "<strong>Zip Code: </strong>",
    philly$CODE,
    "<br><br><strong>Risk Index: </strong>",
    philly$Risk,
    "<br><strong>Poverty: </strong>",
    philly$Poverty,
    "%",
    "<br><strong>Education: </strong>",
    philly$Education,
    "%",
    "<br><strong>Unemployment: </strong>",
    philly$Unemployment,
    "%",
    "<br><strong>Crime: </strong>",
    philly$Crime,
    " shootings<br>per 10,000 people",
    "<br><strong>ACEs: </strong>",
    philly$ACEs,
    "%"
  )
  
  # color palette
  factpal <-
    colorFactor(c("#FBDA93",  "#F69953", "#DF5258", "#C42834"), philly$Risk)
  
  # create map using leaflet
  map <- leaflet(philly) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      stroke = FALSE,
      smoothFactor = 0.2,
      fillOpacity = .8,
      color = ~ factpal(Risk),
      weight = 1,
      popup = state_popup
    ) %>%
    addLegend(
      "bottomright",
      colors = c("#FBDA93",  "#F69953", "#DF5258", "#C42834"),
      labels = c("0% - 24%", "25% - 49%", "50% - 74%", "75% - 98%"),
      title = "Risk: Lowest To Highest",
      opacity = 1
    )
  
  # here we tell shiny to render the map in our app
  output$map <- renderLeaflet(map)
  
  ####################
  ###    SCATTER   ###
  ####################
  
  output$plot.philly <- renderPlotly({
    plot_ly(
      data = philly@data,
      x = ~ eval(parse(text = input$CODE1)),
      y = ~ eval(parse(text = input$CODE2)),
      marker = list(
        size = 10,
        color = 'rgba(255, 182, 193, .9)',
        line = list(color = 'rgba(152, 0, 0, .8)',
                    width = 2)
      )
    ) %>%
      add_trace(hovertemplate = paste0(philly$CODE,
                                       "<extra></extra>"),
                showlegend = FALSE) %>%
      layout(
        title = '',
        yaxis = list(title = input$CODE2),
        xaxis = list(title = input$CODE1)
      )
  })
  
  ####################
  ###    ARTICLE   ###
  ####################
  
  # load data
  oct19 <-
    import("fall 2019 final project data/2019 october - nbc south poll.sav")
  jul19 <-
    import("fall 2019 final project data/2019 july - nbc south poll.sav")
  sep18 <-
    import("fall 2019 final project data/2018 sept - nbc south poll.sav")
  apr18 <-
    import("fall 2019 final project data/2018 april - nbc south poll.sav")
  rural <- read.csv("Rural-Urban Codes_090619/rural-full.csv")
  
  names(oct19)
  names(rural)
  names(jul19)
  
  # merge for each dataset
  for (x in c("oct19", "jul19", "sep18", "apr18")) {
    a <- paste0(x, "$zip_code <- as.integer(", x, "$zip_code)")
    b <-
      paste0(x, " <- left_join(", x, ", rural, by = c('zip_code' = 'Zip'))")
    
    eval(parse(text = a))
    eval(parse(text = b))
  }
  
  # create dummy variables, update most important issue
  for (x in c("oct19", "jul19", "sep18", "apr18")) {
    if (x %in% c("oct19")) {
      c <-
        paste0(x,
               "$trump_impeach_yes <- ifelse(",
               x,
               "$trump_impeach == '1', 1, 0)")
      eval(parse(text = c))
    }
    
    if (x %in% c("oct19", "jul19")) {
      d <-
        paste0(x, "$support_roe <- ifelse(", x, "$roe_v_wade == '1', 1, 0)")
      eval(parse(text = d))
    }
    
    e <-
      paste0(x, "$liberal <- ifelse(", x, "$ideology %in% c('5','6'), 1, 0)")
    f <-
      paste0(x,
             "$bachelors <- ifelse(",
             x,
             "$education %in% c('5','6'), 1, 0)")
    g <-
      paste0(x,
             "$trump_approve <- ifelse(",
             x,
             "$trump_approval %in% c('5','6'), 1, 0)")
    
    h <- paste0(
      x,
      "$issue_matters_most_2 <- factor(",
      x,
      "$issue_matters_most," ,
      "labels = names(attr(",
      x ,
      "$issue_matters_most, \"labels\")))"
    )
    
    eval(parse(text = e))
    eval(parse(text = f))
    eval(parse(text = g))
    eval(parse(text = h))
  }
  
  # add when the surveys are
  oct19$survey <- "2019 October"
  jul19$survey <- "2019 July"
  sep18$survey <- "2018 September"
  apr18$survey <- "2018 April"
  
  
  ovr <- names(oct19)[names(oct19) %in% names(sep18)]
  change <- rbind(oct19[ovr], sep18[ovr])
  
  change$approve_of_congress <-
    factor(
      change$congress_approval,
      levels = c(1:5),
      labels = c(1, 1, 0, 0, NA)
    ) %>%
    as.character() %>%
    as.integer()
  
  change$approve_of_trump <-
    factor(change$trump_approval,
           levels = c(1:5),
           labels = c(1, 1, 0, 0, NA)) %>%
    as.character() %>%
    as.integer()
  
  change$trust_fed_gov_2 <-
    factor(change$trust_fed_gov,
           levels = c(1:6),
           labels = c(1, 1, 1, 0, 0, NA)) %>%
    as.character() %>%
    as.integer()
  
  change$condition_national_economy_2 <-
    factor(
      change$condition_national_economy,
      levels = c(1:5),
      labels = c(1, 1, 0, 0, NA)
    ) %>%
    as.character() %>%
    as.integer()
  
  change$region2 <-
    factor(change$region, labels = names(attr(change$region, "labels")[1:4]))
  
  filteredData <- reactive({
    if (input$region != "All")
    {
      change <- filter(change, region2 == input$region)
    }
    else {
      change
    }
  })
  
  output$plot <- renderPlot({
    filteredData() %>%
      mutate(Geography = ifelse(
        RuralCode == 'R',
        "Rural",
        ifelse(RuralCode == 'U', "Urban", "Suburban")
      )) %>%
      group_by(survey, Geography) %>%
      filter(!is.na(Geography)) %>%
      summarise(
        #`Approve of\nCongress` = weighted.mean(approve_of_congress, weight_natl, na.rm = T),
        `Approve of Trump` = weighted.mean(approve_of_trump, weight_natl, na.rm = T),
        `Trust Federal Government*` = weighted.mean(approve_of_congress, weight_natl, na.rm = T),
        `Thing National Econony\nIs In Good Condition**` = weighted.mean(approve_of_trump, weight_natl, na.rm = T)
      ) %>%
      gather(key = question, value = percent,-survey,-Geography) %>%
      ggplot() +
      geom_line(aes(x = percent, y = question)) +
      geom_point(aes(
        x = percent,
        y = question,
        color = survey,
        size = survey
      )) +
      theme_light() +
      facet_wrap( ~ Geography, ncol = 1) +
      scale_size_discrete(range = c(3, 5)) +
      scale_color_manual(values = c("steelblue2", "steelblue4")) +
      theme(
        strip.background = element_rect(fill = "white", color = "grey50"),
        strip.text = element_text(color = "grey20", face = "bold"),
        legend.position = c(0.84, 0.135),
        legend.background = element_rect(color = "grey50"),
        legend.title.align = 0.5,
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(size = 7)
      ) +
      scale_x_continuous(
        breaks = seq(0, 0.6, .1),
        limits = c(0, .6),
        labels = paste0(seq(0, 60, 10), "%")
      ) +
      labs(
        y = "",
        x = "",
        color = "Survey Month",
        title = "Fundamentals of Presidential Elections",
        subtitle = "Changes since the 2018 Midterms by geography",
        size = "Survey Month",
        caption = paste0(
          "* Trust the government at about half of the time or more\n",
          "** Reponsed that the economy is fairly good or very good"
        )
      )
  })
  
  oct19$party.lean <- factor(
    oct19$party5,
    levels = c(1:6),
    labels = c(
      "Republican",
      "Republican",
      "Independent",
      "Democrat",
      "Democrat",
      "No Answer"
    )
  )
  
  oct19$race2 <- factor(oct19$race,
                        levels = c(1:6),
                        labels = names(attr(oct19$race, "labels")))
  
  oct19$bachelors <-
    ifelse(oct19$education %in% c(5:6),
           1,
           ifelse(oct19$education %in% c(1:4), 0, NA)) %>%
    as.factor()
  
  oct19$region2 <- factor(oct19$region,
                          labels = names(attr(oct19$region, "labels"))[1:4])
  
  race.plot <- oct19 %>%
    filter(RuralCode == 'S') %>%
    mutate(
      is.white = ifelse(race == 1, 1, 0),
      is.black = ifelse(race == 2, 1, 0),
      is.hispanic = ifelse(race == 3, 1, 0),
      is.asian = ifelse(race == 4, 1, 0),
      is.other = ifelse(race == 5, 1, 0)
    ) %>%
    group_by(region2) %>%
    summarise(
      'White' = weighted.mean(is.white, weight_natl),
      'Black' = weighted.mean(is.black, weight_natl),
      'Hispanic' = weighted.mean(is.hispanic, weight_natl),
      'Asian' = weighted.mean(is.asian, weight_natl),
      'Other' = weighted.mean(is.other, weight_natl)
    ) %>%
    gather(key = race, value = percent,-region2) %>%
    #filter(!is.na(Geography)) %>%
    ggplot(aes(
      x = reorder(race, percent),
      y = percent,
      color = race
    )) +
    geom_linerange(aes(ymin = 0, ymax = percent), color = "black") +
    geom_point(size = 3) +
    facet_wrap( ~ region2) +
    theme_light() +
    scale_color_hue(l = 50) +
    scale_y_continuous(breaks = seq(0, 1, .2),
                       labels = paste0(seq(0, 100, 20), "%")) +
    labs(
      y = "Percent of Population",
      x = "Race",
      title = "Race: Percent of the Suburban Population by Region",
      caption = "Percents are weighted."
    )  +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white", color = "grey50"),
      strip.text = element_text(color = "grey20", face = "bold"),
      plot.title = element_text(face = "bold"),
      text = element_text(family = "Times New Roman")
    )
  
  
  manual.order <- c(
    'Did not\ncomplete\nhigh school',
    'High school\nor G.E.D.',
    'Associate\'s\ndegree',
    'Some\ncollege',
    'College\ngraduate',
    'Post\ngraduate\ndegree'
  )
  
  educ.plot <- oct19 %>%
    filter(RuralCode == 'S') %>%
    mutate(
      none = ifelse(race == 1, 1, 0),
      hs = ifelse(race == 2, 1, 0),
      aa = ifelse(race == 3, 1, 0),
      col = ifelse(race == 4, 1, 0),
      ba = ifelse(race == 5, 1, 0),
      ma = ifelse(race == 5, 1, 0)
    ) %>%
    group_by(region2) %>%
    summarise(
      '1~Did not\ncomplete\nhigh school' = weighted.mean(none, weight_natl),
      '2~High school\nor G.E.D.' = weighted.mean(hs, weight_natl),
      '3~Associate\'s\ndegree' = weighted.mean(aa, weight_natl),
      '4~Some\ncollege' = weighted.mean(col, weight_natl),
      '5~College\ngraduate' = weighted.mean(ba, weight_natl),
      '6~Post\ngraduate\ndegree' = weighted.mean(ma, weight_natl)
    ) %>%
    gather(key = education, value = percent,-region2) %>%
    separate(col = education,
             into = c("ord", "educ"),
             sep = '~') %>%
    mutate(ord = as.integer(ord)) %>%
    ggplot(aes(
      x = reorder(educ, ord),
      y = percent,
      color = educ
    )) +
    geom_linerange(aes(ymin = 0, ymax = percent), color = "black") +
    geom_point(size = 3) +
    facet_wrap( ~ region2) +
    theme_light() +
    scale_color_hue(l = 50) +
    scale_y_continuous(breaks = seq(0, 1, .2),
                       labels = paste0(seq(0, 100, 20), "%")) +
    labs(
      y = "Percent of Population",
      x = "Education",
      title = "Education: Percent of the Suburban Population by Region",
      caption = "Percents are weighted."
    )  +
    theme(
      legend.position = "none",
      strip.background = element_rect(fill = "white", color = "grey50"),
      strip.text = element_text(color = "grey20", face = "bold"),
      plot.title = element_text(face = "bold"),
      text = element_text(family = "Times New Roman")
    )
  
  output$plot2 <- renderPlot({
    if (input$pick2 == "Race") {
      race.plot
    } else {
      educ.plot
    }
  })
  
  oct19$party.lean <- factor(
    oct19$party5,
    levels = c(1:6),
    labels = c("R", "R", "I", "D", "D", NA)
  )
  
  oct19$impeach.dummy <- ifelse(oct19$trump_impeach == 1, 1, 0)
  
  graph <- oct19 %>%
    group_by(party.lean, RuralCode) %>%
    summarise(impeach.mean = weighted.mean(impeach.dummy, weight_natl)) %>%
    filter(!is.na(RuralCode) & party.lean %in% c("R", "I", "D")) %>%
    mutate(Geography = ifelse(
      RuralCode == 'R',
      "Rural",
      ifelse(RuralCode == 'U', "Urban", "Suburban")
    )) %>%
    ggplot(aes(
      x = party.lean,
      y = impeach.mean,
      color = party.lean,
      shape = party.lean
    )) +
    geom_linerange(aes(ymin = 0, ymax = impeach.mean), color = "black") +
    geom_point(size = 3) +
    facet_wrap( ~ Geography, ncol = 3) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "white"),
      legend.position = "none",
      plot.title = element_text(face = "bold"),
      text = element_text(family = "Times New Roman"),
      plot.caption = element_text(size = 7)
    ) +
    scale_color_manual(values = c("#DE0100", "#6F0B5E", "#0015BC")) +
    labs(
      x = "",
      y = "Percent Supporting Trump's Impeachment",
      title = "Partisan Views on Impeachment by Geographical Region",
      subtitle = "Are the suburbs the public opinion tipping point on impeachment?",
      caption = "\nThe 2018 Midterm Elections were largely determined by the suburban vote. However, the suburbs may not play the same tipping point\nrole when it comes public opinion on impeachment. This is according to an October 2019 suvey from NBC and Survey Monkey."
    ) +
    coord_flip() +
    scale_y_continuous(
      limits = c(0, 1),
      breaks = seq(0, 1, 0.25),
      labels = paste0(seq(0, 100, 25), "%")
    ) +
    scale_x_discrete(
      labels = c(
        "Republicans and\n Lean Republican",
        "Independents",
        "Democrats and\n Lean Democrats"
      )
    ) +
    geom_text(aes(label = paste0(round(
      impeach.mean * 100, 0
    ), "%")),
    vjust = -0.9)
  
  output$graph <- renderPlot({
    graph
  })
}


ui <- shinyUI(fluidPage(
  theme = shinytheme("journal"),
  
  # create navigation bar
  navbarPage(
    "Data Visualization in R",
    tabPanel(
      "Philadelphia Analysis",
      headerPanel("Map of Philadelphia"),
      h4("Instructions: click on each zipcode for more information"),
      br(),
      # the map is called here
      leafletOutput("map",
                    width = "100%",
                    height = "400px"),
      headerPanel("Variable Comparison"),
      fluidRow(
        column(
          4,
          h1(" "),
          h1(" "),
          h4(
            "Instructions: Use the dropdowns below to compare different variables in Philadelphia's zipcodes."
          ),
          h1(" "),
          selectInput(
            "CODE1",
            "X Axis:",
            c("Risk",
              "Poverty",
              "Education",
              "Unemployment",
              "Crime",
              "ACEs")
          ),
          h1(" "),
          selectInput(
            "CODE2",
            "Y Axis:",
            c("Poverty",
              "Education",
              "Unemployment",
              "Crime",
              "ACEs",
              "Risk")
          )
        ),
        column(8,
               plotlyOutput("plot.philly"))
      )
    ),
    
    tabPanel(
      "Suburbs Article",
      h1("The Story of the Suburbs", align = "center"),
      h3("by Bayley Tuch", align = "center"),
      h4(
        "The suburbs were the tipping point of the 2018 midterms. But will Democrats be able to rely on them in the same way in 2020? Or are they just a product of the rural-urban divide?",
        align = "center"
      ),
      
      img(src = 'suburbs.jpg', width = "100%", align = "center"),
      
      br(),
      br(),
      
      HTML(
        paste0(
          "The so-called suburban \"blue wave\" was the major story of the 2018 Midterm Elections. It felt like every news outlet wanted to cover how and why suburban voters, ",
          "many of whom are white and well educated, showed up to vote for the Democrats in such high numbers. The numbers truly were staggering; of the net 40 seats the Democrats ",
          "gained in the House of Representatives, 28 were in the suburbs. Comparably, Republicans didn't flip a single suburban district.<sup>1</sup>"
        )
      ),
      br(),
      br(),
      paste0(
        "Now it is a year later and everyone's attention has shifted to the 2020 Presidential Election. With the election only eleven months away, the new question is whether the ",
        "suburbs will be the tipping point of 2020."
      ),
      br(),
      br(),
      paste0(
        "In October 2019, NBC and Survey Monkey conducted a poll asking Americans about their political views. Using zipcode information from this survey and rural, suburban, and ",
        "urban classifications from GreatData.com, we can explore the current status of the American suburbs. The October survey included over 20,000 respondents, over 8,000 of ",
        "which live in the suburbs."
      ),
      br(),
      h3("Who are the Suburban Voters?"),
      paste0(
        "After the 2018 Midterm Election, many attributed the suburban blue wave to \"soccer moms,\" a catch-all term for white middle class well educated women (and men). Yes, ",
        "this demographic exists, but the suburbs are far from a monolith."
      ),
      br(),
      paste0(
        "Use the drop down menu to explore the figure below. It displays various demographics in the suburbs based on the October 2019 survey"
      ),
      br(),
      br(),
      selectInput("pick2",
                  "Filter Demographic",
                  c("Race", "Education")),
      plotOutput("plot2", width = "800px"),
      
      br(),
      paste0(
        "The graph about race displays that, yes, a majority of suburban voters are white. However, in the South and the West this number is below 60%, leaving around  40% of ",
        "voters to be nonwhite. In the South the second largest group is black respondents while in the West the second largest group is Hispanic respondents. These populations ",
        "make up a significant proportion of the voting block and are important demographic groups for suburban elections, especially in these regions."
      ),
      br(),
      br(),
      paste0(
        "The education graph displays that the vast majority of respondents in the suburbs did not complete high school. Only a small portion of the suburban population has a ",
        "bachelors degree, masters degree, or some post graduate degree. "
      ),
      br(),
      br(),
      HTML(
        paste0(
          "As previously mentioned, the suburbs are not a monolith and these graphs certainly portray this. While the highest race category <i>was</i> white, the highest eduacation ",
          "category was no high school degree. Clearly, suburbs in all regions (Northeast, Midwest, South, and West) do not perfectly fit into one \"soccer mom\" archetype"
        )
      ),
      br(),
      br(),
      HTML(
        paste0(
          "This context is important for understanding the role the suburbs play in American politics. There is no <i>one</i> suburban voter. Rather, the shifting demographic trends ",
          "in the suburbs can translate into major political trends, as we saw in 2018."
        )
      ),
      br(),
      h3("Where Do the Suburbs Stand on the Election Fundamentals?"),
      HTML(
        paste0(
          "To understand how the American suburbs might vote in 2020, we can look at the \"fundamentals\" of elections. These fundamentals, which have historically been able to ",
          "fairly accurately predict Presidential election results, are: approval of the incumbent, public opinion about the \"in-party,\" and the US economy.<sup>2</sup> While these factors",
          "cannot perfectly any election, they can give us an indication of where the suburbs currently stand regarding the 2020 Election. More specifically, a comparison of the ",
          "results from the October survey to a similar survey from right before the 2018 Midterms displays the changes in President Trump's approval, satisfaction with the federal ",
          "government, and perceptions of the national government"
        )
      ),
      br(),
      br(),
      paste0("You can use the drop down menu to view the results by region."),
      br(),
      br(),
      selectInput(
        "region",
        "Filter Responses by Region",
        c("All", "Northeast", "Midwest", "South", "West")
      ),
      plotOutput("plot", width = "800px"),
      paste0(
        "Notably, this analysis uses survey responses about people's views of the economy as a proxy for actual economic indicators like GDP, unemployment, and inflation. Since ",
        "the fundamentals approach to elections involves people making decisions off of how the economy affects them, we are able to use survey results to stand in for the actual ",
        "status of the economy."
      ),
      br(),
      br(),
      HTML(
        paste0(
          "he graph above displays that across rural, suburban , and urban regions, there has been relatively little change in opinion about Trump and the condition of the economy. ",
          "In the suburbs specifically, there has been a slight and relatively insignificant decrease (only about 1%) in public opinion for both approval of Trump and perception of a ",
          "good economy. However, trust in the federal government has improved durastically in the past year. In suburban areas the difference was around a 6% increase. This increased ",
          "trust in the federal government accross the board is surprising given the recent impeachment inquiry. Dispite the fact that 47.5% of Americans support impeachment (45.8% are ",
          "opposed), Americans seem to have gained trust in the federal government over the past year.<sup>3</sup> That being said, trust in the federal government is still relartively low. In ",
          "all three geographical regions -- rural, suburban, and urban -- less than half of respondents trust the federal government half the time or more. "
        )
      ),
      br(),
      h3("What does this mean for the Presidential Election?"),
      paste0(
        "These responses indicate  that two of the three fundamentals are relatively unchanged from 2018. Suburban voters' approval of President Trump and views on the economy dropped",
        " from a little over 45% to a little under 45% -- but this difference is insignificant. These numbers should excite Democrats who were extremely successful in getting the ",
        "suruban vote a year ago. If voters' perceptions on these fundamentals haven't changed since the Democrat's immense success a year ago, the suburbs may deliver for the Democrats",
        " again in 2020."
      ),
      br(),
      br(),
      paste0(
        "That being said, the numbers regarding trust in the federal government are a little harder to interpret. If you approach these number through the lens of the fundamentals, ",
        "this question may indicate an increase in support for the \"in-party.\" However, it is unclear whether respondents took this question to mean the \"in-party\" in the White House, ",
        "in the Senate, or in the House of Representatives. If this question is an indicator on people's views of the party in the White House, Republicans should be hopeful. This would ",
        "indicate that despite the impeachment inquiry, or perhaps because of the backlash to it, the President is winning over the trust of the white suburban voters he may have lost ",
        "between 2016 and 2018. However, if respondents answered this question thinking about Congress, it is unclear what to think. These individuals may have more trust in the federal ",
        "government because of the new Democratic Representatives many suburban districts vote for. This may indicate that many suburban voters will continue to vote Democrat in 2020."
      ),
      br(),
      br(),
      paste0(
        "As a whole, the suburbs seem relatively unchanged from where they were going into the 2018 Election."
      ),
      br(),
      h3("Are the Suburbs Just a Product of the Rural Urban Divide?"),
      paste0(
        "Another potential explanation for how the suburbs voted in 2018 is the rural-urban divide. The changing suburbs, demographically and politically, may be attributed to how the ",
        "suburbs are shifting to be more similar to urban environments while urban and rural environments  are moving further and further apart. "
      ),
      br(),
      br(),
      paste0(
        "The graph above displays a major gap between urban and rural voters on approval of President Trump and perception of the national economy. The difference is more 20 percentage ",
        "points. "
      ),
      br(),
      br(),
      paste0(
        "While the shift over the past year in public opinion regarding Trump and perception of the economy was small for all three geographic groups, it is notable that the suburbs ",
        "moved (a tiny bit) in the direction of urban respondents. This could be an indication of the direction the suburbs are moving going forward. "
      ),
      br(),
      br(),
      paste0(
        "Some might argue that the difference between rural and urban Americans is only because of partisanship. We looked at public opinion on impeachment to evaluate whether this is ",
        "true. And, if it is true, if partisanship also influences the trends we see in the suburbs."
      ),
      plotOutput("graph", width = "800px"),
      br(),
      br(),
      paste0(
        "According to this analysis of public opinion regarding the impeachment inquiry, there are major divides between rural and urban respondents, even when separated by race. Most ",
        "notably there is a 10 point difference between rural Republicans and urban Republicans. The suburbs are at 10%, which is more aligned with rural Republicans than it is with urban ",
        "Republicans. This goes against the theory that the suburbs may be moving in the direction of liberal urban voters."
      ),
      br(),
      br(),
      paste0(
        "For Democrats and Independents, the suburban respondents are squarely in the middle of rural and urban respondents. Though for Democrats, the difference between rural and urban ",
        "individuals is extremely small, and potentially insignificant."
      ),
      br(),
      br(),
      paste0(
        "What does this mean? One thing is certain: the rural-urban divide is not imaginary. While Democrats in rural and urban communities hold similar opinions, Independents and Republicans ",
        "do not. For Independents and Democrats, the suburbs are squarely in the middle of this rural-urban divide. The suburban Republicans' low support of impeachment may indicate strong party ",
        "ties that are not as present for urban Republicans."
      ),
      br(),
      br(),
      paste0(
        "This divide raises many questions -- why are there regional differences between Republicans and not Democrats? Which direction are the suburbs moving, towards their rural counterparts ",
        "or their urban counterparts? And how has this changed since 2018? Unfortunately, this survey did not ask respondents about their opinion of impeachment before the 2018 Midterms. However, ",
        "this information could have been useful in evaluating the movement of suburban public opinion. After all, given the previous graph, the suburbs may be moving closer to their liberal ",
        "suburban counterparts. "
      ),
      h3("Why does this matter?"),
      paste0(
        "In a country so divided by both partisanship and region, it is important to look at the \"in between.\" When it comes to geography, the suburbs are the middle ground between the ",
        "increasingly liberal urban areas and the increasingly conservative rural areas."
      ),
      br(),
      br(),
      paste0(
        "The 2018 Midterm Election put the suburbs in the spotlight. Now we have a chance to see how public opinion in the suburbs has changed since this election and whether the suburbs ",
        "will keep their spot in the political spotlight. Both are relatively uncertain. There has been little change in election fundamentals in the past year, but any change seems to have ",
        "been in the direction of liberal suburbs. Yet, if impeachment is any indication of how the suburbs feel about Trump, the Democrats might not get the sweeping victory they did in 2020."
      ),
      br(),
      br(),
      paste0(
        "Rather, the suburbs are smack in the middle of rural and urban respondents, no matter their partisanship. With suburban impeachment, specifically among independents, being so mixed, ",
        "both parties should think seriously about their approach to the suburbs in November."
      ),
      br(),
      br(),
      paste0("In 2020, the suburbs could be anyone's game."),
      br(),
      br(),
      
      
      hr(),
      
      h4("Methodology"),
      paste0(
        "The survey data for this article comes from two surveys conducted by NBC and Survey Monkey as a part of a project about registered voters and the South region. That being said, ",
        "the respondents are from across the entire US and any weighting that was used was based on national demographic weights. The surveys used were in the field in October 2019 and ",
        "September 2018. Additionally, this analysis used urban/suburban/rural data from GreatData.com. Their dataset matches zip codes to these three geographical classifications based on ",
        "population and other indicators."
      ),
      
      h4("Citations"),
      "1. Badger, Emily, Quoctrung Bui, and Josh Katz. 2018. \"The Suburbs Are Changing. But Not in All the Ways Liberals Hope.\" The New York Times.",
      br(),
      HTML(
        "2. Campbell, James E. 2005. \"The Fundamentals in US Presidential Elections: Public Opinion, the Economy, and Incumbency in the 2004 Presidential Election\" in <i>Journal of Elections, Public Opinion, and Parties</i>"
      ),
      br(),
      "3.Bycoffe, Aaron, Ella Koeze and Nathaniel Rakich. 2019. \"Do Americans Support Impeaching Trump?\" FiveThirtyEight"
    )
  )
))

shinyApp(ui = ui, server = server)
