source("global.R")

# Define UI for application that draws a histogram
ui <- navbarPage(

    title =  div(
    "Louisville's American Rescue Plan",
    div(
        id = "img-id",
        img(src = "GLP_logo_short2.png", height = 50)
    )
    ),

    theme = shinythemes::shinytheme("paper"),

    header = tags$head(includeCSS("styles.css")),
    windowTitle = "ARP in Louisville",

    tabPanel(
        "Welcome",
        fluidRow(
            column(12, align = "center",
                   h2("The American Rescue Plan in Louisville"))),
        fluidRow(
            column(10, offset = 1, class = "story",
                  h5("American Rescue Plan Funding represents an historic opportunity for every city in the United States, but Louisville is particularly uniquely positioned for transformation.
                      As a combined city-county government, Louisville Metro Government will receive one of the largest allocations of funding to a single government of Louisville’s peer cities,
                      both on an absolute and per capita basis.
                      Community members, particularly Black leaders and impacted people, are at the civic table and have articulated their ideas through A Path Forward.
                      As a result, Louisville is uniquely positioned among its peers to invest in the visions of people who have been most impacted by its deepest problems.
                      Over the next four years, the GLP can give impacted people, systems-level thinkers,
                      and new leaders the information they need to push the ARP process towards one that is transformational for the city.")))

    ),

    tabPanel(
    "Overview",
        fluidRow(
            column(12, align = "center",
                h2("Tracking Louisville's ARP Spending"))),

        fluidRow(
            column(10, offset = 1, class = "story",
            h4("Funding Allocated so Far"),
            p("Louisville will spend more than $388 million over the next 5 years. It has already allocated most of the funding across a handful of issue areas.
               Click into the graph below to dig into what's been funded."),
           plotlyOutput("arp_treemap"))
        ),

        fluidRow(
            column(10, offset = 1, class = "story",
                   h2("Looking at our peer cities shows us how Louisville's spending stacks up."),
                   h5("Over the next 5 years, Louisville will spend more than $388 million. Our peers will be doing the same, and all of the data will be reported through the federal government.
                      We can compare Louisville to our peers to understand where we are overinvesting and where we are underinvesting."),
                   plotlyOutput("issue_plot"))),

    fluidRow(
        column(10, offset = 1,
               h2("Current projects"),
               DT::dataTableOutput("mytable"))),

        fluidRow(
            column(10, offset = 1,
                   h2("Many projects are underway."),
                   h5("Use the timeline below to understand the current status of ongoing ARP projects."),
                   timevisOutput("ARP_timeline"))
        )
    ),

    tabPanel(
        "Homelessness and Affordable Housing",
        fluidRow(
            column(12, align = "center",
                   h2("Homelessness and Affordable Housing"))),
        fluidRow(
            column(10, offset = 1, class = "story",
                   h5("When Louisville Metro Government surveyed residents to ask which topic they would most like to see addressed by ARP spenging,
                      houselessness and affordable housing rose to the top as the most urgent."))),
        fluidRow(
            column(10, offset = 1,
            h2("Court Eviction Diversion Program"),
            h5("Louisville's Court Eviction Diversion Program provides financial assistance for both past due rent and future rent for households below 80% of area median income who are facing an eviction and have received a Forcible Detainer."),
            h4("Demographics")),
            column(2, offset = 1,
                   p("The chart to the right compares the demograhpics of participants in the Court Eviction Diversion Program to cost-burdened residents in Louisville by race.
                     The data show that Black residents and residents of other races are represented in the program at a greater rate than which they are cost burdened.
                     Whether this is equitable, and how GLP should talk about it, is up for discussion.")),
            column(6, plotlyOutput("ced_plot"))),
        fluidRow(
            column(10, offset = 1, h4("Peer Actions")),
            column(2, offset = 1,
                   p("Louisville peers are also investing in eviction prevention. The dots on the map to the right show the scale of their investment.
                     Hover over the map to learn about their programs.")),
            column(6,
                  leafletOutput("map")))

    ),

    tabPanel(
        "Public Safety",
        fluidRow(
            column(12, align = "center",
                   h2("Public Safety"))),
        fluidRow(
            column(6, offset = 3, class = "story",
                   h5("The data that the Greater Louisville Project has traditionally gathered is usually objective, peer-city comparable, and in line with other agencies across the city.
                      The Public safety component of Louisville's ARP spending is both at the center of the conversation and a new step for the Greater Louisville Project.")))

    )
)
