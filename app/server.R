#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

server <- function(input, output, session) {

  output$ARP_timeline <- renderTimevis({
    timevis(timevisData,
            groups = timevisDataGroups,
            options = list(stack = FALSE, start = "1/1/2021", end = "3/31/2027"))
  })


  output$map <- renderLeaflet({

    starting_map

  })

  output$arp_treemap <-  renderPlotly({

    fig <- plot_ly(
      type    = "treemap",
      labels  = lou_allocations_treemap$Project_name[],
      parents = lou_allocations_treemap$Category,
      values  = lou_allocations_treemap$Funding,
      texttemplate = '%{label}<br>%{value:$,.3s}',
      branchvalues="total",
      domain  = list(column=1)) %>%
      layout(
        treemapcolorway = c("#0E4A99", "#F58021", "#00A9B7", "#800055", "#356E39", "#CFB94C", "#7E9C80", "#0E4A99", "#F58021"),
        uniformtext=list(minsize = 14, mode='hide'),
        font = list(family = "Museo Sans 300"),
        plot_bgcolor = 'transparent',
        paper_bgcolor = 'transparent')

    fig
  })

  output$issue_plot <- renderPlotly({

    issue_plot

  })

  output$ced_plot <- renderPlotly({

    plotly_CED_chart

  })

  output$mytable <- DT::renderDataTable(lou_projects_og,
                                        options = list(scrollX = TRUE),
                                        rownames = FALSE)
}



# Define server logic required to draw a histogram

