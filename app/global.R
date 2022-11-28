library(dplyr)
library(tidyr)
library(htmltools)
library(timevis)
library(leaflet)
library(sf)
library(magrittr)
library(DT)
library(plotly)
library(shiny)
library(shinyWidgets)
#library(shinyjs)

# library(dygraphs)
# library(xts)

library(scales)
# library(stringr)
# library(lubridate)

#setwd("app")
load("data_export.RData")

# FOR JOSH

# We're going to replace the old data that goes into this dashboard with the new peer city data: https://greaterlou.shinyapps.io/ARP-in-Lou/

# This file loads data_export.RData and then preps the data frames to go into the shiny app. 
# We're going to edit the data we load in and the code below to get everything updated.


### Step 1: Step up arp_resource.Rmd to save all data frames into an RData file.
# We want arp_resource.Rmd to write a new .RData file that will eventually replace data_export.RData once it's complete.
# This is important because we will need to load these data frames without rerunning all of arp_resource.Rmd.
# Here's the code (you might need to edit slightly):
### save(FIPS_df, FIPS_info, governments_long, governments_wide, projects, expenditures, file = "app/data_export2.RData")






### lou_allocations_treemap

# It was derived from the lou_allocations data frame, and will now come from the projects data frame. 

# lou_allocations_l3 <- projects %>%
#   filter() %>%                                # filter to Louisville 
#   select(Category = lou_category, Project_name = project_name, Funding = amount) # Let's keep the same column names column names by using code like 

# Add in the unallocated amount
# lou_total = governments_wide %>% filter(FIPS == "21111") %>% pull(total_allocation)
# lou_allocated = projects %>% fiter(LOUISVILLE) %>% summarize(amount = sum(amount)) %>% pull(amount)
# unallocated_row = data.frame(Category = "Unallocated", Project_name = "Unallocated", Funding = lou_total - lou_allocated)

# lou_l3 %<>% bind_rows(unallocated_row)


lou_allocations_l3 <- lou_allocations %>%
  select(Category, Project_name, Funding)

lou_allocations_l2 <- lou_allocations_l3 %>%
  group_by(Category) %>%
  summarize(Funding = sum(Funding), .groups = "drop") %>%
  mutate(
    Project_name = Category,
    Category = "")

lou_allocations_treemap <- bind_rows(lou_allocations_l2, lou_allocations_l3)



# Some data frames might 





# Need: 

# timevisData and timevisDataGroups

#https://censusreporter.org/profiles/14000US37129010501-census-tract-10501-new-hanover-nc/

timevisData <- data.frame(
  id = 1:4,
  content =
    c("RFA Opened", "RFA Under Review",
      "RFI Opened", "RFA being crafted"),
  start =
    c("2021-09-30", "2021-12-10",
      "2021-11-18", "2021-12-09"),
  end  =
    c("2021-12-10", "2021-12-13",
      "2021-12-09", "2021-12-13"),
  group = c(rep("Permanent Supportive Housing", 2),
            rep("Childcare and Early Learning Initiatives", 2)),
  type = c(rep("range", 4))
)

timevisDataGroups <- data.frame(
  id = c("Permanent Supportive Housing", "Childcare and Early Learning Initiatives"),
  content = c("Permanent Supportive Housing", "Childcare and Early Learning Initiatives")
)


timevis(timevisData,
        groups = timevisDataGroups,
        options = list(stack = FALSE))


# lou_projects_og
# Replace with the projects data frame filtered to Louisville

lou_projects_og %<>%
  transmute(Project,
            Amount = dollar(Amount),
            Description)


# starting_map

lou_label <-
  sprintf("%s<br/>%s<br/>%s<br/>%s",
          "<b>Louisville</b>",
          "Louisville has allocated <b>$9 million</b> to provide financial assistance",
          "for both past due rent and future rent for households below 80% of area median income",
          "who are facing an eviction and have received a Forcible Detainer.")

cinci_label <-
  sprintf("%s<br/>%s<br/>%s<br/>%s",
          "<b>Cincinnati</b>",
          "Cincinnati has allocated <b>$5 million</b> to to assist community members",
          "who are housing insecure because of an inability to make mortgage payments or",
          "keep current on utility bills.")

all_labels <- c("", "", lou_label, "", "", "", "", "", "", cinci_label, rep("", 7))

all_labels %<>% lapply(htmltools::HTML)

temp_sizes <- c(0, 3, 9, 0, 10, 5, 6, 2, 0, 5, rep(0, 7))

starting_map <-
leaflet() %>%
  addPolygons(
    data = usa,
    fillOpacity = 1,
    color = "#404040",
    opacity = 0,
    weight = 2) %>%
  addCircleMarkers(
    data = counties,
    radius = ~sqrt(temp_sizes)*3,
    fillOpacity = 1,
    fillColor = "#00A9B7",
    opacity = 0,
    weight = 2,
    label = ~all_labels)







issue_plot <- plot_ly(ARP_major) %>%
  add_trace(
    x = ~Louisville, y = ~Category, name = "Louisville", type = 'scatter',
    mode = "markers", marker = list(color = "#00A9B7",
                                    size = 15)) %>%
  add_trace(
    x = ~`Peer Average`, y = ~Category, name = "Peer Average", type = 'scatter',
    mode = "markers", marker = list(color = "#000000",
                                    symbol = 142,
                                    size = 15)) %>%
  add_trace(
    x = ~Cincinnati, y = ~Category, name = "Cincinnati", type = 'scatter',
    mode = "markers", marker = list(color = "#800055")) %>%
  add_trace(
    x = ~Columbus, y = ~Category, name = "Columbus", type = 'scatter',
    mode = "markers", marker = list(color = "#800055")) %>%
  layout(yaxis = list(title = "",
                      gridcolor = "#BEBEBE",
                      categoryorder = "array",
                      categoryarray = c("Administrative and Other",
                                        "Revenue Replacement",
                                        "Infrastructure",
                                        "Premium Pay",
                                        "Services to Disproportionately Impacted Communities",
                                        "Negative Economic Impacts",
                                        "Public Health")),
         plot_bgcolor = 'transparent',
         paper_bgcolor = 'transparent',

         xaxis = list(title = "Funding Amount",
                      gridcolor = "#BEBEBE"))


# t=glpdata::housing_county %>%
#   filter(FIPS == "21111", var_type == "estimate", year == 2019, sex == "total")

# black cb renter 23690, w 51795, other 6691
#c(0.29, 0.63, 0.08)

# prog 75, 37, 26
#c(0.54, 0.26, 0.20)

ced_df <- data.frame(
  type = c("Cost-Burdened Residents", "CED Program Participants"),
  Black = c(29, 54),
  White = c(63, 26),
  Other = c(8, 20))

# Housing demograhpics
plotly_CED_chart <- plot_ly(ced_df,x = ~type, y = ~White, type = 'bar',name= 'White') %>%
  add_trace(y = ~Black, name = 'Black') %>%
  add_trace(y = ~Other, name = 'Other') %>%
  layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack")


