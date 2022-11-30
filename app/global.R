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

load("data_export2.RData")

expenditure_graph <- expenditures  %>%
  mutate(time_period = paste0(quarter, "-", year)) %>%
  select(FIPS, time_period, Total.Cumulative.Expenditures) %>%
  group_by(FIPS, time_period) %>%
  
  summarize(n=sum(Total.Cumulative.Expenditures, na.rm = TRUE)) %>%
  mutate(
    total = sum(n),
    rate = n/sum(n)*100) %>%
  drop_na()

# Treemap
lou_allocations_l3 <- projects %>%
  filter(`city/county` == 'Louisville',
         !is.na(amount)) %>%
  select(Category = lou_category,
         Project_name = project_name,
         Funding = amount)

lou_allocations_l2 <- lou_allocations_l3 %>%
  group_by(Category) %>%
  summarize(Funding = sum(Funding), .groups = "drop") %>%
  mutate(
    Project_name = Category,
    Category = "")


# Add in the unallocated amount

lou_total = governments_wide %>%
  filter(FIPS == "21111") %>%
  pull(total_allocation)

lou_allocated = projects %>%
  filter(`city/county` == 'Louisville',
         !is.na(amount)) %>%
  summarize(amount = sum(amount)) %>%
  pull(amount)


high_level_row <- data.frame(Category = "", Project_name = "Unallocated", Funding = lou_total - lou_allocated)
low_level_row <- data.frame(Category = "Unallocated", Project_name = NA, Funding = lou_total - lou_allocated)

lou_allocations_l2 %<>% bind_rows(high_level_row)

lou_allocations_l3 %<>% bind_rows(low_level_row)

lou_allocations_treemap <- bind_rows(lou_allocations_l2, lou_allocations_l3) %>%
  mutate(Funding = round(Funding, 0))

lou_allocations_treemap$Project_name[lou_allocations_treemap$Category == "Compliance and Reporting"] <- "Compliance & Reporting"

lou_allocations_treemap %<>%
  mutate(
    Project_name = 
      case_when(
        Category == "" ~ Project_name,
        Category != "" ~ str_wrap(Project_name, width = 15) %>% str_replace_all("\\n", "<br>")))

# Need: 

# timevisData and timevisDataGroups

#https://censusreporter.org/profiles/14000US37129010501-census-tract-10501-new-hanover-nc/

time_data <- projects %>%
  filter(`city/county` == "Louisville",
          !is.na(amount))

# temp <- expenditures_lou %>%
#   complete(nesting(year, quarter), project_id) %>%
#   group_by(year, quarter)

expenditures_wide <- expenditures_lou %>%
  mutate(period = paste0(quarter, "_", year)) %>%
  select(project_id, period, total_expenditures) %>%
  pivot_wider(names_from = period, values_from = total_expenditures) %>%
  select(project_id, Q4_2021, Q1_2022, Q2_2022, Q3_2022)



# FROM PROJECTS
# Project Name
# Funding amount
# start date
# end date

# Left join expenditure data to projects and then calculate a few things:
# Missing start and end dates
# desn't  have a tart date - first day of the quarter with spending
# doesn't have an end date and has been > 95% spent - end date is the end of that quarter


# mutate(new_end_date = if_else(end_date == "none", calculated_end_date, end_date))


# Completely undetermined dates
# Start date April 1, 2023
# End date December 31, 2026


# Calculate percent spent
# Calculate four days for each bar 

#####################

project_expenditures <- projects %>%
  mutate(
    city_county = `city/county`,
    city_county_status = `city/county_status`) %>%
  filter(city_county == "Louisville") %>%
  left_join(expenditures_wide, by = c("project_id"))  %>%
  transmute(
    project_id,
    
    # info
    lou_category,
    amount,
    project_name,
    
    # dates
    start_date = start_date...8,
    end_date = stop_date...9,
   
    Q4_2021,
    Q1_2022,
    Q2_2022,
    Q3_2022,
    
    final_pct = replace_na(Q3_2022 / amount , 0))
  
  

##### calculate date information
#q4 dates October 31st to Dec 31st
#q1 dates Jan 1st to March 31st
#q2 dates April 1st to June 30th
# q3 dates July 1st to Sep 30th

library(lubridate)

#solves undertermined na's
project_expenditures %<>%
  mutate(
    start_date_calc = 
      case_when(
        !is.na(Q4_2021) & Q4_2021 > 0 ~ "6/1/2021",
        !is.na(Q1_2022) & Q1_2022 > 0 ~ "1/1/2022",
        !is.na(Q2_2022) & Q2_2022 > 0 ~ "4/1/2022",
        !is.na(Q3_2022) & Q3_2022 > 0 ~ "7/1/2022",
        TRUE ~ "none"),
    
    end_date_calc = 
      case_when(
        Q4_2021 / amount > .99 ~ "12/31/2021",
        Q1_2022 / amount > .99 ~ "3/31/2022",
        Q2_2022 / amount > .99 ~ "6/30/2022",
        Q3_2022 / amount > .99 ~ "9/31/2022",
        TRUE ~ "none"),
    
    date_source = case_when(
      start_date != "none" & end_date != "none" ~ "project_report",
      start_date_calc != "none" & end_date_calc != "none" ~ "dolllars",
      TRUE ~ "guess"),
    
    start_date = case_when(
      start_date != "none" ~ start_date,
      start_date_calc != "none" ~ start_date_calc,
      TRUE ~ "4/1/2023"),
    
    end_date = case_when(
      end_date != "none" ~ end_date,
      end_date_calc != "none" ~ end_date_calc,
      TRUE ~ "12/31/2026")) %>%
  select(project_id, lou_category, amount, project_name, start_date, end_date, final_pct) %>%
  filter(!is.na(amount))

# Add in category summaries

full_groups <- project_expenditures %>%
  group_by(lou_category) %>%
  summarize(
    final_pct = sum(final_pct * amount) / sum(amount),
    amount = sum(amount),
    start_date = "06/1/2021",
    end_date = "12/31/2026") %>%
  mutate(project_name = lou_category)

project_expenditures$project_name[project_expenditures$project_name == "Compliance and Reporting"] <- "Compliance & Reporting"

project_expenditures %<>%
  bind_rows(full_groups) %>%
  mutate(
    start_date = mdy(start_date),
    end_date = mdy(end_date),
    current_date = start_date + (end_date - start_date) * final_pct,
    amount1 = amount * final_pct,
    amount2 = amount * (1-final_pct))

# Rename columns and keep only data with complete dates at this point

project_expenditures %<>%
  transmute(
    lou_category, 
    amount, amount1, amount2, final_pct,
    group = project_name,
    start_date1 = start_date, 
    end_date1 = current_date,
    start_date2 = current_date,
    end_date2 = end_date) %>%
  filter(!is.na(start_date1), !is.na(end_date2))

  
first_bar = project_expenditures %>%
  transmute(lou_category, 
            amount = amount1,
            budget_pct = final_pct, 
            group, 
            start = start_date1, 
            end = end_date1)

second_bar = project_expenditures %>%
  transmute(lou_category, 
            amount = amount2,
            budget_pct = 1 - final_pct, 
            group, 
            start = start_date2, 
            end = end_date2)

all_bars <- bind_rows(first_bar, second_bar)

top_groups = c("Affordable Housing", "Public Safety", "Healthy Neighborhoods", "Workforce Development", "Public Health", "Premium Pay", "Infrastructure", "Compliance and Reporting")

all_bars %<>%
  mutate(lou_category = factor(lou_category, levels = top_groups, ordered = TRUE)) %>%
  arrange(lou_category) %>%
  group_by(lou_category) %>%
  arrange(desc(group %in% top_groups), start, .by_group = TRUE) %>%
  ungroup()

#Affordable Housing", "Public Safety", "Healthy Neighborhoods", "Workforce Development", "Public Health", "Premium Pay", "Compliance and Reporting")

all_bars$group[all_bars$group == "University of Louisville Environmental Institute Healthy Building Research Complex and Parkscape"] <- "U of L Environmental Institute Healthy Building Research Complex and Parkscape"
all_bars$group[all_bars$group == "Other COVID-19 Public Health Expenses (including Communications, Enforcement"] <- "Other COVID-19 Public Health Expenses"
all_bars$group[all_bars$group == "Vaccine Incentives - Component Units/External Agencies (Public and Private)"] <- "Vaccine Incentives - Component Units/External Agencies"




timevisData <- all_bars %>%
  arrange(group) %>%
  transmute(
    id = row_number(),
    content = if_else(end - start > 180, dollar(amount), "."),
    start,
    end,
    group,
    type = "range",
    style = rep(c("background-color: green; border-style: none; border-radius: 0px;", 
                  "background-color: lightgreen; border-style: none; border-radius: 0px;"), times = nrow(all_bars) / 2))

timevisDataGroups <- data.frame(
  id = unique(all_bars$group),
  content = unique(all_bars$group),
  nestedGroups = I(list(
    # Affordable Housing
    c("Court Eviction Diversion Program", "Security Deposit and Rental Assistance", "Utility Assistance Program",
      "Outdoor Safe Space", "Home Repair", "College Street Property Renovations", "Affordable Housing Trust Fund",
      "Down Payment Assistance", "Shelter Renovations", "Permanent Supportive Housing", "Reverse Redlining",
      "Veterans Housing Southwest", "Foreclosure funding"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
    # Public Safety
   c("Community Violence Intervention Program", "LMPD Technology", "Juvenile Assessment Center", 
     "Public Safety Salaries", "Public Safety Hiring Incentive", "Public Safety Reforms",
     "Restorative Justice Expansion", "Ambassador Institute Expansion - Community Mobilization Project", "Ambassador Institute Expansion - Capacity Building Fellowship",
     "Family Recovery Court - Seven Counties", "Coordinated Crisis Response to Gun Violence", "Trauma Resilient Communities Expansion",
     "Office of Youth Development"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
   # Healthy Neighborhoods
   c("Enhanced Community Ambassador and Security Programs", "Lead Free Louisville", "Rhodia Brownfields Remediation",
     "Chickasaw Park Pond", "Baxter Community Center", "Main Library", "Portland Library", "Parkland Library", 
     "Iroquois Park Tennis Courts", "Berrytown Park", "Elliott Park Master Plan Implementation", "Swimming Pools",
     "Fern Creek Library", "Broadband", "U of L Environmental Institute Healthy Building Research Complex and Parkscape",
     "Early Learning"), NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
   # Workforce Development
   c("Small Business Economic Assistance", "Comprehensive Reentry Employment Services", "Healthcare Workforce Innovation Coalition",
     "JCTC Taste of the Trades"), NA, NA, NA, NA,
   # Public Health
   c("COVID-19 Response - Vaccination", "COVID-19 Response - Testing", "COVID-19 Response - Contact Tracing", "COVID-19 Response - Personal Protective Equipment",
     "Other COVID-19 Public Health Expenses", "COVID-19 Response - Other COVID-19 Public Health FY 21",
     "COVID-19 Response - Payroll", "Household Assistance", "Residential Services for Substance Abuse and Addiction",
     "Other Public Health Services", "Substance Abuse Services", "Temporary Staff Support for High Food Distribution",
     "COVID-19 Response - Prevention in Congregate Settings", "Suicide Prevention Project", "Electronic Health Records",
     "St Mary's Birthing Center", "U of L Health-Mary & Elizabeth Hospital Birthing Center"), 
   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
   # Premium Pay
   c("Vaccine Incentives - Metro Employees", "Premium Pay - Metro Employees", "Vaccine Incentives - Component Units/External Agencies",
     "Premium Pay - Private Sector Component Units/External Agencies", "Premium Pay - Public Sector Component Units/External Agencies", 
     "Premium Pay Round 2"), NA, NA, NA, NA, NA, NA,
   # Infrastructure
   c("LouMed Infrastructure Improvements"), NA,
   # Compliance and Reporting
   c("Compliance & Reporting"), NA)))

# nestedGroups = I(list(c("Court Eviction Diversion Program", "Security Deposit and Rental Assistance", "Utility Assistance Program"), NA, NA, NA,
#                       c("Coordinated Crisis Response to Gun Violence", "Restorative Justice Expansion", "Ambassador Institute Expansion - Capacity Building Fellowship",
#                       "Family Recovery Court - Seven Counties", "Public Safety Hiring Incentive", "Public Safety Salaries"), NA, NA, NA, NA, NA, NA,
#                       c("Enhanced Community Ambassador and Security Programs", "Lead Free Louisville"), NA, NA,
#                       NA,
#                       c("COVID-19 Response - Other COVID-19 Public Health FY 21", "Other Public Health Services"), NA, NA,
#                       c("Vaccine Incentives - Metro Employees", "Premium Pay - Metro Employees"), NA, NA,
#                       NA)))

# lou_projects_og
# Replace with the projects data frame filtered to Louisville

lou_projects_og <- projects %>%
  filter(`city/county` == "Louisville") %>%
  transmute(
    Project = project_name,
    Category = lou_category,
    Amount = dollar(amount),
    Description = description)


# starting_map

map_df <- projects %>%
  mutate(FIPS = if_else(FIPS %in% c("29189", "29510"), "MERGED", FIPS)) %>%
  group_by(FIPS) %>%
  filter(lou_category == "Affordable Housing") %>%
  arrange(desc(amount)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(FIPS, project_name, amount, description)

# lou_label <-
#   sprintf("%s<br/>%s<br/>%s<br/>%s",
#           "<b>Louisville</b>",
#           "Louisville has allocated <b>$9 million</b> to provide financial assistance",
#           "for both past due rent and future rent for households below 80% of area median income",
#           "who are facing an eviction and have received a Forcible Detainer.")
# 
# cinci_label <-
#   sprintf("%s<br/>%s<br/>%s<br/>%s",
#           "<b>Cincinnati</b>",
#           "Cincinnati has allocated <b>$5 million</b> to to assist community members",
#           "who are housing insecure because of an inability to make mortgage payments or",
#           "keep current on utility bills.")

map_obj <- counties %>% left_join(map_df, by = "FIPS")

leaflet_labels <- 
  
  sprintf("%s<br/>%s<br/>%s",
          paste0("<b>", map_obj$city, "</b>"),
          paste0("Budget: <b>", dollar(map_obj$amount), "</b>"),
          map_df$description) %>% 
  
  lapply(htmltools::HTML)

starting_map <- leaflet() %>%
  addPolygons(
    data = usa,
    fillOpacity = 1,
    color = "#404040",
    opacity = 0,
    weight = 2) %>%
  addCircleMarkers(
    data = map_obj,
    radius = ~sqrt(amount) / 500,
    fillOpacity = 1,
    fillColor = "#00A9B7",
    opacity = 0,
    weight = 2,
    label = ~leaflet_labels)


summary_df <- projects %>%
  mutate(
    FIPS = if_else(FIPS %in% c("29189", "29510"), "MERGED", FIPS),
    FIPS = if_else(FIPS %in% c("1073"), "01073", FIPS)) %>%
  group_by(FIPS, lou_category) %>%
  summarize(amount = sum(amount, na.rm = TRUE), .groups = "drop") %>%
  complete(FIPS, lou_category, fill = list(amount = 0)) %>%
  filter(lou_category %in% top_groups) %>%
  left_join(FIPS_info, by = "FIPS")

unallocated_rows <- summary_df %>%
  group_by(FIPS) %>%
  summarize(amount = sum(amount), .groups = "drop") %>%
  left_join(governments_wide, by = "FIPS") %>%
  mutate(unallocated = total_allocation - amount) %>%
  transmute(FIPS, lou_category = "Unallocated", amount = unallocated)
  
summary_df %<>%
  select(FIPS, lou_category, amount) %>%
  bind_rows(unallocated_rows) %>%
  left_join(FIPS_info, by = "FIPS") %>%
  select(city, lou_category, amount)

peer_df <- summary_df %>%
  filter(city != "Louisville") %>%
  group_by(lou_category) %>%
  summarize(
    `25th Percentile` = quantile(amount, 0.25),
    `Peer Average` = mean(amount),
    `75th Percentile` = quantile(amount, 0.75))
  
peer_cities <- summary_df %>%
  filter(city != "Louisville")

summary_df %<>%
  pivot_wider(names_from = "city", values_from = "amount")

issue_plot <- plot_ly(peer_df) %>%
  
  add_trace(
    data = pivot_longer(select(summary_df, -Louisville), Birmingham:`St. Louis`),
    x = ~value, y = ~lou_category, split = ~name, type = 'scatter',
    mode = "markers", marker = list(color = "#979797"), showlegend = FALSE) %>%
  
  add_trace(
    data = peer_df,
    x = ~`Peer Average`, y = ~lou_category, name = "Peer Average", type = 'scatter',
    mode = "markers", marker = list(color = "#000000",
                                    symbol = 142,
                                    size = 15)) %>%  
  
  add_trace(
    data = peer_df,
    x = ~`25th Percentile`, y = ~lou_category, name = "25th Percentile", type = 'scatter',
    mode = "markers", marker = list(color = "#979797",
                                    symbol = 142,
                                    size = 10)) %>%  
  
  add_trace(
    data = peer_df,
    x = ~`75th Percentile`, y = ~lou_category, name = "75th Percentile", type = 'scatter',
    mode = "markers", marker = list(color = "#979797",
                                    symbol = 142,
                                    size = 10)) %>%  
  

  
  add_trace(
    data = summary_df,
    x = ~Louisville, y = ~lou_category, name = "Louisville", type = 'scatter',
    mode = "markers", marker = list(color = "#00A9B7",
                                    size = 15)) %>%

  layout(yaxis = list(title = "",
                      gridcolor = "#BEBEBE",
                      categoryorder = "array",
                      categoryarray = rev(c(top_groups, "Unallocated"))),
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

# ced_df <- data.frame(
#   type = c("Cost-Burdened Residents", "CED Program Participants"),
#   Black = c(29, 54),
#   White = c(63, 26),
#   Other = c(8, 20))
# 
# # Housing demograhpics
# plotly_CED_chart <- plot_ly(ced_df,x = ~type, y = ~White, type = 'bar',name= 'White') %>%
#   add_trace(y = ~Black, name = 'Black') %>%
#   add_trace(y = ~Other, name = 'Other') %>%
#   layout(yaxis = list(title = 'Percentage (%)'),barmode = "stack")


