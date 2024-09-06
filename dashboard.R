source(file = "cbproj.R", local = T)
library(dplyr)
library(lubridate)
library(tidyr)


fwhlr_qtrend_bymaker <- function(){
  fwheelers_bymaker <- evsales_bymaker[(as.numeric(evsales_bymaker$year)>=2022 & 
                                          evsales_bymaker$vehicle_category == "4-Wheelers"), ]
  
  fwheelers_bymaker$date <- as.Date(fwheelers_bymaker$date, format = "%d-%b-%Y")
  fwheelers_bymaker$date <- as.Date(format(fwheelers_bymaker$date, "20%y-%m-%d"))
  fwheelers_bymaker <- fwheelers_bymaker %>% mutate(quarter = paste0(year, "-Q", quarter(date, fiscal_start = 4)))
  
  top5makers <- fwheelers_bymaker %>% group_by(maker) %>% 
    summarise(total_sold = sum(electric_vehicles_sold, na.rm = T)) %>% arrange(desc(total_sold)) %>% slice_head(n=5)
  
  quarterly_trend <- fwheelers_bymaker %>% group_by(maker, quarter) %>%
    summarize(total_sold = sum(electric_vehicles_sold, na.rm = TRUE)) %>% arrange(desc(total_sold))
  
  quarterly_trend <- quarterly_trend[quarterly_trend$maker %in% top5makers$maker, ]
  
  quarterly_trend <- quarterly_trend %>%
    mutate(quarter = factor(quarter, levels = unique(quarter[order(quarter)])))
  
  # Create the line plot
  plot_q <- ggplot(quarterly_trend, aes(x = quarter, y = total_sold, color = maker, group = maker)) +
    geom_line(linewidth = 1) +           # Add lines
    geom_point(size = 2) +          # Add points at each data point
    labs(title = "Quarterly Sales by Top 5 Makers",
         x = "Quarter", y = "Sales", color = "Maker") +         
    theme_minimal() +             
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size=12),
          plot.title = element_text(hjust = 0.5, family = "Comic Sans MS", size = 14, face = "italic"),
          plot.background = element_blank(), legend.position = c(0.05, 0.95))
  
  return (plot_q)
}

fwhlr_qtrend_bystate <- function(){
  fwheelers_bystate <- evsales_bystate[(as.numeric(evsales_bystate$year)>=2022 & 
                                         evsales_bystate$vehicle_category == "4-Wheelers"), ]
  fwheelers_bystate$date <- as.Date(fwheelers_bystate$date, format = "%d-%b-%Y")
  fwheelers_bystate$date <- as.Date(format(fwheelers_bystate$date, "20%y-%m-%d"))
  fwheelers_bystate <- fwheelers_bystate %>% mutate(quarter = paste0(year, "-Q", quarter(date, fiscal_start = 4)))
  
  top5states <- fwheelers_bystate %>% group_by(state) %>% 
    summarise(total_sold = sum(electric_vehicles_sold, na.rm = T)) %>% 
    arrange(desc(total_sold)) %>% slice_head(n = 5)
  
  quarterly_trend <- fwheelers_bystate %>% group_by(state, quarter) %>%
    summarize(total_sold = sum(electric_vehicles_sold, na.rm = TRUE)) %>% arrange(desc(total_sold))
  
  quarterly_trend <- quarterly_trend[quarterly_trend$state %in% top5states$state, ]
  
  quarterly_trend <- quarterly_trend %>%
    mutate(quarter = factor(quarter, levels = unique(quarter[order(quarter)])))
  
  # Create the line plot
  plot_q <- ggplot(quarterly_trend, aes(x = quarter, y = total_sold, color = state, group = state)) +
    geom_line(linewidth = 1) +           # Add lines
    geom_point(size = 2) +          # Add points at each data point
    labs(title = "Quarterly Sales by Top 5 States",
         x = "Quarter", y = "Sales", color = "State") + 
    theme_minimal() +                # A clean theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size=12),
          plot.title = element_text(hjust = 0.5, family = "Comic Sans MS", size = 14, face = "italic"),
          plot.background = element_blank(), legend.position = c(0.05, 0.95))
  
  return (plot_q)
}


# States with negative penetration/ decline in sales

sales_decline <- function(){
  evsales_peryear <- evsales_bystate[as.numeric(evsales_bystate$year) <2024, ] %>% group_by(year, state) %>% 
    summarise(ev_sold = sum(electric_vehicles_sold))

  wide_data <- evsales_peryear %>%
    pivot_wider(names_from = year, values_from = ev_sold)

  wide_data <- wide_data %>%
    mutate(`Decline (2023-2022)` = `2023` - `2022`, 
           `Decline percentage` = (`2023` - `2022`)/`2022` * 100)

  # Filter for states with a decline
  decline_states <- wide_data[, -2] %>% filter(`Decline (2023-2022)` < 0)
  return (decline_states)
}

# Makers with reduction in sales volume: 2 & 4 wheelers combined

maker_decline <- function(){
  evsales_peryear <- evsales_bymaker[as.numeric(evsales_bymaker$year) < 2024, ] %>% group_by(year, maker) %>% 
    summarise(ev_sold = sum(electric_vehicles_sold))
  
  wide_data <- evsales_peryear %>% pivot_wider(
    names_from = year, values_from = ev_sold)
  
  wide_data <- wide_data %>% mutate(
    `Decline (2022-2023)` = `2023` - `2022`,
     `Decline percentage` = (`2023` - `2022`)/`2022` * 100
  )
  
  decline_makers <- wide_data[, c(1, 3:6)] %>% filter(`Decline (2022-2023)` < 0)
  
  return (decline_makers)
}


# X vs Y

states_compare <- function(state1, state2) {
  sales <- evsales_bystate[evsales_bystate$state %in% c(state1, state2) & 
                             as.numeric(evsales_bystate$year) == 2023, ]
  
  sales <- sales %>% group_by(month, state) %>% summarise(electric_vehicles_sold = sum(electric_vehicles_sold))
  sales$month <- factor(sales$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  plot_s <- ggplot(sales, aes(x = month, y = electric_vehicles_sold, color = state, group = state)) + 
    geom_line() + geom_point() + 
    labs(title = glue::glue("EV Sales and Penetration rates {state1} vs {state2} in the year 2023"), 
        y = "EVs Sold", x = "Month") + 
    theme_bw() + 
    scale_fill_discrete() + 
    theme(
      legend.position = c(0.95, 0.95),
      axis.text.x = element_text(family = "Helvetica", size = 8, hjust = 1, angle = 45), 
      plot.title = element_text(family = "Comic Sans MS", size = 12, hjust = 0.5, face = "italic"), 
      plot.background = element_blank()
    )
  
  return (plot_s)
}

# filter & group maker data: month, year, both
filter_maker <- function(makers, groups) {
  filtered_makers <- evsales_bymaker[evsales_bymaker$maker %in% makers, ]
  filtered_makers <- filtered_makers %>% 
    group_by(across(all_of(groups)), maker, vehicle_category) %>% 
    summarise(`electric vehicles sold` = sum(electric_vehicles_sold))
  filtered_makers <- rename(filtered_makers, `vehicle category` = vehicle_category)
  return (filtered_makers)
}

# filter & group state data: month, year, both
filter_state <- function(states, groups) {
  filtered_states <- evsales_bystate[evsales_bystate$state %in% states, ]
  filtered_states <- filtered_states %>% 
    group_by(across(all_of(groups)), state, vehicle_category) %>% 
    summarise(`electric vehicles sold` = sum(electric_vehicles_sold), 
              `total vehicles sold` = sum(total_vehicles_sold))
  filtered_states <- rename(filtered_states, `vehicle category` = vehicle_category)
  return (filtered_states)
}
