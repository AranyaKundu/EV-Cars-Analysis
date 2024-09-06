library(readxl)
library(dplyr)
library(ggplot2)
library(glue)

evsales_bymaker <- read.csv("./electric_vehicle_sales_by_makers.csv")
evsales_bymaker$year <- as.Date(evsales_bymaker$date, format = "%d-%b-%y") %>% format("%Y")
evsales_bymaker$month <- as.Date(evsales_bymaker$date, format = "%d-%b-%Y") %>% format("%b")

evsales_bystate <- read.csv("./electric_vehicle_sales_by_state.csv")
evsales_bystate$year <- as.Date(evsales_bystate$date, format = "%d-%b-%y") %>% format("%Y")
evsales_bystate$month <- as.Date(evsales_bystate$date, format = "%d-%b-%Y") %>% format("%b")

top_and_bottom_makers_by_wheels <- function(top_n, bottom_n, wheels_count, year){
  
  plot_data <- evsales_bymaker[(evsales_bymaker$year==year & evsales_bymaker$vehicle_category == wheels_count), ]
  plot_data <- plot_data %>% group_by(year, maker, vehicle_category) %>% 
    summarise(ev_sold = sum(electric_vehicles_sold))
  plot_data <- plot_data[order(plot_data$ev_sold, decreasing = T), ][c(1:top_n, (nrow(plot_data)-(bottom_n-1)):nrow(plot_data)), ]
  
  plot_1 <- ggplot(data = plot_data) + 
    geom_bar(aes(x = reorder(maker, -ev_sold), y=ev_sold, fill = maker), stat = "identity") +
    theme_bw() + 
    scale_fill_viridis_d() +
    labs(x = "Maker", y = "Electric Vehicles Sold", title = "Electric Vehicle Sales by Maker") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12, face = "italic"), legend.position = "none",
          axis.title = element_text(size=14),
          plot.title = element_text(hjust = 0.5, family = "Comic Sans MS", size = 16, face = "italic"),
          plot.background = element_blank())
  
  return (plot_1)
}

top_and_bottom_states_by_wheels <- function(top_n, bottom_n, wheels_count, year){
  plot_data2 <- evsales_bystate[(evsales_bystate$year == year & evsales_bystate$vehicle_category == wheels_count), ]
  plot_data2 <- plot_data2 %>% group_by(year, state, vehicle_category) %>% 
    summarise(ev_sold = sum(electric_vehicles_sold))
  plot_data2 <- plot_data2[order(plot_data2$ev_sold, 
                                 decreasing = T), ][c(1:top_n, 
                                                      (nrow(plot_data2)-bottom_n+1):nrow(plot_data2)), ]
  
  plot_2 <- ggplot(data = plot_data2) + 
    geom_bar(aes(x= reorder(state, -ev_sold), y=ev_sold, fill=state), stat="identity") +
    theme_bw()+
    scale_fill_viridis_d() +
    labs(x="State", y="Electric Vehicles Sold", title = "Electric Vehicle Sales by State") + 
    theme(axis.title = element_text(size = 14), 
          axis.text.x = element_text(angle=45, hjust = 1, size = 12, face = "italic"),
          plot.background = element_blank(), 
          plot.title = element_text(family = "Comic Sans MS",face = "italic", size = 16, hjust = 0.5),
          legend.position = "none")
  return (plot_2)

}

month_list <- unique(evsales_bymaker$month)
maker_list <- unique(evsales_bymaker$maker)
state_list <- unique(evsales_bystate$state)
year_list <- unique(evsales_bymaker$year)


selected_makers_plot <- function(makers, wheels_count, year, month){
  plot_data3 <- evsales_bymaker[(evsales_bymaker$maker %in% makers & 
                                   evsales_bymaker$vehicle_category == wheels_count & 
                                   evsales_bymaker$year %in% year & evsales_bymaker$month %in% month), ]
  
  plot_3 <- ggplot(plot_data3) + 
    geom_bar(aes(x = reorder(maker, -electric_vehicles_sold), y = electric_vehicles_sold, fill = maker), 
             stat="identity") +
    geom_text(aes(x = reorder(maker, -electric_vehicles_sold), y = electric_vehicles_sold, 
                  label = glue::glue("{month}-{year}"), 
                  group = maker), position = position_stack(vjust = 0.5), size = 6) +
    theme_bw() + 
    scale_fill_viridis_d() +
    labs(x="Maker", y="Electric Vehicles Sold", title = "Electric Vehicle Sales by Maker") + 
    theme(axis.title = element_text(size = 14), 
          axis.text.x = element_text(family = "Roboto", angle=45, hjust = 1, size = 12, face = "italic"),
          plot.background = element_blank(), 
          plot.title = element_text(family = "Comic Sans MS",face = "italic", size = 16, hjust = 0.5),
          legend.position = "none")
  
  return (plot_3)
  
}

selected_states_plot <- function(states, wheels_count, year, month) {
  plot_data4 <- evsales_bystate[(evsales_bystate$state %in% states &
                                   evsales_bystate$vehicle_category == wheels_count &
                                   evsales_bystate$year %in% year & evsales_bystate$month %in% month), ]
  
  plot_4 <- ggplot(plot_data4) + 
    geom_bar(aes(x = reorder(state, -electric_vehicles_sold), y=electric_vehicles_sold, fill = state), 
             stat = "identity") +
    geom_text(aes(x = reorder(state, -electric_vehicles_sold), y = electric_vehicles_sold, 
                  label = glue::glue("{month}-{year}"), 
                  group = state), position = position_stack(vjust = 0.5), size = 6) +
    theme_bw() + 
    scale_fill_viridis_d() +
    labs(x = "State", y = "Electric Vehicles Sold", title = "Electric Vehicle Sales by State") + 
    theme(axis.title = element_text(size = 14), 
          axis.text.x = element_text(family = "Roboto", angle=45, hjust = 1, size = 12, face = "italic"),
          plot.background = element_blank(), 
          plot.title = element_text(family = "Comic Sans MS",face = "italic", size = 16, hjust = 0.5),
          legend.position = "none")
  
  return (plot_4)
}
