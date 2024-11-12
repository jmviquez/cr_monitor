#Read the tables with the model outputs ################################
#
library(terra)
library(RSQLite)
library(dplyr)
library(data.table)
library(tidyr)

#Load the basins shapefile######
vect_basins<-vect("shapes/Catchments_CostaRica.geojson")
#Load the dataframes_by scenario###### 
q<-fread("Data/85_saul/run4/85_1981-2060timeCOUT.txt",header = T,skip=1)
tail(AET)<-fread("Data/85_saul/run4/85_1981-2060timeEVPT.txt",header = T,skip=1)
PET<-fread("Data/85_saul/run4/85_1981-2060timeEPOT.txt",header = T)
prec<-fread("Data/85_saul/pcp85_mean_by_basin.txt",header = T)
t<-fread("Data/85_saul/Tmean85_mean_by_basin.txt",header = T)
#Creates a sting ranked by date with "station" being the indicator of the basin
data_Q<-pivot_longer(q,col=-DATE,names_to="subid",values_to = "Q")
data_AET<-pivot_longer(AET,col=-DATE,names_to="subid",values_to = "AET")
data_PET<-pivot_longer(PET,col=-DATE,names_to="subid",values_to = "PET")
data_Prec<-pivot_longer(prec,col=-Date,names_to="subid",values_to = "Prec")
data_Tmean<-pivot_longer(t,col=-Date,names_to="subid",values_to = "Tmean")
#make the tables and the SQL############################################################################
con<-dbConnect(SQLite(), dbname = "Data85.sqlite")
dbWriteTable(con, "data_Q", data_Q)
dbWriteTable(con, "data_AET", data_AET)
dbWriteTable(con, "data_PET", data_PET)
dbWriteTable(con, "data_Prec", data_Prec)
dbWriteTable(con, "data_Tmean", data_Tmean)

dbDisconnect(con)

#########################################################################################################
con <- dbConnect(SQLite(), dbname = "Data85.sqlite")
tables_in_db <- dbListTables(con)

#Function to call all the info from the database#########################################################

query_all_tables_by_subid <- function(con, subid) {
  tables_in_db <- dbListTables(con)
    result_list <- list()
  for (table in tables_in_db) {
    # SQL query to select rows where subid matches the given value
    query <- paste0("SELECT * FROM ", table, " WHERE subid = ", subid)
    result <- dbGetQuery(con, query)
    result_list[[table]] <- result
  }
  return(result_list)
}

#Update dates########################################################################################################
update_dates <- function(df) {
  # Create date sequence
  start_date <- as.Date("2006-01-01")
  end_date <- as.Date("2050-01-01")
  date_seq <- seq(from = start_date, to = end_date, by = "day")
  
  # Crop and attach the date sequence to the dataframe
  df <- df[1:length(date_seq), ]
  df$DATE <- date_seq
  
  return(df)
}
df<-update_dates(result_list$data_AET)
#########################################################################################################
library(ggplot2)
library(dplyr)
plot_yearly_data <- function(data, value_col) {
  data<-update_dates(data)
  data <- data %>%
    mutate(Year = format(DATE, "%Y")) %>%  # Extract year
    group_by(Year) %>%
    summarise(mean_value = mean(!!sym(value_col), na.rm = TRUE)) 
  p <- ggplot(data, aes(x = as.numeric(Year), y = mean_value)) +
    geom_line(linewidth = 1) + 
    labs(title = paste("Yearly Mean of", value_col),
         x = "Year", y = paste("Mean", value_col)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),  
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  # Return the plot
  return(p)
}
#HeatMap########################################################################################################
plot_monthly_heatmap <- function(data, value_col) {
  # Ensure the dates are updated with your custom update_dates function
  data <- update_dates(data)
  
  # Extract the year and month, and calculate the monthly averages
  data <- data %>%
    mutate(Year = format(DATE, "%Y"),       # Extract year
           Month = format(DATE, "%m")) %>%  # Extract month
    group_by(Year, Month) %>%
    summarise(mean_value = sum(!!sym(value_col), na.rm = TRUE))  # Calculate mean for each month and year
  
  # Create the heatmap
  p <- ggplot(data, aes(x = as.numeric(Year), y = Month, fill = mean_value)) +
    geom_tile(color = "white") +   # Create the tiles for the heatmap
    scale_fill_viridis_c(name = paste("Mean", value_col)) +  # Use a color scale for fill
    labs(title = paste("Monthly Mean of", value_col, "by Year"),
         x = "Year", y = "Month") +
    theme_minimal(base_size = 15) +  # Apply minimal theme with larger font
    theme(
      plot.title = element_text(hjust = 0.5),  # Center the title
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
    )
  
  # Return the plot
  return(p)
}


#yearly########################################################################################################

plot_yearly_data(result_list$data_AET, value_col = "AET")
plot_yearly_data(result_list$data_Tmean, value_col = "Tmean")

heatmap_plot <- plot_monthly_heatmap(result_list$data_Q, value_col = "Q")
heatmap_plot
#########################################################################################################
#########################################################################################################
library(ggplot2)
library(dplyr)
library(plotly)
plot_10year_monthly_data <- function(data, value_col) {
  data <- update_dates(data)
  data <- data %>%
    mutate(Year = as.numeric(format(DATE, "%Y")),    # Extract year
           Month = as.numeric(format(DATE, "%m")),   # Extract month as numeric for plotting
           Decade = floor(Year / 10) * 10) %>%       # Group by decades (2000, 2010, etc.)
    group_by(Decade, Month) %>%
    summarise(mean_value = mean(!!sym(value_col), na.rm = TRUE))  # Calculate mean by decade and month
  
  p <- plot_ly(data, x = ~Month, y = ~mean_value, color = ~as.factor(Decade),
               type = 'scatter', mode = 'lines+markers', 
               marker = list(size = 8), 
               line = list(width = 2)) %>%
    layout(title = list(text = paste("Monthly Mean of", value_col, "by Decade")),
           xaxis = list(title = "Month", tickvals = 1:12, ticktext = month.name, tickangle = 45),  
           yaxis = list(title = paste("Mean", value_col)),
           legend = list(title = list(text = "Decade")),
           plot_bgcolor = '#f9f9f9',  
           paper_bgcolor = '#f9f9f9', 
           hovermode = "closest")  
  
  return(p)
}

plot_10year_monthly_data(result_list$data_Q, value_col = "Q")

#########################################################################################

con <- dbConnect(SQLite(), dbname = "Data85.sqlite")
result_list <- query_all_tables_by_subid(con, subid = 207)
tables_in_db <- dbListTables(con)
plot_yearly_data(result_list$data_Tmean, value_col = "Tmean")
plot_yearly_data(result_list$data_Prec, value_col = "Prec")

plot_10year_monthly_data(result_list$data_Q, value_col = "Q")

