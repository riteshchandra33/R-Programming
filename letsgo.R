library(tidyverse)
library(stringr)
library(micromapST)

cases<-read.csv("cases.csv")
deaths  <- read.csv(file='deaths.csv')

state<-read_csv("State_Pop_2019.csv")
nday <- 3
daygap <- 28

nams <- names(cases)
lastvar <- length(nams)
subs <- lastvar- daygap*((nday-1):0)
newnams <- c('Fips','County','Postal',fixDates(nams[subs]))
subs <- c(1:3,subs)
cases <- cases[,subs]
names(cases) <- newnams


nams <- names(deaths)
lastvar <- ncol(deaths)
subs <- lastvar- daygap*((nday-1):0)
newnams <- c('Fips','County','Postal',
             fixDates(nams[subs]))
subs <- c(1:3,subs)
deaths <- deaths[,subs]
names(deaths) <- newnams
lastDay<-newnams[length(newnams)]


cases <-  cases %>%
  filter(Fips != 2270 & 
           Fips != 6000 & Fips > 999)

deaths <-  deaths %>%
  filter(Fips != 2270 & 
           Fips != 6000 & Fips > 999)
cases
deaths

workPop <-select(County_2019_Pop,FIPS,Pop)
names(workPop) <- c('Fips','Pop')
cases <- left_join(cases,workPop,by='Fips')
deaths <- left_join(deaths,workPop,by='Fips')


nams <- names(cases)[4:(ncol(cases)-1)]
tmp <- pivot_longer(cases,all_of(nams),
                    names_to = "Days", values_to ="Counts")

cnty_cRates<- tmp %>% 
  mutate(Rates=100000*Counts/Pop) %>%
  select(!c(Pop,Counts))

# deaths
nams <-names(deaths)[4:(ncol(cases)-1)]
tmp <- pivot_longer(deaths,all_of(nams),
                    names_to = "Days", values_to ="Counts")

cnty_dRates<- tmp %>% 
  mutate(Rates=100000*Counts/Pop) %>%
  select(!c(Pop,Counts))

save(cnty_cRates,cnty_dRates,
     file = paste0('Cnty rates days=',nday,' gap=',daygap,' end=',lastDay,'.RData'))
data<-data.frame(cnty_dRates)
data$Days <- as.Date(gsub("MX", "", data$Days), format = "%d.%m.%Y")
df<-data.frame(data)
df
df$Days <- as.Date(df$Days)

# Extract the unique dates from the "data.Days" column
unique_dates <- unique(df$Days)

# Create unique column names based on the unique dates
column_names <- paste(unique_dates)

# Map the unique dates to their corresponding column names
date_to_column_mapping <- data.frame(Days = unique_dates, Column_Name = column_names)

# Merge the mapping back to the original data
df <- df %>%
  left_join(date_to_column_mapping, by = "Days")

# Pivot the data from long to wide format
result <- df %>%
  select(Postal, Rates, Column_Name) %>%
  pivot_wider(names_from = Column_Name, values_from = Rates, values_fill = NULL,values_fn=mean)

# Print the result
print(result)

data_f<-data.frame(result)

rownames(data_f)<-result$Postal
data_f

data_f2<-subset(data_f,select=-c(Postal))
data_f2
library(tidyr)
data_long <- gather(result, Date, Cases, -Postal)


# Create the line plot
ggplot(data_long, aes(x = Date, y = Cases, group = Postal, color = Postal)) +
  geom_line() +
  labs(
    title = "COVID-19 Cases Over Time by State",
    x = "Date",
    y = "Cases"
  ) +
  scale_x_discrete(breaks = c("2021-08-04", "2021-11-02", "2021-11-03"), labels = c("Aug 4, 2021", "Nov 2, 2021", "Nov 3, 2021")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(data_long, aes(x = Date, y = Postal, fill = Cases)) +
  geom_tile() +
  labs(
    title = "COVID-19 Cases Heatmap by State and Date",
    x = "Date",
    y = "State",
    fill = "Cases"
  ) +
  scale_fill_gradient(low = "cyan", high = "darkgreen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_long, aes(x = Postal, y = Cases, fill = Date)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "COVID-19 deaths Comparison by State and Date",
    x = "State",
    y = "Deaths",
    fill = "Date"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(data_long, aes(x = Date, y = Cases, fill = Date)) +
  geom_boxplot() +
  labs(
    title = "Box Plot of COVID-19 Deaths by Date",
    x = "Date",
    y = "Deaths",
    fill = "Date"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data_long, aes(x = Date, y = Cases, fill = Postal)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Stacked Bar Plot of COVID-19 Cases by State and Date",
    x = "Date",
    y = "Cases",
    fill = "State"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


total_deaths <- aggregate(Cases ~ Postal, data = data_long, sum)
ggplot(total_deaths, aes(x = Postal, y = Cases, fill = Postal)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total COVID-19 Deaths by State Across Three Dates",
    x = "State",
    y = "Total Deaths",
    fill = "State"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data_long, aes(x = Postal, y = Cases, fill = Date)) +
  geom_boxplot() +
  labs(
    title = "Box Plots of COVID-19 Cases by State (Colored by Date)",
    x = "State",
    y = "Cases",
    fill = "Date"
  ) +
  scale_fill_brewer(palette = "Set3") +  # You can change the color palette here
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

ggplot(data_long, aes(x = Postal, y = Cases)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(
    title = "Box Plots of COVID-19 Cases by State",
    x = "State",
    y = "Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  








data_matrix <- as.matrix(data_f2)

# Set the column names for the matrix
data_array <- array(data_matrix, dim = c(nrow(data_matrix), ncol(data_matrix), 4), dimnames = list(rownames(data_matrix), colnames(data_matrix)))

new<-data.frame(data_f2)
replace_cell_values <- function(df) {
  for (col in colnames(df)) {
    df[, col] <- col
  }
  return(df)
}
new_data<-replace_cell_values(new)


new_data
data_matrix2<-as.matrix(new_data)
data_array2=array(data_matrix2, dim = c(nrow(data_matrix2), ncol(data_matrix2), 1), dimnames = list(rownames(data_matrix2), colnames(data_matrix2)))

data_array2

combined_data <- data.frame(data_array, data_array2)

# Convert the data frame to a matrix
combined_matrix <- as.matrix(combined_data)

# Transpose the matrix if needed (depends on the desired orientation)
# combined_matrix <- t(combined_matrix)

# Print the combined matrix
print(combined_matrix)

array2=array(c(data_array,data_array2),dim=c(nrow(combined_matrix),ncol(combined_matrix),2))

panelDesc   <- data.frame(                    
  type=c("maptail","id","tsconf"),      
  lab1=c("","","Time Series"),  
  lab2=c("","","Annual Rate"), 
  lab3=c("","","Years"), 
  
  col1=c(NA,NA,NA),        
  panelData =c(NA,NA,"data_array")
)

pdf(file="8.pdf",width=7.5,height=10)

micromapST(data_f2,panelDesc,sortVar="X2021.08.04",ascend=FALSE)  

dev.off()

ggplot(data_long, aes(x = Date, y = Cases, fill = Postal)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Grouped Bar Plot of COVID-19 Cases by State and Date",
    x = "Date",
    y = "Cases",
    fill = "State"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

ggplot(data_long, aes(x = Cases)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(
    title = "Histogram of COVID-19 Cases",
    x = "Cases",
    y = "Frequency"
  ) +
  theme_minimal()





library(ggplot2)
library(sf)

# Assuming 'spatial_data' is an sf object with geometry and 'Cases' column
# Replace 'spatial_data' with your spatial dataset
spatial_data <- 
# Create a choropleth map
ggplot() +
  geom_sf(df, aes(fill = cases)) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Choropleth Map of COVID-19 Cases",
    fill = "Cases"
  ) +
  theme_minimal()

ggplot(county_summary, aes(x = Mean_Case_Rate)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(
    title = "Distribution of Mean Case Rates",
    x = "Mean Case Rate",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.margin = margin(40, 20, 20, 20)
  )

library(ggplot2)

ggplot(data_long, aes(x = Postal, y = Cases)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(
    title = "Box Plots of COVID-19 Cases by State",
    x = "State",
    y = "Cases"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # Center-align the title
  ) +
  guides(fill = guide_legend(title = "Date"))  # Add a legend title

library(ggplot2)

ggplot(data_long, aes(x = Postal, y = Cases, fill = Cases)) +
  geom_tile() +
  labs(
    title = "Heatmap of COVID-19 Cases by State",
    x = "State",
    y = "Date",
    fill = "Cases"
  ) +
  scale_fill_gradient(low = "red", high = "blue") +  # Adjust color scale
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # Center-align the title
  )
library(ggplot2)

ggplot(data_long, aes(x = Postal, y = Cases)) +
  geom_point(aes(color = Cases), size = 3) +
  labs(
    title = "Scatter Plot of COVID-19 Cases by State",
    x = "State",
    y = "Cases",
    color = "Cases"
  ) +
  scale_color_gradient(low = "white", high = "blue") +  # Adjust color scale
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # Center-align the title
  )



