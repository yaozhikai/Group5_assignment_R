library(readxl)
library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

# Define a list of file paths for each month
monthly_list <- c("1.xlsx", "2.xlsx", "3.xlsx", "4.xlsx", "5.xlsx", "6.xlsx", "7.xlsx")

# Read and clean the "Table 5" data from all files
all_tables <- lapply(monthly_list, function(file) {
  read_excel(file, sheet = "Table 5")
})

# Write the cleaned data into a new Excel file with multiple sheets
sheet_names <- c("January", "February", "March", "April", "May", "June", "July")
output_file <- "final_combined_table_5.xlsx"
wb <- createWorkbook()

# Write data to different sheets
for (i in seq_along(all_tables)) {
  addWorksheet(wb, sheet_names[i])
  writeData(wb, sheet = sheet_names[i], all_tables[[i]])
}
saveWorkbook(wb, output_file, overwrite = TRUE)
print(head(filtered_data, 20))


# Extract rows 7-11, clean, and combine data from all sheets
all_data <- lapply(seq_along(sheet_names), function(i) {
  table_5 <- read.xlsx(output_file, sheet = sheet_names[i])
  filtered_rows <- table_5[7:11, -1]
  filtered_rows <- cbind(Month = sheet_names[i], filtered_rows)
  filtered_rows <- filtered_rows[, -c(ncol(filtered_rows)-1, ncol(filtered_rows))]
  filtered_rows
})

combined_data <- do.call(rbind, all_data)

# Rename columns and save the combined data
colnames(combined_data)[2:7] <- c("Travel purpose", "2020", "2021", "2022", "2023", "2024")

# Write the combined data to a new sheet
addWorksheet(wb, "Filtered_Data")
writeData(wb, sheet = "Filtered_Data", combined_data)
saveWorkbook(wb, output_file, overwrite = TRUE)
print(head(filtered_data, 10))


# Ensure year columns are numeric
combined_data <- combined_data %>%
  mutate(across(c("2020", "2021", "2022", "2023", "2024"), ~ as.numeric(as.character(.))))

# Monthly Travel Purpose Trends by Year
combined_data$Month <- factor(combined_data$Month, levels = sheet_names)
long_data <- combined_data %>%
  pivot_longer(cols = c("2020", "2021", "2022", "2023", "2024"), names_to = "Year", values_to = "Value")
ggplot(long_data, aes(x = Month, y = Value, color = Year, group = Year)) +
  geom_line() +
  facet_wrap(~ `Travel purpose`, scales = "free_y") +
  labs(title = "Monthly Travel Purpose Trends by Year", 
       x = "Month", 
       y = "Number of Visitors", 
       color = "Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)

# 'Holiday' vs Other Travel Purposes by Year and Month
ggplot(long_data, aes(x = Month, y = Value, color = `Travel purpose`, group = interaction(Year, `Travel purpose`))) +
  geom_line(aes(linetype = ifelse(`Travel purpose` == "Holiday", "solid", "dashed")), size = 0.8) +
  scale_color_manual(values = c("Holiday" = "red", "Business" = "blue", "Conferences & conventions" = "green",
                                "Visiting friends & relatives" = "purple", "Education" = "orange")) +
  facet_wrap(~ Year, scales = "free_y") +
  labs(title = "Holiday vs Other Travel Purposes by Year and Month", 
       x = "Month", 
       y = "Number of Visitors", 
       color = "Travel Purpose") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma) 

#Comparison of Travel Purposes Across Years by Year Ended July
# Clean and analyze data from "Year Ended July" like before
year_ended_july <- "7.xlsx"
new_file <- "year_ended_july_new.xlsx"
table_6_data <- read_excel(year_ended_july, sheet = "Table 6")
print(head(table_6_data, 20))
processed_data <- table_6_data[11:15, -1]
processed_data <- processed_data[, -c(ncol(processed_data)-1, ncol(processed_data))]
colnames(processed_data) <- c("Travel purpose", "2020", "2021", "2022", "2023", "2024")
processed_data <- processed_data %>%
  mutate(across(c("2020", "2021", "2022", "2023", "2024"), ~ as.numeric(as.character(.))))
wb_new <- createWorkbook()
addWorksheet(wb_new, "Year Ended July")
writeData(wb_new, sheet = "Year Ended July", processed_data)
saveWorkbook(wb_new, new_file, overwrite = TRUE)
print(head(ended_year_july, 10))

# Plot lines
ended_year_july <- read_excel(new_file, sheet = "Year Ended July")
long_data <- pivot_longer(ended_year_july, cols = c("2020", "2021", "2022", "2023", "2024"), names_to = "Year", values_to = "Value")
ggplot(long_data, aes(x = Year, y = Value, color = `Travel purpose`, group = `Travel purpose`)) +
  geom_line(size = 0.8) +
  geom_point() +
  labs(title = "Comparison of Travel Purposes Across Years by Year Ended July", 
       x = "Year", 
       y = "Number of Visitors", 
       color = "Travel Purpose") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma) 