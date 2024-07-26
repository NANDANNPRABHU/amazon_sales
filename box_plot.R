# Load necessary libraries
library(dplyr)
library(ggplot2)

# Read the data from the CSV file
data <- read.csv("report.csv")

# Ensure the Date column is of Date type with the correct format
data$Date <- as.Date(data$Date, format = "%m-%d-%y")

# Remove rows with missing or invalid dates
data <- data %>%
  filter(!is.na(Date))

# Display the dimensions of the dataset
print(dim(data))

# Display the structure of the dataset
str(data)

# Calculate total orders by state before cleaning
total_orders_by_state <- data %>%
  group_by(ship.state) %>%
  summarise(total_orders = n())

# Display total orders by state
print(total_orders_by_state)

# Display unique state names before cleaning
print(unique(data$ship.state))

# Create a named vector of corrections for common variations or misspellings
state_corrections <- c(
  "Gujarat" = "GUJARAT",
  "Goa" = "GOA",
  "goa" = "GOA",
  "delhi" = "DELHI",
  "Delhi" = "DELHI",
  "New Delhi" = "DELHI",
  "RAJASTHAN" = "RAJASTHAN",
  "Rajshthan" = "RAJASTHAN",
  "rajsthan" = "RAJASTHAN",
  "Rajsthan" = "RAJASTHAN",
  "punjab" = "PUNJAB",
  "Punjab" = "PUNJAB",
  "PB" = "PUNJAB",
  "Punjab/Mohali/Zirakpur" = "PUNJAB",
  "Orissa" = "ODISHA",
  "orissa" = "ODISHA",
  "Pondicherry" = "PUDUCHERRY",
  "Puducherry" = "PUDUCHERRY",
  "Arunachal pradesh" = "ARUNACHAL PRADESH",
  "Arunachal Pradesh" = "ARUNACHAL PRADESH",
  "Arunachal Pradesh" = "ARUNACHAL PRADESH",
  "Dadra And Nagar Haveli" = "DADRA AND NAGAR",
  "Andaman & Nicobar" = "ANDAMAN & NICOBAR",
  "NL" = "NAGALAND",
  "AR" = "ARUNACHAL PRADESH",
  "APO" = "ANDHRA PRADESH",
  "HP" = "HIMACHAL PRADESH",
  "J & K" = "JAMMU & KASHMIR",
  "JK" = "JAMMU & KASHMIR",
  "Lakshadweep" = "LAKSHADWEEP"
)

# Convert all state names to upper case for consistency
data$ship.state <- str_to_upper(data$ship.state)

# Apply the corrections for common variations or misspellings
data$ship.state <- recode(data$ship.state, !!!state_corrections)

# Remove leading and trailing whitespaces
data$ship.state <- str_trim(data$ship.state)

# Display unique state names after cleaning
print(unique(data$ship.state))

# List of valid state names in India
valid_states <- c(
  "MAHARASHTRA", "KARNATAKA", "PUDUCHERRY", "TAMIL NADU", "UTTAR PRADESH", "CHANDIGARH",
  "TELANGANA", "ANDHRA PRADESH", "RAJASTHAN", "DELHI", "HARYANA", "ASSAM",
  "JHARKHAND", "CHHATTISGARH", "ODISHA", "KERALA", "MADHYA PRADESH", "WEST BENGAL",
  "NAGALAND", "GUJARAT", "UTTARAKHAND", "BIHAR", "JAMMU & KASHMIR", "PUNJAB",
  "HIMACHAL PRADESH", "ARUNACHAL PRADESH", "MANIPUR", "GOA", "MEGHALAYA", "TRIPURA",
  "LADAKH", "DADRA AND NAGAR", "SIKKIM", "ANDAMAN & NICOBAR", "MIZORAM",
  "LAKSHADWEEP"
)

# Filter the dataset to keep only valid states
cleaned_data <- data %>%
  filter(ship.state %in% valid_states)

# Display unique state names after filtering
print(unique(cleaned_data$ship.state))

# Display the dimensions of the cleaned dataset
print(dim(cleaned_data))

# Display the dimensions of the original dataset
print(dim(data))

# Save the cleaned data to a CSV file
write.csv(cleaned_data, "cleaned_data.csv", row.names = FALSE)

# Calculate total orders by state after cleaning
total_orders_by_state <- cleaned_data %>%
  group_by(ship.state) %>%
  summarise(total_orders = n()) %>%
  arrange(desc(total_orders))
total_orders_by_state

# Plot the total orders by state
ggplot(total_orders_by_state, aes(x = reorder(ship.state, -total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("State") +
  ylab("Total Orders") +
  ggtitle("Total Orders by State") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Filter the dataset to include only orders from Karnataka and Maharashtra
filtered_data <- cleaned_data %>%
  filter(ship.state %in% c("KARNATAKA", "MAHARASHTRA") & is.finite(Amount))

# Save the filtered data to CSV files
write.csv(filtered_data %>% filter(ship.state == "KARNATAKA"), "karnataka_data.csv", row.names = FALSE)
write.csv(filtered_data %>% filter(ship.state == "MAHARASHTRA"), "maharashtra_data.csv", row.names = FALSE)

# Plot the box plot for Karnataka
ggplot(filtered_data %>% filter(ship.state == "KARNATAKA"), aes(x = ship.state, y = Amount, fill = ship.state)) +
  geom_boxplot() +
  xlab("State") +
  ylab("Order Amount") +
  ggtitle("Order Amount Distribution in Karnataka") +
  theme_minimal() +
  scale_fill_manual(values = c("KARNATAKA" = "skyblue"))

# Plot the box plot for Maharashtra
ggplot(filtered_data %>% filter(ship.state == "MAHARASHTRA"), aes(x = ship.state, y = Amount, fill = ship.state)) +
  geom_boxplot() +
  xlab("State") +
  ylab("Order Amount") +
  ggtitle("Order Amount Distribution in Maharashtra") +
  theme_minimal() +
  scale_fill_manual(values = c("MAHARASHTRA" = "orange"))

# Identify outliers in the Amount column for Karnataka
karnataka_outliers <- boxplot.stats(filtered_data$Amount[filtered_data$ship.state == "KARNATAKA"])$out
print(paste("Number of outliers in Karnataka:", length(karnataka_outliers)))
print("Outliers in Karnataka:")
print(karnataka_outliers)

# Identify outliers in the Amount column for Maharashtra
maharashtra_outliers <- boxplot.stats(filtered_data$Amount[filtered_data$ship.state == "MAHARASHTRA"])$out
print(paste("Number of outliers in Maharashtra:", length(maharashtra_outliers)))
print("Outliers in Maharashtra:")
print(maharashtra_outliers)



# Function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Remove outliers from the Amount column for both states
filtered_data$Amount <- with(filtered_data, ifelse(ship.state == "KARNATAKA", remove_outliers(Amount), Amount))
filtered_data$Amount <- with(filtered_data, ifelse(ship.state == "MAHARASHTRA", remove_outliers(Amount), Amount))

# Filter out rows with NA values in the Amount column (i.e., the outliers)
filtered_data_no_outliers <- filtered_data %>%
  filter(!is.na(Amount))

# Plot the box plot for Karnataka and Maharashtra without outliers
ggplot(filtered_data_no_outliers, aes(x = ship.state, y = Amount, fill = ship.state)) +
  geom_boxplot() +
  xlab("State") +
  ylab("Order Amount") +
  ggtitle("Order Amount Distribution in Karnataka and Maharashtra (Without Outliers)") +
  theme_minimal() +
  scale_fill_manual(values = c("KARNATAKA" = "skyblue", "MAHARASHTRA" = "orange"), name = "State")

