# Load necessary libraries
library(dplyr)
library(stringr)
library(ggplot2)
# Read the data from the CSV file
data <- read.csv("report.csv")

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


# Calculate total orders by state after cleaning
total_orders_by_state <- cleaned_data %>%
  group_by(ship.state) %>%
  summarise(total_orders = n()) %>%
  arrange(desc(total_orders))
total_orders_by_state

#arrange(desc(total_orders_by_state))

# Plot the total orders by state
ggplot(total_orders_by_state, aes(x = reorder(ship.state, -total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("State") +
  ylab("Total Orders") +
  ggtitle("Total Orders by State") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Calculate total orders by status
order_status <- cleaned_data %>%
  group_by(Status) %>%
  summarise(total_orders = n()) %>%
  arrange(desc(total_orders))

# Filter to keep only the top 3 order statuses
top_3_order_status <- order_status %>%
  top_n(3, total_orders)

# Plot the top 3 order statuses
ggplot(top_3_order_status, aes(x = reorder(Status, -total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("Order Status") +
  ylab("Total Orders") +
  ggtitle("Order Status Distribution") +
  theme_minimal()
unique(cleaned_data$Status)








# Filter the dataset to include only orders from Karnataka and Maharashtra
combined_data <- cleaned_data %>%
  filter(ship.state %in% c("KARNATAKA", "MAHARASHTRA"))

# Calculate total orders by status for both states
order_status_combined <- combined_data %>%
  group_by(ship.state, Status) %>%
  summarise(total_orders = n()) %>%
  arrange(ship.state, desc(total_orders))

# Filter to keep only the top 3 order statuses by total orders for each state
top_3_order_status_combined <- order_status_combined %>%
  group_by(ship.state) %>%
  top_n(3, total_orders)

# Plot the top 3 order statuses for Karnataka and Maharashtra
ggplot(top_3_order_status_combined, aes(x = reorder(Status, -total_orders), y = total_orders, fill = ship.state)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Order Status") +
  ylab("Total Orders") +
  ggtitle("Order Status Distribution in Karnataka and Maharashtra") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", "orange"), name = "State") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))











# Calculate metrics for the entire dataset
overall_metrics <- cleaned_data %>%
  group_by(ship.state) %>%
  summarise(
    total_orders = n(),
    avg_amount = mean(Amount, na.rm = TRUE),
    cancellation_rate = mean(Status == "Cancelled")
  )

# Display overall metrics
print(overall_metrics)

# Plot the overall metrics for all states
ggplot(overall_metrics, aes(x = reorder(ship.state, -total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("State") +
  ylab("Total Orders") +
  ggtitle("Total Orders by State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(overall_metrics, aes(x = reorder(ship.state, -avg_amount), y = avg_amount)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("State") +
  ylab("Average Order Amount") +
  ggtitle("Average Order Amount by State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(overall_metrics, aes(x = reorder(ship.state, -cancellation_rate), y = cancellation_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("State") +
  ylab("Cancellation Rate") +
  ggtitle("Cancellation Rate by State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






# Calculate metrics for Karnataka and Maharashtra
filtered_metrics <- cleaned_data %>%
  filter(ship.state %in% c("KARNATAKA", "MAHARASHTRA")) %>%
  group_by(ship.state) %>%
  summarise(
    total_orders = n(),
    avg_amount = mean(Amount, na.rm = TRUE),
    cancellation_rate = mean(Status == "Cancelled")
  )

# Display metrics for Karnataka and Maharashtra
print(filtered_metrics)


# Plot the metrics for Karnataka and Maharashtra
ggplot(filtered_metrics, aes(x = reorder(ship.state, -total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "orange") +
  xlab("State") +
  ylab("Total Orders") +
  ggtitle("Total Orders in Karnataka and Maharashtra") +
  theme_minimal()

ggplot(filtered_metrics, aes(x = reorder(ship.state, -avg_amount), y = avg_amount)) +
  geom_bar(stat = "identity", fill = "orange") +
  xlab("State") +
  ylab("Average Order Amount") +
  ggtitle("Average Order Amount in Karnataka and Maharashtra") +
  theme_minimal()

ggplot(filtered_metrics, aes(x = reorder(ship.state, -cancellation_rate), y = cancellation_rate)) +
  geom_bar(stat = "identity", fill = "orange") +
  xlab("State") +
  ylab("Cancellation Rate") +
  ggtitle("Cancellation Rate in Karnataka and Maharashtra") +
  theme_minimal()








# Correct spellings of Bengaluru
bengaluru_variations <- c("BENGALURU", "Bengaluru", "bangalore", "Bangalore", "BANGALORE", "BANGALORE, KARNATAKA", 
                          "BANGALORE NORTH", "BENGALURU 560066", "BENGALURU-97", "BENGALURU -50015", "BENGALURE", 
                          "BENGALURU 560064", "BANGARAPET", "Bangalore North", "BangaLore", "Banglore", "BENGALUR", 
                          "bengaluru", "bANGALORE", "Bangalore Rural", "Bangalore Urban", "Bangalore 560062", 
                          "BANGALORE CITY 560002", "BENGALURU-560005", "BENGALURU-28", "Bangaluru", "BENGALURU,", 
                          "BENGALURU-560098", "BENGALURU-11", "Bangalore,", "Bangalore-78", "BENGALURU 560043", 
                          "BENGALURU 560037", "BENGALURU 560016", "BANGALORE18", "Bangalore-560 086", 
                          "BENGALOORU", "Bengaluru South", "Bengaluru east", "BENGALURU RURAL", "BENGALURU NORTH", 
                          "BENGALURU 560020", "Bengaluru 560020", "BENGALURU; 560047", "BENGALURu", 
                          "BENGALURU 562125", "BENGALURU 560085", "BENGALURU-560085", "Bangalore south", 
                          "Bangalore north", "Bengaluru 560064", "BENGALURU 560064", "Bangalore North")

# Replace all variations with "Bengaluru"
cleaned_data <- cleaned_data %>%
  mutate(ship.city = ifelse(ship.city %in% bengaluru_variations, "Bengaluru", ship.city))

karnataka_cities <- cleaned_data %>%
  filter(ship.state == "KARNATAKA" )

unique(karnataka_cities$ship.city)


# Calculate cancellation rate for Bengaluru city and non-Bengaluru cities within Karnataka
bengaluru_vs_non_bengaluru <- cleaned_data %>%
  filter(ship.state == "KARNATAKA") %>%
  mutate(city_group = ifelse(grepl("Bengaluru", ship.city, ignore.case = TRUE), "Bengaluru", "Non-Bengaluru")) %>%
  group_by(city_group) %>%
  summarise(
    total_orders = n(),
    avg_amount = mean(Amount, na.rm = TRUE),
    cancellation_rate = mean(Status == "Cancelled")
  )

bengaluru_vs_non_bengaluru


# Plot cancellation rate for Bengaluru vs Non-Bengaluru
ggplot(bengaluru_vs_non_bengaluru, aes(x = city_group, y = cancellation_rate, fill = city_group)) +
  geom_bar(stat = "identity") +
  xlab("City Group") +
  ylab("Cancellation Rate") +
  ggtitle("Cancellation Rate: Bengaluru vs Non-Bengaluru") +
  theme_minimal() +
  scale_fill_manual(values = c("Bengaluru" = "skyblue", "Non-Bengaluru" = "orange"))






# Ensure the Date column is of Date type with the correct format
data$Date <- as.Date(data$Date, format = "%m-%d-%y")

# Remove rows with missing or invalid dates
data <- data %>%
  filter(!is.na(Date))



# Calculate the number of orders over time
orders_over_time <- data %>%
  group_by(Date) %>%
  summarise(total_orders = n()) %>%
  arrange(Date)

# Determine the min and max dates in the dataset
min_date <- min(data$Date, na.rm = TRUE)
max_date <- max(data$Date, na.rm = TRUE)

print(min_date)
print(max_date)
# Plot the number of orders over time within the date range
ggplot(orders_over_time, aes(x = Date, y = total_orders)) +
  geom_line(color = "skyblue") +
  scale_x_date(limits = c(min_date, max_date)) +
  xlab("Date") +
  ylab("Total Orders") +
  ggtitle("Number of Orders Over Time") +
  theme_minimal()




# Calculate the total cancellations for normalization
total_cancellations <- sum(bengaluru_vs_non_bengaluru$cancellation_rate)

# Normalize the cancellation rates to sum to 100%
bengaluru_vs_non_bengaluru <- bengaluru_vs_non_bengaluru %>%
  mutate(normalized_cancellation_rate = cancellation_rate / total_cancellations * 100)

# Data preparation for pie chart
bengaluru_vs_non_bengaluru_pie <- bengaluru_vs_non_bengaluru %>%
  mutate(label = paste0(city_group, ": ", round(normalized_cancellation_rate, 1), "%"))

# Create the pie chart with normalized values
ggplot(bengaluru_vs_non_bengaluru_pie, aes(x = "", y = normalized_cancellation_rate, fill = city_group)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  xlab("") +
  ylab("") +
  ggtitle("Normalized Cancellation Rate: Bengaluru vs Non-Bengaluru") +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c("Bengaluru" = "skyblue", "Non-Bengaluru" = "orange"))



# Calculate cancellation rate by state
cancellation_rate_by_state <- cleaned_data %>%
  group_by(ship.state) %>%
  summarise(
    total_orders = n(),
    cancellations = sum(Status == "Cancelled", na.rm = TRUE),
    cancellation_rate = cancellations / total_orders * 100
  ) %>%
  arrange(desc(cancellation_rate))

# Display cancellation rate by state
print(cancellation_rate_by_state)

# Plot the cancellation rate by state
ggplot(cancellation_rate_by_state, aes(x = reorder(ship.state, -cancellation_rate), y = cancellation_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("State") +
  ylab("Cancellation Rate (%)") +
  ggtitle("Cancellation Rate by State") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




# Calculate average order amount and cancellation rate by state
price_cancellation_data <- cleaned_data %>%
  group_by(ship.state) %>%
  summarise(
    total_orders = n(),
    cancellations = sum(Status == "Cancelled", na.rm = TRUE),
    cancellation_rate = cancellations / total_orders * 100,
    avg_order_amount = mean(Amount, na.rm = TRUE)
  ) %>%
  arrange(desc(cancellation_rate))

# Display the data
print(price_cancellation_data)

# Plot the distribution of price vs cancellation rate
ggplot(price_cancellation_data, aes(x = avg_order_amount, y = cancellation_rate)) +
  geom_point(color = "skyblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  xlab("Average Order Amount") +
  ylab("Cancellation Rate (%)") +
  ggtitle("Distribution of Price vs Cancellation Rate ") +
  theme_minimal()






cleaned_data <- cleaned_data %>%
  filter(!is.na(Amount))

# Create bins for different price ranges
cleaned_data <- cleaned_data %>%
  mutate(price_range = cut(Amount, breaks = seq(0, max(Amount, na.rm = TRUE), by = 500), right = FALSE, labels = paste0(seq(0, max(Amount, na.rm = TRUE) - 500, by = 500), "-", seq(500, max(Amount, na.rm = TRUE), by = 500))))

# Calculate the cancellation rate for each price range
cancellation_rate_by_price_range <- cleaned_data %>%
  group_by(price_range) %>%
  summarise(
    total_orders = n(),
    cancellations = sum(Status == "Cancelled", na.rm = TRUE),
    cancellation_rate = cancellations / total_orders * 100
  )

# Display the cancellation rate by price range
print(cancellation_rate_by_price_range)

# Create a bar graph for cancellation rate by price range
ggplot(cancellation_rate_by_price_range, aes(x = price_range, y = cancellation_rate)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  xlab("Price Range (Rs)") +
  ylab("Cancellation Rate (%)") +
  ggtitle("Cancellation Rate by Price Range") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

