unique(cleaned_data$Fulfilment)


fulfillment_performance <- cleaned_data %>%
  group_by(Fulfilment) %>%
  summarise(
    total_orders = n(),
    avg_amount = mean(Amount, na.rm = TRUE),
    cancellation_rate = mean(Status == "Cancelled")
  )

print(fulfillment_performance)


top_products <- cleaned_data %>%
  group_by(SKU) %>%
  summarise(total_sales = sum(Amount, na.rm = TRUE)) %>%
  arrange(desc(total_sales)) %>%
  head(10)

ggplot(top_products, aes(x = reorder(SKU, total_sales), y = total_sales)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("SKU") +
  ylab("Total Sales") +
  ggtitle("Top 10 Products by Sales")





