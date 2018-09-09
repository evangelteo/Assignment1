#####################################
#                                   #
#             Latency               #
#                                   #
#####################################

latency <-retail_data_wo_cancelled %>% as.tibble() %>% select(CustomerID, InvoiceNo, TotalSpent, Date)
latency <- latency %>% group_by(InvoiceNo) %>% mutate(TotalAmount = sum(TotalSpent))
latency$TotalSpent <- NULL

latency2 <- latency %>% unique() %>%arrange(Date)%>%group_by(CustomerID)%>%
  mutate(purchase_number=rank(Date,ties.method = "first"),total_purchases=n()) %>%filter(total_purchases>1)


find_avg_latency <- function(greater_purchase_num) {
  as.numeric((latency2%>%filter(total_purchases>=greater_purchase_num)%>%
                summarise(latency=nth(Date,greater_purchase_num)-nth(Date,greater_purchase_num-1))%>%summarise(mean(latency)))[[1]])}


one_to_two <- find_avg_latency(2)
two_to_three <- find_avg_latency(3)
three_to_four <- find_avg_latency(4)
four_to_five <- find_avg_latency(5) 
five_to_six <- find_avg_latency(6)
print(c(one_to_two, two_to_three, three_to_four, four_to_five,five_to_six))

between_purchases <- c("1 and 2", "2 and 3", "3 and 4", "4 and 5")
average_latency <- c(one_to_two, two_to_three, three_to_four, four_to_five,five_to_six)
cumulative_latency <- cumsum(average_latency)


latency_data <- tribble(
  ~Purchases,    
  "1 and 2",       
  "2 and 3",       
  "3 and 4",      
  "4 and 5",      
  "5 and 6",      
)

latency_data <- latency_data %>% cbind(average_latency) %>% cbind(cumulative_latency)
ggplot(data = latency_data) +
  geom_bar(mapping = aes(x = Purchases, y = average_latency), stat = "identity")

ggplot(data = latency_data) +
  geom_bar(mapping = aes(x = Purchases, y = cumulative_latency), stat = "identity")
