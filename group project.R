#####################################
#                                   #
#           Reading Data            #
#                                   #
#####################################

library(stringr)
retail_data <- read.csv("online_retail.csv",header=TRUE)
head(retail_data)
dim(retail_data) #contains 541909 obs, 8 columns
length(unique(retail_data$CustomerID)) #4373 unique customer IDs



#####################################
#                                   #
#          Data Cleaning            #
#                                   #
#####################################

#Convert CustomerID to factor
retail_data$CustomerID <- as.factor(retail_data$CustomerID)

#Convert Invoice date to character class first 
invoicedate <- as.character(retail_data$InvoiceDate)

#Creating a Date & Time column 
retail_data$Time <- format(as.POSIXct(invoicedate,format="%d/%m/%Y %H:%M"),"%H:%M")
retail_data$Date <- format(as.POSIXct(invoicedate,format="%d/%m/%Y %H:%M"),"%Y-%m-%d")
retail_data$InvoiceDate <- NULL

#Creating a TotalSpent column
retail_data$TotalSpent = retail_data$Quantity * retail_data$UnitPrice

#Exploring the occurence for each items
number <- table(retail_data$Description)
number <- data.frame(number)
colnames(number) <- c('Desciption', 'Freq')
number <- number[sort(number$Desciption),]

#remove all the problematic descriptions
number2 <- number[-c(14:28,58:59,152:156,167,174,179:186,321,323,598,603,649,655,695,
                     742:744,883,918,921,954:957,987,1001:1013,1061,1069:1070,1137:1151,
                     1153,1196,1293,1297,1418,1420:1426,1536,1681:1682,1790,1836:1838,
                     1839:1840,1901,1964,2068,2085:2087,2145:2146,2155,2170,2187,
                     2188,2256:2258,2261,2263,2288:2290,2312,2343,2378,2380,2465,2817:2818,
                     2830:2831,2877:2879,3031:3033,3113:3114,3132,3134:3136,3434:3435,
                     3570,3582:3590,3635:3636,3703:3704,3728:3729,3734:3738,3739,3744,
                     3835,3976:3978,3986,3989:3997,4167:4181),]
new_retail_data <- retail_data[retail_data$Description %in% number2[,1],]

missingCust <- new_retail_data %>% filter(is.na(CustomerID))
knownCust <- new_retail_data %>% filter(!is.na(CustomerID))
intersect(missingCust$InvoiceNo,knownCust$InvoiceNo)

#Since the intersect of the InvoiceNo for those with missing and known CustomerID is empty,
# there are no rows with missing CustomerID that can be filled using InvoiceNo.

#Below we split the dataset into those with descriptions and those without. 
missingDctn <- new_retail_data %>% filter(Description == "")
retail_data_w_description <- new_retail_data %>% filter(Description != "")
intersect(missingDctn$StockCode, retail_data_w_description$StockCode)

retail_data_DescandCID <- retail_data_w_description %>% filter(!is.na(CustomerID))
retail_data_w_description_wo_CID <- retail_data_w_description %>% filter(is.na(CustomerID))
#retail_data_w_description is split into those with CID and those without.
#retail_data_DescandCID is data that has BOTH Description and CID. (Use for Analysis where CustomerID is needed)


#For the data without descriptions, we fill in the descriptions using StockCode.
missingDctn %>% filter(!is.na(CustomerID))
#There are no products that are missing description and have a CustomerID.
dim(missingDctn)
#1454 rows without Descriptions

unknownDctn <- unique(subset(missingDctn,missingDctn$Description == "", "StockCode"))
foundDctn <- unique(subset(new_retail_data,StockCode %in% unknownDctn$StockCode & new_retail_data$Description != ""))
for(i in unknownDctn$StockCode) {
  if (i %in% foundDctn$StockCode) {
    missingDctn[missingDctn$StockCode == i,"Description"] <- subset(foundDctn,StockCode %in% i,"Description")[1,]
  }  
}
missingDctn %>% filter(Description == "") %>% dim()
#There are still 124 rows without Descriptions.

#Removing rows without descriptions
retail_filled_descriptions <- missingDctn %>% filter(Description != "")

retail_filled_descriptions %>% filter(UnitPrice == 0) %>% dim()
retail_filled_descriptions %>% dim()
#All 1330 rows in retail_filled_description have UnitPrice = 0.

unknownPrice <- unique(subset(retail_filled_descriptions,retail_filled_descriptions$UnitPrice == 0, "StockCode"))
foundPrice <- unique(subset(new_retail_data,StockCode %in% unknownPrice$StockCode & new_retail_data$UnitPrice != 0)) %>% arrange(StockCode,desc(Date, Time))
for(i in unknownPrice$StockCode) {
  if (i %in% foundPrice$StockCode) {
    retail_filled_descriptions[retail_filled_descriptions$StockCode == i,"UnitPrice"] <- subset(foundPrice,StockCode %in% i,"UnitPrice")[1,]
  }  
}

retail_filled <- retail_filled_descriptions %>% filter(UnitPrice!= 0)
#This is the data with filled descriptions and price

#Update the TotalSpent column
retail_filled$TotalSpent = retail_filled$Quantity * retail_filled$UnitPrice

retail_wo_CID <- rbind(retail_filled, retail_data_w_description_wo_CID)
#This is the all the data without CustomerID.
#Combine this with retail_data_DescandCID to obtain data that can be used for analysis that does not require CustomerID

#Removing those with very large quantity (>12000)
retail_data_DescandCID <- retail_data_DescandCID %>% filter(Quantity <= 12000)

#Removing those with quantity > 1600 & Unitprice==0
retail_data_DescandCID <- retail_data_DescandCID %>% filter(Quantity<1600 && UnitPrice!=0)

#Removing those with InvoiceNo. starting with "C"
retail_data_wo_cancelled <- retail_data_DescandCID %>% filter(!str_detect(InvoiceNo,"C"))

#Converting the Date column to a "DATE" class.
retail_data_DescandCID$Date <- as.Date(retail_data_DescandCID$Date) #for M
retail_data_wo_cancelled$Date <- as.Date(retail_data_wo_cancelled$Date) #for R & F 

#(start)dec 2010 - march 2011(end)
rfm_data_1_m <- retail_data_DescandCID %>% filter(Date <= as.Date("2011-03-31"))
rfm_data_1_rf <- retail_data_wo_cancelled %>% filter(Date <= as.Date("2011-03-31"))

#(start)april 2011 - july 2011(end)
rfm_data_2_m <- retail_data_DescandCID %>% filter(Date >= as.Date("2011-04-01")) %>% filter(Date < as.Date("2011-07-01")) 
rfm_data_2_rf <- retail_data_wo_cancelled %>% filter(Date >= as.Date("2011-04-01")) %>% filter(Date < as.Date("2011-07-01")) 

#(start)aug 2011 - dec 2011(end)
rfm_data_3_m <- retail_data_DescandCID %>% filter(Date >= as.Date("2011-07-01"))
rfm_data_3_rf <- retail_data_wo_cancelled %>% filter(Date >= as.Date("2011-07-01"))

#####################################
#                                   #
#            RFM Model              #
#                                   #
#####################################

#FIRST PERIOD: Dec 2010 - March 2011

# The following code sorts the transactions by date.
rfm_data_1_rf <- rfm_data_1_rf[order(rfm_data_1_rf$Date),]
rfm_data_1_m <- rfm_data_1_m[order(rfm_data_1_m$Date),]

head(rfm_data_1_rf) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_1 <- NULL
total_amount_1 <- NULL
since_prev_1 <- NULL

for (id in unique(rfm_data_1_rf$CustomerID)) {
  total_visits_1 <- c(total_visits_1, sum(rfm_data_1_rf$CustomerID == id))
  since_prev_1 <- c(since_prev_1, min(as.numeric(as.Date("2018/09/07")
                                             - rfm_data_1_rf$Date[rfm_data_1_rf$CustomerID == id])))
  total_amount_1 <- c(total_amount_1, sum(rfm_data_1_m$TotalSpent[rfm_data_1_m$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_1 <- data.frame(id=unique(rfm_data_1_rf$CustomerID),
                            total_visits=total_visits_1,
                            total_amount=total_amount_1,
                            since_prev=since_prev_1)
head(customer_data_1)
################################################################################
customers_1 <- data.frame(cid = unique(customer_data_1$id))
# The following commands assign the recency, frequency, and monetary value
# rating on a scale of 1-5 with 5 being the most recent, most frequent, most
# monetary value, and 1 being the least recent, least frequent and least
# monetary value.

# New R, F, M-score assigning function.
map_quantiles <- function(vect, num_groups=5) {
  ranks <- order(vect)
  result <- numeric(length(vect))
  one_unit <- floor(length(vect) / num_groups)
  for (index in 1:num_groups) {
    if (index == num_groups) {
      result[(index - 1) * one_unit < ranks] <- index
    } else {
      result[(index - 1) * one_unit < ranks & ranks <= index * one_unit] <- index
    }
  }
  result
}

customers_1$recency <- 6 - map_quantiles(customer_data_1$since_prev)

customers_1$frequency <- map_quantiles(customer_data_1$total_visits)

customers_1$amount <- map_quantiles(customer_data_1$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_1$rfm <- (customers_1$recency*100
                  + customers_1$frequency*10
                  + customers_1$amount)
head(customers_1)




################################################################################################
# SECOND PERIOD: April 2011 - July 2011


# The following code sorts the transactions by date.
rfm_data_2_rf <- rfm_data_2_rf[order(rfm_data_2_rf$Date),]
rfm_data_2_m <- rfm_data_2_m[order(rfm_data_2_m$Date),]

head(rfm_data_2_rf) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_2 <- NULL
total_amount_2 <- NULL
since_prev_2 <- NULL

for (id in unique(rfm_data_2_rf$CustomerID)) {
  total_visits_2 <- c(total_visits_2, sum(rfm_data_2_rf$CustomerID == id))
  since_prev_2 <- c(since_prev_2, min(as.numeric(as.Date("2018/09/07")
                                               - rfm_data_2_rf$Date[rfm_data_2_rf$CustomerID == id])))
  total_amount_2 <- c(total_amount_2, sum(rfm_data_2_m$TotalSpent[rfm_data_2_m$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_2 <- data.frame(id=unique(rfm_data_2_rf$CustomerID),
                              total_visits=total_visits_2,
                              total_amount=total_amount_2,
                              since_prev=since_prev_2)
head(customer_data_2)
################################################################################
customers_2 <- data.frame(cid = unique(customer_data_2$id))

customers_2$recency <- 6 - map_quantiles(customer_data_2$since_prev)

customers_2$frequency <- map_quantiles(customer_data_2$total_visits)

customers_2$amount <- map_quantiles(customer_data_2$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_2$rfm <- (customers_2$recency*100
                    + customers_2$frequency*10
                    + customers_2$amount)
head(customers_2)



##############################################################################################
# THIRD PERIOD: Aug 2011 - Dec 2011


# The following code sorts the transactions by date.
rfm_data_3_rf <- rfm_data_3_rf[order(rfm_data_3_rf$Date),]
rfm_data_3_m <- rfm_data_3_m[order(rfm_data_3_m$Date),]

head(rfm_data_3_rf) 

################################################################################
# Now, for each customer, we find the days between each subsequent purchase,
# the total number of visits made, and amount spent.
total_visits_3 <- NULL
total_amount_3 <- NULL
since_prev_3 <- NULL

for (id in unique(rfm_data_3_rf$CustomerID)) {
  total_visits_3 <- c(total_visits_3, sum(rfm_data_3_rf$CustomerID == id))
  since_prev_3 <- c(since_prev_3, min(as.numeric(as.Date("2018/09/07")
                                                 - rfm_data_3_rf$Date[rfm_data_3_rf$CustomerID == id])))
  total_amount_3 <- c(total_amount_3, sum(rfm_data_3_m$TotalSpent[rfm_data_3_m$CustomerID == id]))
}

#This means that those that only have cancelled orders will be left out in RFM calculation.


customer_data_3 <- data.frame(id=unique(rfm_data_3_rf$CustomerID),
                              total_visits=total_visits_3,
                              total_amount=total_amount_3,
                              since_prev=since_prev_3)
head(customer_data_3)
################################################################################
customers_3 <- data.frame(cid = unique(customer_data_3$id))

customers_3$recency <- 6 - map_quantiles(customer_data_3$since_prev)

customers_3$frequency <- map_quantiles(customer_data_3$total_visits)

customers_3$amount <- map_quantiles(customer_data_3$total_amount)

# The RFM score is then a concatenation of the above three scores. Here is its
# calculation:
customers_3$rfm <- (customers_3$recency*100
                    + customers_3$frequency*10
                    + customers_3$amount)
head(customers_3)


###############################################################################################
#Combining the rfm from each period

rfm_1 <- NULL
rfm_2 <- NULL
rfm_3 <- NULL
cid <- NULL

for (id in intersect(intersect(customers_1$cid,customers_2$cid),customers_3$cid)) {
  cid <- c(cid, id)
  rfm_1 <- c(rfm_1, (customers_1$rfm[customers_1$cid == id]))
  rfm_2 <- c(rfm_2, (customers_2$rfm[customers_2$cid == id]))
  rfm_3 <- c(rfm_3, (customers_3$rfm[customers_3$cid == id]))
}

combined_rfm <-data.frame(cid,
                          rfm_1=rfm_1,
                          rfm_2=rfm_2,
                          rfm_3=rfm_3)
