#Student ID: L00005877
#CA 2- Data Science
#Credit Card Fraud Analysis 

# ------------------------------------------------------------------------------
# Load the dataset
# ------------------------------------------------------------------------------
fraud <- read.csv("C:/Users/brian/OneDrive/Documents/Education/ATU 2026/PostGrad in Big Data Analytics/Module 5 Data Science/CA/CA2/fraudTest.csv")
View(fraud)

# ------------------------------------------------------------------------------
# Clean the dataset
# ------------------------------------------------------------------------------

#display the column names
names(fraud)

#drop columns that are not required
fraud_data_1 <- subset(fraud, select =-c(X, cc_num, merchant, first, last,
              street, city, zip, lat, long, trans_num, unix_time, merch_lat, 
              merch_long, job, dob))

#check the columns that are left 
View(fraud_data_1)


# removes any rows that contains NA
new_fraud_data <- na.omit(fraud_data_1)
View(new_fraud_data)


# use complete cases to show rows where data is available
complete_data <- complete.cases(new_fraud_data)
complete_data


# show sum of completed rows
sum(complete_data)

# show sum of not completed rows
sum(!complete_data)

# note: no missing values found

# check for duplicate rows
sum(duplicated(new_fraud_data))

# note: no duplicates found



# ------------------------------------------------------------------------------
# Convert variables
# ------------------------------------------------------------------------------

# convert categorical variables to factors
  new_fraud_data$category <- as.factor(new_fraud_data$category)
  new_fraud_data$gender <- as.factor(new_fraud_data$gender)
  new_fraud_data$state <- as.factor(new_fraud_data$state)
  
#extract hour from existing variable trans_date_trans_time
  new_fraud_data$hour <- as.numeric(format(as.POSIXct(new_fraud_data$trans_date_trans_time), "%H"))
  
#check the variables    
str(new_fraud_data)
 
#drop the original variable trans_date_trans_time
new_fraud_data <- subset(new_fraud_data, select = -c(trans_date_trans_time))

#view the final variables
View(new_fraud_data)

#show sum of completed rows
sum(complete_data)

#Show sum of not completed rows
sum(!complete_data)

#note: no missing values found

# check for duplicate rows
sum(duplicated(new_fraud_data))

# note: 391 duplicates found now that some columns were dropped
# not going to drop them as they have just lost the unique date/time identifier
# they will not be actual duplicates, but duplicates on the remaining variables

#check the count again   
str(new_fraud_data)

#save the clean data
write.csv(new_fraud_data, "C:/Users/brian/OneDrive/Documents/Education/ATU 2026/PostGrad in Big Data Analytics/Module 5 Data Science/CA/CA2/clean_fraud_data.csv", row.names=FALSE)

# ------------------------------------------------------------------------------
# Data Analysis 
# ------------------------------------------------------------------------------

# is_fraud summary for Chart 1 
table(new_fraud_data$is_fraud)

# category count for Chart 2 
table(new_fraud_data$category)

# hour summary and count for Chart 3
summary(new_fraud_data$hour)
table(new_fraud_data$hour)

# amt summary for Chart 4
summary(new_fraud_data$amt)

# gender count for Chart 5
table(new_fraud_data$gender)

# city_pop summary for Chart 6
summary(new_fraud_data$city_pop)

# state count for Chart 7
summary(new_fraud_data$state)



# ------------------------------------------------------------------------------
#Chart 1: Transaction outcomes 
# ------------------------------------------------------------------------------

#find the number of 0 and 1 transactions
outcomes <- table(new_fraud_data$is_fraud)
View(outcomes)

#create a bar chart 
barplot(outcomes,
        names.arg = c(paste("Legitimate:", outcomes["0"]),
                      paste("Fraudulent:", outcomes["1"])
        ),
        main = "Number of Transactions by Outcome",
        xlab = "Transaction outcome",
        ylab = "Number of transactions",
        yaxt = "n", # hide the y axis as text is hardcoded above
        col = "blue")


# ------------------------------------------------------------------------------
#Chart 2: Fraud rate by category
# ------------------------------------------------------------------------------

#split into groups and get the mean
category_rates <- aggregate(is_fraud ~ category,data = new_fraud_data,FUN = mean)

View(category_rates)

# convert fraud rates to percentages
category_rates$fraud_percentage <- category_rates$is_fraud * 100

# display category and fraud percentage
#category_rates[, c("category", "fraud_percentage")]
View(category_rates)

#check the totals are correct
sum(table(new_fraud_data$category))


barplot(category_rates$is_fraud * 100,
        names.arg = category_rates$category,
        las = 2,
        main = "Fraud Rate by Category",
        xlab = "",
        ylab = "Fraud rate (%)",
        col = "red",
        cex.names = 0.75) #reduce text size 


# ------------------------------------------------------------------------------
#Chart 3: Fraud rate by hour
# ------------------------------------------------------------------------------

#split into groups and get the mean
hour_rates <- aggregate(is_fraud ~ hour,data = new_fraud_data,FUN = mean)

# convert fraud rates to percentages
hour_rates$fraud_percentage <- hour_rates$is_fraud * 100

# Display hour and fraud percentage
View(hour_rates)

#create the graph

plot(hour_rates$hour,
     hour_rates$is_fraud * 100,
     type = "b", #connect points
     main = "Fraud Rate by Hour",
     xlab = "Hour of day",
     ylab = "Fraud rate (%)",
     xaxt = "n") #dont set the x axis

 axis(1, at = 0:23) #set the hours on x axis
 

 
 
# ------------------------------------------------------------------------------
# Chart 4: Fraud Rate by Transaction Amount
# ------------------------------------------------------------------------------
 #what is the max value for breaks
 max(new_fraud_data$amt)
 
 #create transaction amount groups using slices
 new_fraud_data$amt_group <- cut(new_fraud_data$amt,
                               breaks = c(0,10,50,100,250,500,1000,25000),
                               labels = c("0-10","11-50","51-100","101-250","251-500","501-1000","1000+"))

  #split into groups and get the mean
  fraud_rates <- aggregate(is_fraud ~ amt_group,data = new_fraud_data,FUN = mean)
 
 # display fraud percentage for each transaction amount range
  fraud_rates$fraud_percentage <- fraud_rates$is_fraud * 100
    round(fraud_rates$fraud_percentage, 2) #round percentage to 2 decimal places
 
    View (fraud_rates)
 
 #  Create a bar chart 
 barplot(fraud_rates$fraud_percentage,
         names.arg = fraud_rates$amt_group,
         main = "Fraud Rate % by Amount",
         xlab = "Transaction Amount $", 
         ylab = "Fraud Rate %",
         col = "green",
         las = 1, #keep numbers horizontal
         ylim = c(0, 20)) #nothing higher than 20% so limit graph to this
 
 

# ------------------------------------------------------------------------------
# Chart 5: Fraud rate by gender
# ------------------------------------------------------------------------------
 
 # calculate fraud rates for each gender
 gender_rates <- aggregate( is_fraud ~ gender, data = new_fraud_data,FUN = mean)
 
 #display percentaages:
 round(gender_rates$is_fraud * 100, 2)
 
 # gender count for Chart 5
 table(new_fraud_data$gender)
 
 View(gender_rates)
 
 # Calculate fraud rates for each gender
barplot(
  gender_rates$is_fraud * 100,
  names.arg = c(
    paste("Female"),
    paste("Male")
  ),
  main = "Fraud Rate by Gender",
  xlab = "Gender",
  ylab = "Fraud Rate (%)",
  col = c("orange", "yellow"),
  ylim = c(0, 0.6),
  cex.names = 0.8
)





# ------------------------------------------------------------------------------
# Chart 6: Fraud rate by Population
# ------------------------------------------------------------------------------

#create population groups using slices
new_fraud_data$pop_group <- cut(new_fraud_data$city_pop,
                              breaks = c(0, 500, 1000, 2500, 10000, 50000, 200000, 1000000, 3000000),
                              labels = c("23-500", "501-1k", "1k-2.5k", "2.5k-10k", "10k-50k", "50k-200k", "200k-1M", "1M+"))

# group populations for each slice
pop_rates <- aggregate(is_fraud ~ pop_group, data = new_fraud_data, FUN = mean)

#display percentages after rounding
round(pop_rates$is_fraud * 100, 2)

# create a bar chart 
barplot(pop_rates$is_fraud * 100,
        names.arg = pop_rates$pop_group,
        main = "Fraud Rate by City Population",
        xlab = "City Population",
        ylab = "Fraud Rate (%)",
        col = "orange",
        ylim = c(0, 0.6), #cap at 0.6 for better view
        las = 2,
        cex.names = 0.7) #reduce size of text






# ------------------------------------------------------------------------------
# Chart 7: Fraud Rate by State
# ------------------------------------------------------------------------------


# total transactions for each state
total_transactions <- table(new_fraud_data$state)

#show transactions
total_transactions

#group and get the mean
state_rates <- aggregate(is_fraud ~ state,data = new_fraud_data, FUN = mean)

# fraud rate percentage for each state
state_rates$fraud_percentage <- state_rates$is_fraud * 100

#round to 2 decimal places
round(state_rates$fraud_percentage, 2)

# Sort by highest fraud rate
state_rates <- state_rates[  order(state_rates$fraud_percentage, decreasing = TRUE),]

# display the table
state_rates


barplot(
  state_rates$fraud_percentage,
  names.arg = state_rates$state,
  main = "Fraud Rate by State",
  xlab = "State",
  ylab = "Fraud Rate (%)",
  col = "orange",
  las = 2,
  cex.names = 0.6,
  ylim = c(0, max(state_rates$fraud_percentage) * 1.15)# add 15% extra space on top
)






  # ------------------------------------------------------------------------------
  # Chart 8: Fraud Rate by State and Population and Transaction Count
  # ------------------------------------------------------------------------------
   
   #group by state and calculate group and rate
   state2 <- aggregate(is_fraud ~ state, data = new_fraud_data, FUN = function(x) c(count=length(x), rate=mean(x)))
   
  
  #create a dataframe and rename columns
    state2 <- do.call(data.frame, state2) #return 2 numbers this time
   colnames(state2) <- c("state", "count", "rate")
   
   #calculate average city population
   avg_pop <- aggregate(city_pop ~ state, data = new_fraud_data, FUN = mean)
   
   #add that avg city population into the state table
   state2$avg_pop <- avg_pop$city_pop[match(state2$state, avg_pop$state)]
   
   #scale population
   state2$log_pop <- log10(state2$avg_pop) #use log10 to compress range to fit
   
   
   #check count
   state2
   
   library(ggplot2) #import library
   
   # draw a bubble chart using ggplot
   ggplot(state2, aes(x = log_pop, y = rate * 100)) +
     geom_point(aes(size = count),colour = "orange",alpha = 0.8) +
     geom_text(aes(label = state), size = 3, vjust = -0.8) +
     scale_size_area(max_size = 12) +
     labs( title = "Fraud Rate vs Population by State and Transactions",
       x = "Average City Population",
       y = "Fraud Rate (%)",
       size = "Transaction Count"
     )
  
   
     
   
# ------------------------------------------------------------------------------
# Hypothesis and Statistical Testing
# ------------------------------------------------------------------------------
 
# ------------------------------------------------------------------------------
# Research Question 1 - transaction amount related to fraud
# ------------------------------------------------------------------------------

#compare transaction amount between groups 0 and 1
     
      tapply(new_fraud_data$amt,new_fraud_data$is_fraud,summary)
   
#wilcoxon test
   wilcox.test(amt ~ is_fraud, data = new_fraud_data)
   
# ------------------------------------------------------------------------------
# Research Question 2 - category related to fraud
# ------------------------------------------------------------------------------
   
#count transactions by category and fraud outcome
   table(new_fraud_data$category,new_fraud_data$is_fraud)
   
#chi squared test
   chisq.test(table(new_fraud_data$category,new_fraud_data$is_fraud))
   
# ------------------------------------------------------------------------------
# Research Question 3 - time of transaction related to fraud
# ------------------------------------------------------------------------------
   
#count transactions by hour and fraud outcome
   table(new_fraud_data$hour, new_fraud_data$is_fraud)
   
#chi square test
   chisq.test(table(new_fraud_data$hour,  new_fraud_data$is_fraud))
   
# ------------------------------------------------------------------------------
# Research Question 4 - gender related to fraud
# ------------------------------------------------------------------------------

#count transactions by gender and fraud outcome
   table(new_fraud_data$gender,new_fraud_data$is_fraud)
   
#chisquare test
   chisq.test(table(new_fraud_data$gender,new_fraud_data$is_fraud))
   
# ------------------------------------------------------------------------------
# Research Question 5a - state related to fraud
# ------------------------------------------------------------------------------
   
# count transactions by state and fraud outcome
   table(new_fraud_data$state, new_fraud_data$is_fraud)
   
   
#chi square test
   chisq.test(table(new_fraud_data$state,new_fraud_data$is_fraud))
   
   
   #getting a "approximation may be incorrect" warning
   #perhaps due to low counts in some states
   
# ------------------------------------------------------------------------------
# Research Question 5b - population related to fraud
# ------------------------------------------------------------------------------

# compare  population for each transaction outcome
   tapply(new_fraud_data$city_pop,new_fraud_data$is_fraud,summary)
   
#wilcoxon test
   wilcox.test(city_pop ~ is_fraud,data = new_fraud_data)
   

   
 
