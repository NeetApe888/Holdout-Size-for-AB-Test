#######################Start here: User input######
#set working directory
setwd('/Users/Neet/Downloads')

# File Names
#inputs:
datamine_output= "datamine_output.txt"
schedule_name = 'schedule.csv'
#output
CSV_output_name='WSI Output Last 28 days v3.csv' #create name here


#load files
data <-read.table(datamine_output, header=T, sep="\t", stringsAsFactors = FALSE)
head(data)
nrow(data)
schedule = read.csv(schedule_name,stringsAsFactors = FALSE)
head(schedule)


#######################End user input##############


#load libraries
library(tidyverse)
library(TrialSize)

#run ab estimate function
ABEstimate <- function(CR_exposed, testRatio,holdoutRatio){
  
  allocation_ratio = testRatio / holdoutRatio
  CR_test <- CR_exposed/seq(1.05,1.20,by=.025)    # effect range, these align with the inc_ fields
  margin <- CR_exposed - CR_test
  alpha <- seq(0.05,0.2, by= 0.05)            # stat sig range
  
  #define the data frame, all values will be updated in future steps
  df <- data.frame(
    confidence =c(0.95,0.90,0.85,0.80,0.75) #aligns with alpha
    ,inc_.050=c(0,0,0,0,0)
    ,inc_.075=c(0,0,0,0,0)
    ,inc_.100=c(0,0,0,0,0)
    ,inc_.125=c(0,0,0,0,0)
    ,inc_.150=c(0,0,0,0,0)
    ,inc_.175=c(0,0,0,0,0)
    ,inc_.200=c(0,0,0,0,0)
  )
  
  for(i in 1:nrow(df)){
    n2 <- TwoSampleProportion.Equality((1-df$confidence[i]), beta=.2, CR_exposed, CR_test, k=allocation_ratio) 
    df[i,2:8]<- ceiling(n2)
  }
  
  return(df)
  
}


#run ab estimate on data function
conversionRate = function(data,advertiserIndex,testRatio,holdoutRatio) {
  allocationRatio = testRatio / holdoutRatio
  split = paste0((100-(100/(allocationRatio+1))),' to ',(100/(allocationRatio+1)),' split')
  
  
  advertisers = data
  data = advertisers %>% filter(advertiser_id == advertisers[advertiserIndex,1])
  
  results = ABEstimate(advertisers[advertiserIndex,11],testRatio,holdoutRatio) #11 is conversion rate
  results = as.data.frame(results)
  
  results$advertiser_id = advertisers[advertiserIndex,1]
  results$advertiser_name = advertisers[advertiserIndex,2]
  results$insertion_order_id = advertisers[advertiserIndex,3]
  results$insertion_order_name = advertisers[advertiserIndex,4]
  results$time_period = advertisers[advertiserIndex,5]
  results$past_impressions = advertisers[advertiserIndex,6]
  results$past_spend = advertisers[advertiserIndex,7]
  results$past_xd_actions = advertisers[advertiserIndex,8]
  results$past_xd_rev = advertisers[advertiserIndex,9]
  results$past_uu = advertisers[advertiserIndex,10]
  results$past_conversion_rate = advertisers[advertiserIndex,11]
  results$holdout_ratio = split
  return(results)
  
}


repeatABTest = function(data,schedule) {
  df <- data.frame()
  
  for (i in 1:nrow(data)) {
    
    for (j in 1:nrow(schedule)) {
      
      test = schedule[j,1]
      holdout = schedule[j,2]
      result = conversionRate(data,i,test,holdout)
      df = rbind(df,result)
      
    }
    DF2<- pivot_longer(df,cols = starts_with("inc_"), 
                       names_to = "Incrementality Minimum",
                       values_to = "Uniques", 
                       values_transform = list(val = as.character))
    print(DF2)
    write.csv(DF2,CSV_output_name,row.names = TRUE)
    
  }
}


repeatABTest(data,schedule)