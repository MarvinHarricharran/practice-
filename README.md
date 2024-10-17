getwd()

load("C:/Users/marvi/Downloads/Data_October7th/acs2021_recoded.RData")

View(acs2021)


library(ggplot2)
library(tidyverse)
library(haven)

summary(acs2021)

PT_Test


library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
require(class)
require(caret)
require(haven)



#This will filter out all the N/A
acs_coll <- acs2021 %>% filter(DEGFIELD != "N/A")

#Code the table,  
data_bach <- matrix(data = c(6838, 7218, 15973,
                             2640, 2230, 20336),
                    nrow = 2,
                    ncol = 3,
                    byrow = TRUE)
BA_Remote <- as.data.frame(data_bach)

#Labeling the table
row.names(BA_Remote) <- c(
  "Higher_Than_Bach",
  "No_Degree")

colnames(BA_Remote) <- c(
  "Remote",
  "Some_Remote",
  "No_Degree")

# Finding T & P values
PT_Test <- t.test(BA_Remote$Remote, BA_Remote$No_Degree)
print(PT_Test)

PT_Test$stderr


#question 2 compare the data of region vs ancestory 

xtabs( ~ born_in_USstate[(REGION == "South Atlantic Division")] + FOODSTMP[(REGION == "South Atlantic Division")], data = acs2021)

xtabs( ~ born_in_USstate[(REGION == "Pacific Division")] + FOODSTMP[(REGION == "Pacific Division")], data = acs2021)

xtabs( ~ born_in_USstate[(REGION == "East North Central Div.")] + FOODSTMP[(REGION == "East North Central Div.")], data = acs2021)

xtabs( ~ born_in_USstate[(REGION == "Middle Atlantic Division")] + FOODSTMP[(REGION == "Middle Atlantic Division")], data = acs2021)

xtabs( ~ born_in_USstate[(REGION == "West South Central Div.")] + FOODSTMP[(REGION == "West South Central Div.")], data = acs2021)

xtabs( ~ born_in_USstate[(REGION == "Mountain Division")] + FOODSTMP[(REGION == "Mountain Division")], data = acs2021)


#Now create the a data frame i will choose from  

WHO_GETS_FOODSTMP <- data.frame(
  REGION = c("South Atlantic Division", "South Atlantic Division", "Pacific Division", "Pacific Division", "East North Central Div.", "East North Central Div.", "Middle Atlantic Division", "Middle Atlantic Division", "West South Central Div.", "West South Central Div.", "Mountain Division", "Mountain Division"),
  born_in_USstate = c("not born in US state", "born in a state in the US", "not born in US state", "born in a state in the US", "not born in US state", "born in a state in the US", "not born in US state", "born in a state in the US", "not born in US state", "born in a state in the US", "not born in US state", "born in a state in the US"),
  FOODSTMP1 = c(78729, 473648, 111256, 344221, 28883, 385482, 60672, 309904, 42728, 274890, 23325, 196140),
  FOODSTMP2 = c(14608, 72879, 19338, 53896, 4976, 54594, 12425, 40007, 6787, 48534, 3516, 25931)
)

#now create a table 


table1 <- xtabs(cbind(FOODSTMP1, FOODSTMP2) ~ REGION + born_in_USstate, data = WHO_GETS_FOODSTMP)
table1

library(dplyr)
library(tidyr)

#Make another data frame
WHO_GETS_FOODSTMP1 <- data.frame(
  REGION = rep(c("South Atlantic Division", "East North Central Div.", "Middle Atlantic Division", "Mountain Division", "Pacific Division", "West South Central Div."), each = 2),
  born_in_USstate = rep(c("not born in US state", "born in a state in the US"), times = 6),
  counts = c(78729, 473648, 111256, 344221, 28883, 385482, 60672, 309904, 42728, 274890, 23325, 196140, 14608, 72879, 19338, 53896, 4976, 54594, 12425, 40007, 6787, 48534, 3516, 25931)
)



# Reshape the data into long format using dplyr
WHO_GETS_FOODSTMP_beta <- WHO_GETS_FOODSTMP1 %>%
  pivot_longer(-c(REGION, born_in_USstate), names_to = "US_Born", values_to = "counts")

# Fit linear regression using lm ()
model <- lm(counts ~ REGION * born_in_USstate, data = WHO_GETS_FOODSTMP_beta)
summary(model)
t_testresult <- summary(model)
t_testresult



#Now this is for question 4 

library(forcats)
library(haven)
use_varb <- 
  (acs2021$AGE >= 25) & 
  (acs2021$AGE <= 60) & 
  (as.numeric(as_factor(acs2021$LABFORCE)) == 2) & 
  (acs2021$WKSWORK2 > 4) & 
  (acs2021$UHRSWORK >= 35) & 
  (acs2021$DEGFIELD == 'Social Sciences') & 
  (as.numeric(as_factor(acs2021$educ_advdeg)) == 1) & 
  (as.numeric(as_factor(acs2021$female)) == 1)

data_use <- subset(acs2021,use_varb) 


model_temp1 <- lm(INCWAGE ~ AGE + unmarried + AfAm + Asian + Amindian + race_oth + white + Hispanic, data = data_use)
summary(model_temp1)

require(stargazer)
stargazer(model_temp1, type = "text")

#The coefficient suggests that being unmarried is associated with a decrease in INCWAGE specifically -10253.7 lower. Since the standard error shows there may be some variability around the result, I wondered if it is because unmarried women may have a smaller family size therefore, they would not need to make more money



model_temp2 <- lm(INCWAGE ~ AGE + unmarried + AfAm + Asian + Amindian + race_oth + white + Hispanic + FAMSIZE, data = data_use)
summary(model_temp2)

require(stargazer)
stargazer(model_temp2, type = "text")


#According to the result, it appears that income is likely to increase by 8086.7 for every increase in family size could be a factor to why unmarried women are reporting lower income. The next step would be to test the family size difference between women who are married and those who are unmarried.


use_varb1 <- (acs2021$AGE >= 25) & (acs2021$AGE <= 60) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 > 4) & (acs2021$UHRSWORK >= 35) & (acs2021$DEGFIELD == 'Social Sciences') & (acs2021$educ_advdeg == 1) & (acs2021$female == 1) & (acs2021$FAMSIZE >= 1) & (acs2021$unmarried == 1)

use_varb2 <- (acs2021$AGE >= 25) & (acs2021$AGE <= 60) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 > 4) & (acs2021$UHRSWORK >= 35) & (acs2021$DEGFIELD == 'Social Sciences') & (acs2021$educ_advdeg == 1) & (acs2021$female == 1) & (acs2021$unmarried == 1)

use_varb3 <- (acs2021$AGE >= 25) & (acs2021$AGE <= 60) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 > 4) & (acs2021$UHRSWORK >= 35) & (acs2021$DEGFIELD == 'Social Sciences') & (acs2021$educ_advdeg == 1) & (acs2021$female == 1) & (acs2021$unmarried == 0) & (acs2021$FAMSIZE >= 1)


table(use_varb1)
table(use_varb2)
table(use_varb3)



#To test my hypothesis, I counted the amount of unmarried women in social science with an advance degree between 25:60 in those working conditions vs the unmarried women in social science with an advance degree between 25:60 with a family size less than 1 vs a family size greater than 1 of married women with the same conditions. 279 more married women reported their family size to be larger than 1 while the two groups with unmarried women had no numerical differences which could be because unmarried women have smaller family size which is seen in model_temp3 where it indicates that as the size of a family increases, the likelihood of being unmarried decreases.


use_varb4 <- (acs2021$AGE >= 25) & (acs2021$AGE <= 60) & (acs2021$LABFORCE == 2) & (acs2021$WKSWORK2 > 4) & (acs2021$UHRSWORK >= 35) & (acs2021$DEGFIELD == 'Social Sciences') & (acs2021$educ_advdeg == 1) & (acs2021$female == 1)
data_use2 <- subset(acs2021,use_varb4) 


model_temp3 <- lm(unmarried ~ INCWAGE + AGE + FAMSIZE + AfAm + Asian + Amindian + race_oth + white + Hispanic, data = data_use2)
summary(model_temp3)

require(stargazer)
stargazer(model_temp3, type = "text")





