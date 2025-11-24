#################
###HOMEWORK 12###
#################

#Packages: readxl, tidyverse, faraway
#Authors: Aubrey Wendorff

# Objective 1 ------------------------------------------------------------
###PART A###---
#Read in the packages
library(readxl)
library(tidyverse)
library(faraway)

#Read in the Data
df <- read_excel("Data/BSB_tagging_data.xlsx")

#Make new column for True/False Sex Change
df <- df %>%
  filter(Sex_at_capture == "F") %>% #Filter just females
  filter(month(Date_at_recapture) %in% 8:12) #Filter for recapture after July
df$SexChange <- df$Sex_at_capture != df$Sex_at_recapture #Make sex change column
                                                            #False = sex at recapture is different
                                                            #True = sex at recapture is same
#Calculate sum for SexChange and no SexChange
sum_sexchange <- sum(df$SexChange == "TRUE")
sum_nochange <- sum(df$SexChange == "FALSE")

#Create Beta distribution parameters
alpha <- sum_sexchange + 1
beta <- sum_nochange + 1

#Generate 1000 x-values between 0 and 1
x_values <- seq(0, 1, length.out = 1000)

#Calculate beta distribution
beta_dist <- dbeta(x_values, alpha, beta)

#Make a line plot
plot(x_values, beta_dist, type = "l")


###PART B###---
#Calculate 95% confidence interval
CI <- qbeta(c(0.025, 0.975), alpha, beta)
print(CI)
    #CI = (0.1728742, 0.4939590)

#Plot CI Plot with dashed lines in red (for fun)
plot(x_values, beta_dist, type = "l")
abline(v = CI, col = "red", lty = 2)



# Objective 2 -------------------------------------------------------------------------
###PART A###---
#Create glm model (reponse ~ predictor)
model <- glm(SexChange ~ Length_at_capture, family = binomial(link = "logit"), data = df)
summary(model) #output of model
  #p_value: 0.1122
  #the length at capture does not influence the probability of a sex change at recapture


###PART B###---
#0.04490 change in log-odds for every mm in length increase


###PART C###---
#get min and max x_values
summary(df$Length_at_capture)

#Create plot
plot(df$SexChange ~ df$Length_at_capture, #response ~ predictor
     xlim = c(273, 340), #min and max x-values
     ylim = c(0,1), #min and max y-values
     xlab = "Length at Capture (mm)", ylab = "Probability of Sex Change",
     pch = 16) #solid points

#De-transform the logit to get the model estimated relationship on the data
Length_at_capture = seq(273, 340, 1); x_frame = as.data.frame(Length_at_capture)
lines(Length_at_capture, predict.glm(model, newdata = x_frame, type = "response"), col = "red", lwd = 2)
  #Figure 1: The relationship between the probability of sex change and length (mm) for female Black sea bass. 
  #Points represent individual  female fish recaptured after the spawning season (July). 
  #Sex change was recorded upon recapture as either true or false. 
  #Length at capture did not have a significant effect on the probability of a sex change at recapture (p-value: 0.1122).

