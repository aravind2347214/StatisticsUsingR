#Setting Directory
getwd()
setwd("C:\\Users\\aravi\\Desktop\\StatisticsUsingR\\SocialMediaStudy")
getwd()

data <- read.csv("IPSMOI.csv")
nrow(data)

#HRSPD - Hours Spend per Day
#SIEFF - Social Interaction Effects Friends and Family
#ACPA - Academic Performance Affect
#MHAF - Mental Health Affected
#QRI - Quality and Relevance of Information
#PC - Privacy Concerns in social Media
#CCF - How Often Content Creation
#SSCC - Support Community and social Cause
#UEP - Use on Education Purpose
#CASI - Creating Awareness On Social Issues
#PDDA - Purchase decisions due to advertising
#SOSP - How Often Social Media Use
#SMPF - Social Media Platform
#RPS - Review and Update Privacy Settings
#TCEM - Type of Content Engagement Most
#OLF - Online Friends
#APOC - Actively Participate in Online Community
#SMIF - Social Media Influencer Following
#PFO - Provide Feedback or opinions
#PG - Personal Growth

#------------------------- Preprocessing -----------------------------
# Remove irrelavant Columns
data <- data[-c(1, 2, 3)]

# Change Column Names to abbreviations
colnames(data) <- c("AgeGroup", "Gender", "CareerStatus", "ResidentialArea",
                    "HRSPD", "SIEFF", "ACPA", "MHAF", "QRI",
                    "PC", "CCF", "SSCC", "UEP", "CASI", "PDDA",
                    "SOSP", "SMPF", "RPS", "TCEM", "OLF", "APOC", "SMIF",
                    "PFO", "PG")
unique(data$PDDA)

data$PDDA<-as.numeric(
  factor(data$PDDA,
         levels = c("Not Influenced at All",
                    "Not Very Influenced",
                    "Neutral",
                    "Infulenced", 
                    "Strongly Influenced")))

data$PDDA
#-------------------------- Problem Statement 1-------------------------------
# Evaluating the Influence of Social Media Advertising on Purchasing Decisions

# 1 Sample t-test:
# Null Hypothesis (H0): The average influence of social media advertising on purchasing decisions
#  is equal to the expected level of influence.
# Alternative Hypothesis (H1): The average influence of social media advertising on purchasing 
# decisions is different from the expected level of influence.
expected_mean_value<-4
# Perform the one-sample t-test
result <- t.test(data$PDDA, mu = expected_mean_value)
# Print the result
print(result)
# The p-value is very small (much less than the conventional significance level of 0.05),
#  suggesting strong evidence against the null hypothesis. Therefore, you may reject the 
#  null hypothesis and conclude that the average influence of social media advertising on
#   purchasing decisions is significantly different from the expected level of 4.
#    The negative t-value indicates that the mean is significantly below 4.
