#Setting Directory
getwd()
setwd("C:\\Users\\aravi\\Desktop\\StatisticsUsingR\\SocialMediaStudy")
getwd()

data <- read.csv("IPSMOI.csv")

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
str(data)
data$PDDA<-as.numeric(
  factor(data$PDDA,
         levels = c("Not Influenced at All",
                    "Not Very Influenced",
                    "Neutral",
                    "Infulenced", 
                    "Strongly Influenced")))

threshold_hours = 3  # Adjust the threshold as needed
# Create a new variable 'UserType' based on the threshold
data$UserType <- ifelse(data$PDDA > threshold_hours, 'Influenced', 'Not Influenced')

data$SMIF<- as.numeric(
  factor(data$SMIF,
         levels = c("Never",
                    "Rarely",
                    "Occasionally", 
                    "Frequently",
                    "All the time")))

data$PG<- as.numeric(
  factor(data$PG,
         levels = c("Never",
                    "Rarely",
                    "Occasionally", 
                    "Frequently",
                    "All the time")))

data$APOC<- as.numeric(
  factor(data$APOC,
         levels = c("Never",
                    "Rarely",
                    "Occasionally", 
                    "Frequently",
                    "All the time")))

#---------------------------End of preproccessing------------------------------

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

#The p-value is very small (much less than the conventional significance level of 0.05),
#suggesting strong evidence against the null hypothesis. Therefore,reject the 
#null hypothesis and concludes that the average influence of social media advertising on
#purchasing decisions is significantly different from the expected level of 4.
#The negative t-value indicates that the mean is significantly below 4.



#2 Sample Independent t-test:
#Null Hypothesis (H0): There is no significant difference in purchasing decisions between individuals
#strongly influenced and not influenced by social media advertising.
#Alternative Hypothesis (H1): There is a significant difference in purchasing decisions between
#individuals strongly influenced and not influenced by social media advertising.

# Subset the data for the two groups: strongly influenced and not influenced
influenced_group <- data[data$UserType == "Influenced", "PDDA"]
not_influenced_group <- data[data$UserType == "Not Influenced", "PDDA"]

# Perform the t-test
two_test_result <- t.test(influenced_group, not_influenced_group)

# Display the results
print(two_test_result)

#The extremely low p-value (< 0.05) suggests strong evidence against the null hypothesis. 
#Therefore,there is evidence to reject the null hypothesis and conclude that there
#is a significant difference in purchasing decisions between individuals strongly influenced
#and not influenced by social media advertising.

#One-way ANOVA:
#Null Hypothesis (H0): There is no significant difference in purchasing decisions across multiple
#groups based on different levels of influencer following.
#Alternative Hypothesis (H1): There is a significant difference in purchasing decisions
#across multiple groups based on different levels of influencer following.


# Perform one-way ANOVA using SMIF
anova_result <- aov(PDDA ~ SMIF, data = data)

# Display the ANOVA table
summary(anova_result)

#The small p-value (< 0.05) suggests that there is a significant difference in purchasing 
#decisions across multiple groups based on different levels of influencer following. 
#Therefore,reject the null hypothesis and conclude that there is a statistically
#significant difference.

#Two-way ANOVA:
#H01:Social media advertising does not significantly influence purchasing decisions.
#H11:Social media advertising has a significant impact on purchasing decisions.
#H02:There is no significant difference in purchasing decisions between users of different social media platforms.
#H12:There is a significant difference in purchasing decisions between users of different social media platforms.

# Perform a two-way ANOVA
two_way_anova_result <- aov(PDDA ~ SMPF * SOSP, data = data)

# Print the results
summary(two_way_anova_result)

#SMPF (Social Media Platform): The p-value is 0.165, which is greater than 0.05.
#This suggests that there is no significant difference in personal growth scores based
#on different social media platforms.

#SOSP (How Often Social Media Use): The p-value is 0.257, which is greater than 0.05.
#This suggests that there is no significant difference in personal growth scores based on
#different levels of social media use frequency.

#SMPF:SOSP (Interaction term): The p-value is 0.701, which is greater than 0.05. 
#This suggests that there is no significant interaction effect between social media
#platform and social media use frequency regarding personal growth scores.

#In summary, based on the provided p-values, there is no evidence of a significant effect 
#of social media platform, social media use frequency, or their interaction on personal growth scores.



#-------------------------- Problem Statement 2-------------------------------
# Analyzing the Relationship between Social Media Interaction and Personal Growth 

# 1 Sample t-test:
# Null Hypothesis (H0): The average contribution of social media interaction to personal growth is 
# equal to the expected level of contribution.
# Alternative Hypothesis (H1): The average contribution of social media interaction to personal growth 
# is different from the expected level of contribution.

expected_level <- 3.5


# Perform one-sample t-test
t_test_result <- t.test(data$PG, mu = expected_level)

# Display the results
print(t_test_result)

#The p-value is highly significant (p-value < 2.2e-16), suggesting that 
#the observed mean (2.086) is significantly different from the expected mean of 3.5. 
#The confidence interval (1.944 to 2.228) further supports this, as it does not include
#the expected mean. This implies that, on average, personal growth from social media 
#interaction is lower than the expected level.

#2 Sample Independent t-test:
#Null Hypothesis (H0): There is no significant difference in personal growth scores
#between individuals who actively participate in online communities and those who do not.
#Alternative Hypothesis (H1): There is a significant difference in personal growth
#scores between individuals who actively participate in online communities and those who do not.

group_active <- data[data$APOC %in% c(5,4), "PG"]
group_inactive <- data[data$APOC %in% c(1,2), "PG"]

# Perform a two-sample independent t-test
t_test_result <- t.test(group_active, group_inactive)

# Print the results
cat("Two-Sample Independent t-Test Results:\n")
print(t_test_result)

# The p-value of 0.001453 is less than the commonly used significance level of 0.05, indicating strong 
# evidence against the null hypothesis. In this case,reject the null hypothesis that the true 
# difference in means is equal to 0.the results suggest that there is a significant difference in means 
#between group_active and group_inactive.



# One-way ANOVA:
# Null Hypothesis (H0): There is no significant difference in personal growth across multiple groups based
#  on different levels of engagement in online communities.
# Alternative Hypothesis (H1): There is a significant difference in personal growth across multiple groups 
# based on different levels of engagement in online communities.

anova_result <- aov(PG ~ APOC, data = data)

# Display the ANOVA table
summary(anova_result)

# The small p-value (< 0.05) suggests that there is a significant difference in personal growth across different levels 
# of engagement in online communities. Therefore,reject the null hypothesis and conclude that there is a 
# statistically significant difference.


# Two-way ANOVA:
# H01:Social media interaction does not significantly contribute to personal growth and development.
# H11:Social media interaction has a significant positive impact on personal growth and development.
# H02:There is no significant difference in personal growth scores between individuals of different genders.
# H12:There is a significant difference in personal growth scores between individuals of different genders.


two_way_anova_result <- aov(PG ~ SMIF * Gender, data = data)

# Print the results
summary(two_way_anova_result)

#SMIF (Social Media Influencer Following): The p-value is extremely small (< 0.001),
#indicating that there is a significant difference in personal growth scores based on 
#different levels of social media influencer following.

#Gender: The p-value is 0.0255, which is less than 0.05. This suggests a significant
#difference in personal growth scores between individuals of different genders.

#SMIF:Gender (Interaction term): The p-value is 0.7651, which is greater than 0.05.
#This suggests that there is no significant interaction effect between social media influencer
#following and gender regarding personal growth scores.

#In summary, both social media influencer following and gender individually have a
#significant effect on personal growth scores, but there is no significant interaction
#effect between them.



data
data$Age=="Under 18"
count.fields(data$Age=="Under 18")
table(data$Age)

