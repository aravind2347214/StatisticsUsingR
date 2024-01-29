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


data$ACPA<-as.numeric(
  factor(data$ACPA,
         levels = c("Extremely declined",
                    "Declined",
                    "Neutral",
                    "Improved",
                    "Extremely improved")))
data$SOSP<-as.numeric(
  factor(data$SOSP,
         levels = c("Rarely",
                    "A few times a month",
                    "A few times a week",
                    "Once a day",
                    "Multiple times a day")))

data$QRI<-as.numeric(
  factor(data$QRI,
         levels = c("Harmful Information",
                    "Not Very Informative",
                    "Neutral",
                    "Moderately Informative",
                    "Highly Informative")))

data$PC<-as.numeric(
  factor(data$PC,
         levels = c("Not Concerned at All",
                    "Not Very Concerned",
                    "Neutral",
                    "Somewhat Concerned",
                    "Very Concerned")))

# -----------------------End of Preprocessing--------------------------

# -----------------------Problem Statement 1---------------------------
#Investigating the Impact of Social Media Usage on Academic Performance

#1 Sample t-test:
# Hypothesis (H0): The average academic performance of students with varying 
#levels of social media usage is equal to the expected performance.
#Alternative Hypothesis (H1): The average academic performance of students
#with varying levels of social media usage is different from the expected performance.

# Assuming 'ACPA' is the column representing academic performance
# and 'expected_mean' is the expected mean academic performance.

# Calculate the mean of the academic performance
sample_mean <- mean(data$ACPA)

# Perform a one-sample t-test
expected_mean<-4
t_test_result <- t.test(data$ACPA, mu = expected_mean)

# Print the results
cat("Sample Mean:", sample_mean, "\n")
cat("Expected Mean:", expected_mean, "\n")
cat("T-Test Results:\n")
print(t_test_result)

#The low p-value and the rejection of the null hypothesis suggest that there is a
#significant difference between the observed academic performance and the expected 
#mean of 4. The 95 percent confidence interval gives a range for the possible true mean.

#2 Sample Independent t-test:

# Null Hypothesis (H0): There is no significant difference in academic performance between students who use social media extensively and those who use it minimally.
# Alternative Hypothesis (H1): There is a significant difference in academic performance between students who use social media extensively and those who use it minimally.

# Assuming 'ACPA' is the column representing academic performance
# and 'SOSP' is the column representing social media usage frequency.

# Create two separate groups based on social media usage
extensive_condition <- data$SOSP %in% c(5, 4, 3)
# Create a logical vector for minimal social media usage
minimal_condition <- data$SOSP %in% c(2, 1)
# Filter data for extensive social media usage
group_extensive <- data[extensive_condition, "ACPA"]
# Filter data for minimal social media usage
group_minimal <- data[minimal_condition, "ACPA"]
t_test_result <- t.test(group_extensive, group_minimal)
# Print the results
cat("Two-Sample Independent t-Test Results:\n")
print(t_test_result)

# Based on the results, we do not have enough evidence to reject the null hypothesis. 
# The p-value is high, and the confidence interval includes zero, indicating that there 
# is no significant difference in academic performance between students who use social 
# media extensively and those who use it minimally.




#One-way ANOVA:
# Null Hypothesis (H0): There is no significant difference in academic performance across multiple groups based on different levels of social media usage.
# Alternative Hypothesis (H1): There is a significant difference in academic performance across multiple groups based on different levels of social media usage.

# Assuming 'ACPA' is the column representing academic performance
# and 'SOSP' is the column representing social media usage frequency.

# Perform a one-way ANOVA
anova_result <- aov(ACPA ~ SOSP, data = data)
# Print the results
cat("One-way ANOVA Results:\n")
summary(anova_result)

# The one-way ANOVA results indicate that the p-value associated with the F-statistic 
# for the variable "SOSP" (social media usage frequency) is 0.134, which is greater
#  than the commonly used significance level of 0.05. Therefore, you do not have
#   sufficient evidence to reject the null hypothesis.

# In simpler terms, based on the data , there is no significant 
# difference in academic performance across different levels of social 
# media usage. Keep in mind that the interpretation might change with
#  additional data or different analyses.





#Two-way ANOVA:
# H01: There is no significant difference in academic performance between students with varying levels of social media usage.
# H11: There is a significant difference in academic performance between students with varying levels of social media usage.
# H02: There is no significant difference in academic performance between male and female students.
# H12: There is a significant difference in academic performance between male and female students.

# Assuming 'ACPA' is the column representing academic performance
# and 'SOSP' and 'Gender' are the columns representing social media usage frequency and gender.

# Perform a two-way ANOVA
two_way_anova_result <- aov(ACPA ~ SOSP * Gender, data = data)

# Print the results
cat("Two-way ANOVA Results:\n")
summary(two_way_anova_result)
# The p-values for both 'SOSP' and 'Gender' factors, as well as their interaction,
#  are greater than the significance level of 0.05. Therefore, there is no significant
# difference in academic performance based on social media usage frequency, gender,
# or their interaction.
#  p-values above 0.05 suggest that we do not have enough evidence
#  to reject the null hypothesis for these factors.
 

#----------------------------- Problem Statement 2:-------------------------------

# Exploring the Association between Social Media Interaction and Real-life Social Relationships:

#1 Sample t-test:
# Null Hypothesis (H0): The average perception of information quality among individuals 
# with different privacy concerns is equal to the expected perception.
# Alternative Hypothesis (H1): The average perception of information quality
#  among individuals with different privacy concerns is different from the expected perception.

# Assuming 'QRI' is the column representing information quality ratings
# and 'expected_mean' is the expected mean perception.
# Calculate the mean of information quality ratings
sample_mean <- mean(data$QRI)
expected_mean<-4
# Perform a one-sample t-test
t_test_result <- t.test(data$QRI, mu = expected_mean)

# Print the results
cat("Sample Mean:", sample_mean, "\n")
cat("Expected Mean:", expected_mean, "\n")
cat("T-Test Results:\n")
print(t_test_result)
# The negative t-value suggests that the average perception of information quality 
# is significantly lower than the expected mean. The 95% confidence interval (3.66, 3.87) 
# indicates the range within which the true population mean is likely to fall.
# the expected value of mean was taken in account as assuming the average was high or moderately informative 




#2 Sample Independent t-test:
# Null Hypothesis (H0): There is no significant difference in information quality ratings between
#  individuals with high and low privacy concerns.
# Alternative Hypothesis (H1): There is a significant difference in information quality ratings 
# between individuals with high and low privacy concerns.

# Assuming 'QRI' is the column representing information quality ratings
# and 'PrivacyConcerns' is the column representing privacy concerns.

# Create two separate groups based on privacy concerns
group_high <- data[data$PC %in% c(5,4), "QRI"]
group_low <- data[data$PC %in% c(1,2), "QRI"]

# Perform a two-sample independent t-test
t_test_result <- t.test(group_high, group_low)

# Print the results
cat("Two-Sample Independent t-Test Results:\n")
print(t_test_result)
# The t-value close to zero indicates that there is no significant difference in 
# information quality ratings between individuals with high and low privacy concerns. 
# The high p-value (0.8647) supports this observation, suggesting that the difference 
# in means is not statistically significant. Thus, we fail to reject the null hypothesis, 
# indicating no significant difference in information quality ratings based on privacy concerns.




#One-way ANOVA:
# Null Hypothesis (H0): There is no significant difference in information quality across multiple groups based on different levels of privacy concerns.
# Alternative Hypothesis (H1): There is a significant difference in information quality across multiple groups based on different levels of privacy concerns.

# Assuming 'QRI' is the column representing information quality ratings
# and 'PrivacyConcerns' is the column representing privacy concerns.

# Perform a one-way ANOVA
anova_result <- aov(QRI ~ PC, data = data)

# Print the results
cat("One-way ANOVA Results:\n")
summary(anova_result)

# the p-value associated with the F-statistic is 0.423, which is greater than the 
# typical significance level of 0.05. Therefore, we fail to reject the null hypothesis, 
# indicating that there is no statistically significant difference in information quality
#  ratings between different levels of privacy concerns.

#Two-way ANOVA:
# H01: Privacy concerns do not significantly affect the perceived quality and relevance of information on social media.
# H11: Privacy concerns have a significant impact on the perceived quality and relevance of information on social media.
# H02: There is no significant difference in information quality ratings between different age groups.
# H12: There is a significant difference in information quality ratings between different age groups.

# Assuming 'QRI' is the column representing information quality ratings
# and 'PrivacyConcerns' and 'AgeGroup' are the columns representing privacy concerns and age groups.

# Perform a two-way ANOVA
two_way_anova_result <- aov(QRI ~ PC * AgeGroup, data = data)

# Print the results
cat("Two-way ANOVA Results:\n")
summary(two_way_anova_result)
# These results indicate that neither the main effects of 'PC' and 'AgeGroup' nor their
#  interaction have a statistically significant impact on information quality ratings.
#   The p-values for both main effects and the interaction are greater than the 
#   conventional significance level of 0.05, suggesting that we do not have enough evidence 
#   to reject the null hypotheses.
