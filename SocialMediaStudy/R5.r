getwd()
setwd("C:\\Users\\aravi\\Desktop\\StatisticsUsingR\\SocialMediaStudy")
getwd()
data <- read.csv("IPSMOI.csv")
data <- data[-c(1, 2, 3)]
colnames(data) <- c("AgeGroup", "Gender", "CareerStatus", "ResidentialArea")

# Bar Plot For Age
barplot(
  table(data$AgeGroup),
  main = "Count of Each Age Group",
  ylim = c(0, 120),
  xlab = "Age Group",
  ylab = "Number Of Respondents",
  col = rainbow(length(unique(data$AgeGroup)))
)

# Bar Plot For Gender
barplot(
  table(data$Gender),
  main = "Count of Each Gender",
  ylim = c(0, 140),
  xlab = "Genders",
  ylab = "Number Of Respondents",
  col = rainbow(length(unique(data$Gender)))
)

# Bar Plot For Career Status
barplot(
  table(data$CareerStatus),
  main = "Count of Each Career Status",
  ylim = c(0, 100),
  xlab = "Career Status",
  ylab = "Number Of Respondents",
  col = rainbow(length(unique(data$CareerStatus)))
)

# Bar Plot For Residential area
barplot(
  table(data$ResidentialArea),
  main = "Count of Each Residential Area",
  ylim = c(0, 200),
  xlab = "Residential Area",
  ylab = "Number Of Respondents",
  col = rainbow(length(unique(data$ResidentialArea)))
)


# Histogram for Age
age_encoded_data <- as.numeric(factor(data$AgeGroup))
age_frequency_table <- table(data$AgeGroup)
print(age_encoded_data)
hist(age_encoded_data,
  main = "Distribution of Encoded Age Groups",
  xlab = "Encoded Age Group",
  ylab = "Count",
  ylim = c(0,130),
  #xaxt = "n",
  border = "black",
  breaks = max(age_encoded_data) - min(age_encoded_data)+1,
  col = rainbow(length(age_frequency_table)), # You can choose any color palette you prefer
  
) # Use the range of encoded values as breaks

# If you have specific labels for each encoded value, you can add them to the x-axis
#axis(side = 1, at = 1:max(age_encoded_data), labels = c("Under 18", "18-24", "25-34", "35-44", "45 and above"))
legend("topright", legend = names(age_frequency_table), fill = rainbow(length(age_frequency_table)))


# Histogram for Gender
gender_encoded_data <- as.numeric(factor(data$Gender))
gender_frequency_table <- table(data$Gender)

print(gender_encoded_data)
hist(gender_encoded_data,
  main = "Distribution of Encoded Gender Groups",
  xlab = "Encoded Gender Group",
  ylab = "Count",
  xaxt = "n",
  col = rainbow(length(gender_frequency_table)), # You can choose any color palette you prefer
  border = "black",
  names=names(gender_frequency_table),
  breaks = max(gender_encoded_data) - min(gender_encoded_data) + 1
) # Use the range of encoded values as breaks

# If you have specific labels
#  for each encoded value, you can add them to the x-axis
legend("topright", legend = names(gender_frequency_table), fill = rainbow(length(gender_frequency_table)))


# Histogram for Career Status
CS_encoded_data <- as.numeric(factor(data$CareerStatus)) # nolint
factor(data$CareerStatus)
print(CS_encoded_data)
hist(CS_encoded_data,
  main = "Distribution of Encoded Career Status Groups",
  xlab = "Encoded Career Status Group",
  ylab = "Count",
  xaxt = "n",
  col = "skyblue", # You can choose any color you prefer
  border = "black",
  breaks = max(CS_encoded_data) - min(CS_encoded_data) + 1
) # Use the range of encoded values as breaks

# If you have specific labels for 
# each encoded value, you can add them to the x-axis
axis(side = 1, at = 1:max(CS_encoded_data), labels = c("Employed", "Postgraduate", "Retired", "School", "Undergraduate", "Unemployed")) # nolint


res_encoded_data <- as.numeric(factor(data$ResidentialArea))
factor(data$ResidentialArea)
print(res_encoded_data)
hist(res_encoded_data,
  main = "Distribution of Encoded Residential Area",
  xlab = "Encoded Residential Area Group",
  ylab = "Count",
  xaxt = "n",
  col = "skyblue", # You can choose any color you prefer
  border = "black",
  breaks = max(res_encoded_data) - min(res_encoded_data) + 1
) # Use the range of encoded values as breaks
# If you have specific labels for each encoded value, you can add them to the x-axis
axis(side = 1, at = 1:max(res_encoded_data), labels = c("Rural", "Suburban", "Urban"))


# Calculate the frequency of each category
age_frequency_table <- table(data$AgeGroup)
age_frequency_table
# Create a pie chart
age_percentages <- round((age_frequency_table / sum(age_frequency_table)) * 100, 2)
pie(age_frequency_table,
  main = "Distribution of Age Groups",
  col = rainbow(length(age_frequency_table)), # You can choose any color palette you prefer
  # labels = c("18-24", "25-34", "35-44", "45 and above","Under 18"))
  labels = paste0(age_percentages, "%")
)
# Add a legend
legend("topright", legend = names(age_frequency_table), fill = rainbow(length(age_frequency_table)))



gender_frequency_table <- table(data$Gender)
gender_frequency_table
# Create a pie chart
gender_percentages <- round((gender_frequency_table / sum(gender_frequency_table)) * 100, 2)
pie(gender_frequency_table,
  main = "Distribution of Genders",
  col = rainbow(length(gender_frequency_table)), # You can choose any color palette you prefer
  # labels = c("18-24", "25-34", "35-44", "45 and above","Under 18"))
  labels = paste0(gender_percentages, "%")
)
# Add a legend
legend("topright", legend = names(gender_frequency_table), fill = rainbow(length(gender_frequency_table)))



CS_frequency_table <- table(data$CareerStatus)
CS_frequency_table
# Create a pie chart
CS_percentages <- round((CS_frequency_table / sum(CS_frequency_table)) * 100, 2)
pie(CS_frequency_table,
  main = "Distribution of Career Status",
  col = rainbow(length(CS_frequency_table)), # You can choose any color palette you prefer
  # labels = c("18-24", "25-34", "35-44", "45 and above","Under 18"))
  labels = paste0(CS_percentages, "%")
)
# Add a legend
legend("topright", legend = names(CS_frequency_table), fill = rainbow(length(CS_frequency_table)))


res_frequency_table <- table(data$ResidentialArea)
res_frequency_table
# Create a pie chart
res_percentages <- round((res_frequency_table / sum(res_frequency_table)) * 100, 2)
pie(res_frequency_table,
  main = "Distribution of Residential Area",
  col = rainbow(length(res_frequency_table)), # You can choose any color palette you prefer
  # labels = c("18-24", "25-34", "35-44", "45 and above","Under 18"))
  labels = paste0(res_percentages, "%")
)
# Add a legend
legend("topright", legend = names(res_frequency_table), fill = rainbow(length(res_frequency_table)))


