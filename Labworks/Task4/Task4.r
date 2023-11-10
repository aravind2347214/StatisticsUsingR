print(getwd())
setwd("C:\\Users\\aravi\\Desktop\\StatisticsUsingR\\Labworks\\Task4")
print(getwd())

employee_data

# Read the CSV file into a data frame
employee_data <- read.csv("employee_data.csv")

# Print out the first few rows of employee_data
head(employee_data)


# print tail of employee data
tail(employee_data)

# View summary statistics for all variables in employee_data
summary(employee_data)

# Display the structure of the data frame
print(employee_data)

# Calculate and add a new column for years of service
employee_data$Years_of_Service <- 2023 - as.numeric(substring(employee_data$Joining_Date,first = 1, last = 4))
print(employee_data)

# Create a new data frame for senior employees
senior_employees <- employee_data[employee_data$Years_of_Service >= 5, ]
print(senior_employees)

# Print average salary by department
print(tapply(employee_data$Salary, employee_data$Department, mean))

# Print highest and lowest salaries with employee details
cat("Highest Salary:\n")
print(employee_data[employee_data$Salary == max(employee_data$Salary), c("Employee_ID", "Name", "Salary")])



cat("\nLowest Salary:\n")
print(employee_data[employee_data$Salary == min(employee_data$Salary), c("Employee_ID", "Name", "Salary")])

# Bar plot for the number of employees in each department
barplot(table(employee_data$Department), main="Number of Employees by Department", xlab="Department", ylab="Number of Employees")

# Scatter plot for years of service vs. salary
plot(employee_data$Years_of_Service, employee_data$Salary, main="Scatter Plot: Years of Service vs. Salary", xlab="Years of Service", ylab="Salary")

# Save the "Senior Employees" data frame as a CSV file
write.csv(senior_employees, "senior_employees.csv", row.names = FALSE)
