#Create employee List
employee1<-list(EmployeeID=101,Name="Sam George",Salary=10000,Departments=c("Sales","Marketing","Development"))
employee2<-list(EmployeeID=102,Name="Tina Mathew",Salary=12000,Departments=c("Sales","HR","Design"))
employee3<-list(EmployeeID=103,Name="Ram kumar",Salary=11000,Departments=c("HR","Marketing","Design"))

employees<-list(employee1,employee2,employee3)


#Annual salary
annual_salary<-as.integer(employee1$Salary) * 12
annual_salary

#change Employee NAme to New Name
employee1$Name<-"Charlie Brown"
employee1$Name

#add a new department 
employee1$Departments <- c(employee1$Departments, "IT")
employee1$Departments

#create Organisation List
organization <- list(
  Name = "XYZ Corporation",
  Employees = list(employee1, employee2, employee3)  
  # Add details of at least three employees
)

#display Content of entire nested list 
print(organization)

#Access and print the annual salary of the second employee in the organization.
second_employee_salary <- organization$Employees[[2]]$Salary * 12
second_employee_salary

#print organisation name
print(organization$Name)

#Create a department_employees list
department_employees <- list()

# Group employees by department
for (employee in employees) {
  for (department in employee$Departments) {
    if (department %in% names(department_employees)) {
      department_employees[[department]] <- c(department_employees[[department]], employee)
    } else {
      department_employees[[department]] <- list(employee)
    }
  }
}

# Display the structure and content of the lists
print("Employee 1 Structure:")
print(str(employee1))



print("Organization List:")
print(str(organization))

print("Department Employees List:")
print(department_employees)
