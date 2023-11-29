v1<-c(1,2,TRUE,3,"hjknasfk",5);

v3<-c(1,2,3,4)
v2=seq(1:10)
typeof(v1)
typeof(v2)
typeof(v3)
x<-c("Jan","Feb","March","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")
y=x[c(3,2,7)]
z=x[c(TRUE,FALSE,FALSE,TRUE,TRUE,FALSE,FALSE,TRUE)]
q=x[c(-2,-4)]

print("Index Based")
print(y)
print("Logical Indexing:")
print(z)
print("Negative Indexing:")
print(q)

a1<-c(1,2,3,4,5)
a2<-c(12,34,21,44,2)
summ<-a1+a2
product<-a1*a2
quotient<-a1/a2
diffrence<-a1-a2

print(summ)
print(product)
print(diffrence)
print(quotient)

sorted_a2 = sort(a2)
print(sorted_a2)

rev_sort_a2 = sort(a2,decreasing = TRUE)
print(rev_sort_a2)

order_a2=order(a2)
print(order_a2)


sales_data<-c(45,60,35,75,80,62,48,53,69,72,40,55)
print(sales_data)
sum_of_sales<-sum(sales_data)
print(sum_of_sales)

avg_sales<-sum_of_sales/length(sales_data)
print(avg_sales)

maxSales<-order(sales_data,decreasing = TRUE)[1]
print(maxSales)

minSales<-order(sales_data)[1]
print(minSales)

sales_data[3]=sales_data[3]+sales_data[3]/10
print(sales_data[3])
print(sales_data)

sorted_sales<-sort(sales_data)
reverse_sales<-sort(sales_data,decreasing = TRUE)
#------------------------------------------------------------------

#creation of matrix1 using matrix function
mat1<-matrix(c(10,5,3,6),nrow = 2,ncol=2,byrow = TRUE,dimnames = list(c("m1R1","m1R2"),c("m1C1","m1C2")))
print(mat1)

#creation of matrix2 using matrix function
mat2<-matrix(c(1,4,8,7),nrow = 2,ncol=2,byrow = FALSE)
print(mat2)

#class of matrix using matrix function
print("Class of Mat1 ")
print(class(mat1))
print("Class of Mat2 ")
print(class(mat2))

rownames(mat2)=c("m2R1","m2R2")
colnames(mat2)=c("m2C1","m2C2")


matSum<-mat1+mat2
matDiff<-mat1-mat2
matProd<-mat1*mat2
matDiv<-mat1/mat2

print("Sum")
print(matSum)

print("Product")
print(matProd)

print("Diffrence")
print(matDiff)

print("Quotient")
print(matDiv)


print("Mat 1 Diamention")
mat1dim<-dim(mat1)
print(mat1dim)

print("Diamension Names of Mat1")
print(dimnames(mat1))

print("Diamension Names of Mat2")
print(dimnames(mat2))


#creating matrix using colum bind
cmat<-cbind(c(1,2,3),c(4,5,6))

#creating matrix using row bind
rmat<-cbind(c(1,2,3),c(4,5,6))

print("Rbinded Matrix")
print(rmat)

print("Cbinded Matrix")
print(cmat)

#creating matrix using changing matrix diamension
changeDmat<-c(1,2,4,2,2,5,7,4,3)
class(changeDmat)
dim(changeDmat)<-c(3,3)
class(changeDmat)
print(changeDmat)

#Accessing matrix elements matname[row,col]
print(mat1[1,2])
print(mat1[1,])
print(mat1[,1])
print(mat1[c(1,2),])
print(mat1[,c(2,1)])
print(mat1[c(1,2),c(2,1)])
print(mat1[])
print(mat1[,])
print(changeDmat[c(TRUE,FALSE,TRUE),c(TRUE,TRUE,FALSE)])
print(changeDmat[c(1,2),c(TRUE,TRUE,FALSE)])
print(changeDmat[changeDmat>2])
print(changeDmat[changeDmat%%2==0])
print(mat1[,"m1C1"])
print(mat1["m1R1",])
print(mat1["m1R2","m1C1"])
print(mat1[TRUE,"m1C1"])
print(mat1[FALSE,c("m1C1","m1C2")])
print(changeDmat[2:3,1:2])
print(changeDmat[1:2,])

#changing value of a matrix

mat1[2,2]<-20 #direct access
print(mat1)

changeDmat[changeDmat>2]<-44 #conditional access
print(changeDmat)

#transpose of matrix
tranMat1<-t(mat1)
print(tranMat1)

#determinant of  matrix
detMat1<-det(mat1)
print(mat1)
print(detMat1)


#adding col and row to existing matrix using rbind and cbind
mat3<-rbind(mat1,c(10,11))
print(mat3)

#removing a row ;removes last row 
mat3<-mat3[1:2,];mat3
print(mat3)

#find inverse of matrix
invMat1<-solve(mat1)
print(invMat1)

#creating identity matrix
identityMat=matrix(c(1,0,0,1),nrow = 2,ncol = 2)
print(identityMat)

#checking orthogonality
mulmattran<-tranMat1==invMat1
c=0
for (x in mulmattran) {
  if(x==FALSE){
    c=1
    break
  }
}
if(c==0){
  print("It is orthogonal")
}else{
  print("It is not orthogonal")
}


#------------------------------------------------------------------
#factor
x1<-c("Dec","Jan","May","Nov")
month_levels<-c("Jan","Feb","March","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec")

y1<-factor(x1,levels=month_levels)
print(y1)
a<-c(1,2,3,4,5)
b<-as.ordered(a)

is.factor(y1)
as.ordered(y1)
sort(y1)
is.ordered(y1)
is.ordered(b)
 
#list
list_info<-list("Blue","YEllow",c(12,3,24),TRUE,13.5,103.5);
length(list_info)
list_info[2]= NULL
length(list_info)
print(list_info)


#releveling a factor

sizes <- factor(c("Samll","Medium","Large"))
sizes

sizes<-relevel(sizes,"Medium")
sizes

sizes<-factor(sizes,levels = rev(levels(sizes)))
sizes

#Dataframe 
df<- data.frame(
                NumberCol = 1:3,
                TextcCol = c("FirstText", "SecondText","ThirdText"),
                BoolCol = c(TRUE,TRUE,FALSE),
                DoubleCol = c(1.2,4.4,7.4),
                #stringsAsFactors = FALSE
                stringsAsFactors = TRUE
                
                
                )
df
#structure of dataframe
str(df)
df<-rbind(df,c(4,"FourthText",FALSE,5.5))


v1<-1:3
v2<-c("FirstText", "SecondText","ThirdText")
v3<-c(1.2,4.4,7.4)

#vector into a dataframe
df2<-data.frame(col1=v1,col2=v2,col3=v3)
df2



#list to dataframe
listdf<-list(item1=1:2,item2=c("SecondText","ThirdText"),item3=c(4.4,7.4))
newldf<-as.data.frame(listdf)
newldf

#is.data.frame(newldf)

#matrix to dataframe
matA<-matrix(c(10,5,3,6),nrow = 2,ncol=2,byrow = TRUE,dimnames = list(c("m1R1","m1R2"),c("m1C1","m1C2")))
matdf<-as.data.frame(matA)
matA

attributes(df)

df[2:3,]

df[c(2,3)]


df[c("NumberCol","TextcCol")]
df[1:2,c(1,3)]
v<-c(1,2,4)
df[,v]
df[,2]
df[,2]
df[,2,drop =FALSE]

mtcars
mtcars[3]

mtcars[10,]
max(mtcars[1])

a<-mtcars[mtcars$am==0,]
nrow(a)
b<-mtcars[mtcars$vs==0,]
row.names(b)

sixCyllinder<-mtcars[mtcars$cyl==6,]
sixCyllinder

foutcylinder<-mtcars[mtcars$cyl==4,]
maxfcylmpg<-max(foutcylinder$mpg)
maxfcylmpg

nrow(foutcylinder[foutcylinder$am==0])
foutcylinder[foutcylinder$am==0,]

mean(mtcars[(mtcars$cyl==8&mtcars$am==0),]$wt)


#=========================================================
student_scores<-data.frame(
  StudentID= sample(1:100),
  Gender=sample(c("Male","Female"),100,replace = TRUE),
  Age= sample(18:25,100,replace = TRUE),
  Marks=round(runif(100,min=40,max=99))
)

print(student_scores)
print(getwd())
setwd("C:\\Users\\aravi\\Desktop\\StatisticsUsingR\\Classwork")
print(getwd())
write.csv(student_scores,"student_scores.csv",row.names = FALSE)
data<-read.csv("student_scores.csv")
print(data)
max_marks<-max(data$Marks)
max_marks

min_age<-min(data$Age)
min_age

print(is.data.frame(data))

age_greater_than_20<-data[data$Age>20,]
age_greater_than_20

female_with_mark_greater_than_50<-data[data$Gender=="Female"&data$Marks>50,]
female_with_mark_greater_than_50

write.csv(female_with_mark_greater_than_50,"female>50Marks.csv",row.names = FALSE)
head(data)
tail(data,10)

#read a text file in R
textfile <- readLines("sample.txt", encoding = "UTF-8") 
textfile
write.table(data,"sample.txt",sep="\t",quote=F,row.names = F)
x

precip
rivers
discoveries


#use stripchart for qualitative data
stripchart(precip,xlab = "rainfall")
stripchart(rivers,method = "jitter")
stripchart(discoveries,method = "stack")

#histogram
hist(precip,main="")
hist(precip,freq = TRUE,main = "Precip")
hist(precip,freq = FALSE,breaks = 200)


carsdata<-as.data.frame(mtcars)
carsdata

barplot()
str(carsdata)
carsdata$gear<-as.ordered(carsdata$gear)
carsdata$am<-as.factor(carsdata$am)
carsdata$vs<-as.factor(carsdata$vs)
carsdata$cyl<-as.ordered(carsdata$cyl)
carsdata$carb<-as.ordered(carsdata$carb)

hist(carsdata$wt,xlab = "Car Weight",ylab = "Number of Cars",main = "Weight/Count Graph of Cars")
barplot(height = table(carsdata$cyl))
stripchart(carsdata$wt,xlab = "Car Weight")

#-------------------------------------------------------







  
  
