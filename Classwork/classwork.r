v1<-c(1,2,TRUE,3,"hjknasfk",5);
v3<-c(1,2,3,4)
v2=seq(1:10)
typeof(v1)
typeof(v2)
typeof(v3)
x<-c("Jan","Feb","March","Apr","May","June","July","Aug")
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





















