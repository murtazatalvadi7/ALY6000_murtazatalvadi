#1 Print my name

myName <- "Murtaza Talvadi"
print(myName)

#2 Install the vcd package

install.packages("vcd")

#3 Import the vcd library

library(vcd)

#4 Plot a sales ~ temp scatter plot using the data below:
#Sales data: (7,11,15,20,19,11,18,10,6,22)
#Temperature data: (69,81,77,84,80,97,87,70,65,90)

sales <- c(7,11,15,20,19,11,18,10,6,22)
temperature <- c(69,81,77,84,80,97,87,70,65,90)
cor(sales, temperature)
plot(sales, temperature)

#5 Find the mean temperature
#Temperature data: (69,81,77,84,80,97,87,70,65,90)

meanValue <- mean(temperature)
print(meanValue)

#6 Delete the 3rd element from the sales vector
#sales <- c(7,11,15,20,19,11,18,10,6,22)

updSales <- sales[-3]
print(updSales)

#7 Insert 16 as the 3rd element into the sales vector

updSales2 <- append (updSales, 16, 2 )
print(updSales2)

#8 Create a vector <names> with elements : Tom, Dick, Harry

name <- c("Tom", "Dick", "Harry")
print(name)

#9 Create a 5 row and 2 column matrix of 10 integers

myMatrix <- matrix(c(1:10), nrow = 5, ncol = 2)
print(myMatrix)

#10 Create a data frame <icSales> with sales and temp attributes

icSales <- data.frame(sales, temperature)
print(icSales)

#11 Display the data frame structure of icScales

structure <- str(icSales)

#12 Display a summary of the icScales data frame

summary(icSales)

#13 Import the dataset Student.csv

studentData <- read.csv("/Users/itsmt/Desktop/Student.csv")
print(studentData)

#14 Display only the variable names of the Student.csv dataset

ls(studentData)