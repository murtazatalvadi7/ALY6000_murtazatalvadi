#1 - Print your name at the top of the script.

lastName <- "Talvadi"
prefix <- "Plotting Basics: "
cat(prefix, lastName, sep = "")

#2 - Import libraries including: FSA, FSAdata, magrittr, dplyr, plotrix, ggplot2, and moments

install.packages(c("FSA","ggplot2", "plotrix", "dplyr", "magrittr", "FSAdata", "moments"))

library(FSAdata)
library(ggplot2)
library(plotrix)
library(dplyr)
library(magrittr)
library(FSA)
library(moments)

#3 - Load the BullTroutRML2 dataset (BullTroutRML2.csv)

data(BullTroutRML2)
print(BullTroutRML2)

#4 - Print the first and last 3 records from the BullTroutRMS2 dataset

firstLastRec = headtail(BullTroutRML2, n=3)
print(firstLastRec)

#5 - Remove all records except those from Harrison Lake

BullTroutFiltered <- filter(BullTroutRML2, lake == "Harrison" )
print(BullTroutFiltered)

#6 - Display the first and last 5 records from the filtered BullTroutRML2 dataset

FilteredFirstLast <- headtail(BullTroutFiltered, n=5)
print(FilteredFirstLast)

#7 - Display the structure of the filtered BullTroutRML2dataset

str(BullTroutFiltered) 

#8 - Display the summary of the filtered BullTroutRML2dataset

summary(BullTroutFiltered)


#9 - Create a scatterplot for "age" (y variable) and "fl" (x variable)

input <- BullTroutFiltered[, c('fl', 'age')]

plot(x = input$fl, y = input$age,
     xlab = "Fork Length(mm)",
     ylab = "Age(yrs)",
     xlim = c(0, 500),
     ylim = c(0, 15),       
     main = "Plot 1: Harrison Lake Trout",
     pch=16,
     cex=0.6
)

#10 - Plot an "Age" histogram

hist( BullTroutFiltered$age , main="Plot 2: Harrison Fish Age Distribution", 
      col.main="cadetblue",
      xlab = "Age(yrs)",
      ylab = "Frequency",
      col = "cadetblue", border = "red", xlim = c(0,15), ylim = c(0,15))

#11 - Create an Overdense plot as per instructions in the assignment

ages <-  BullTroutFiltered$age
ggplot(data = BullTroutFiltered, aes(x=ages)) + geom_density() +
  labs(title = "Plot 3: Harrison Density Shaded by Era",
       x = "Fork Length(mm)",
       y = "Age(yrs)") +
  geom_point(data=BullTroutFiltered, aes(x=fl, y=age, size=ages),
             alpha =0.5, color="green") +
  lims(x = c(0,500), y=c(0,15))


#12 - Create a new object called "tmp" that includes the first 3 and last 3 records of the BullTroutRML2 data set.

tmp <- rbind(head(BullTroutRML2, 3), tail(BullTroutRML2, 3))
print(tmp)


#13 - Display the "era" column (variable) in the new "tmp" object

spc <- data.frame(tmp$era)
print(spc)


#14 - Create a pchs vector with the argument values for + and x.

pchs <- c(3,4)
plot(1:10, xlim=c(1, 10), ylim=c(1, 20), 
     pch=pchs, cex=2, 
     col="blue", bg="red", lwd=2)


#15 - Create a cols vector with the two elements "red" and "gray60"

colVec <- c("red", "gray60")
dim(colVec) <- c(2,1)
print(colVec) 


#16 - Convert the tmp era values to numeric values.

fac <- factor(temp$era)
numValue <- as.numeric(fac)
print(numValue)


#17 - Initialize the cols vector with the tmp era values

cols[numValue]


#18 - Create a plot of "Age (yrs)" (y variable) versus "Fork Length (mm)" (x variable) as per instructions in the assignment

input <- BullTroutFiltered[, c('age', 'fl')]
plot(x = input$fl, y = input$age,
     xlab = "Age(yrs)",
     ylab = "Fork Length(mm)",
     xlim = c(0, 500),
     ylim = c(0, 15),       
     main = "Plot 4: Symbol & Color by Era",
     pch=pchs,
     col=cols,
     cex=1
)


#19 - Plot a regression line overlay on Plot 4 and title the new graph "Plot 5: Regression Overlay".

plot(age~fl,data = BullTroutFiltered,
     xlim=c(0,500),
     ylim=c(0,15),
     xlab="Age(yrs)",
     ylab="Fork Length(mm)",
     main="Plot 5: Regression Overlay",
     pch=pchs,
     col=cols)
regLine = lm(age~fl,data = BullTroutFiltered)
abline(regLine, lty=2, lwd=3)


#20 - Place a legend of on Plot 5 and call the new graph "Plot 6: :Legend Overlay"

plot(age~fl,data = BullTroutFiltered,
     xlim=c(0,500),
     ylim=c(0,15),
     xlab="Fork Length(mm)",
     ylab="Age(yrs)",
     main="Plot 6: Legend Overlay",
     pch=pchs,
     col=cols)
regLine = lm(age~fl, data = BullTroutFiltered)
abline(regLine, lty=2, lwd=3)

legend("topleft", legend=levels(BullTroutFiltered$era), 
       pch=pchs,
       col=cols,
       bty="n",
       title="Era",
       inset=c(0.1),
       cex = 0.90)