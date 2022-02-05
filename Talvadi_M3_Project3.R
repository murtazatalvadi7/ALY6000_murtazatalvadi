#1 - Print your name at the top of the script and load these libraries.

name <- "Murtaza Talvadi"
print(name)

install.packages(c("FSA" ,"FSAdata", "magrittr", "dplyr", "tidyr", "plyr", "tidyverse"))

library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)

#2 - Import the inchBio.csv and name the table <bio>

bio <- read.csv("/Users/itsmt/Desktop/inchBio.csv")
print(bio)

#3 - Display the head, tail and structure of <bio>

headtail(bio)
str(bio)

summary(bio)

#4 - Create an object, <counts>, that counts and lists all the species records

counts <- data.frame(bio$species)
print(counts)

#5- Display just the 8 levels (names) of the species
unique(bio$species)

#6 - Create a <tmp> object that displays the different species and the number of record of each species in the dataset.

tmp <- table(bio$species)
print(tmp)

#7 - Create a subset, <tmp2>, of just the species variable and display the first five records

tmp2 <- subset(bio,select = species)
head(tmp2, 5)

#8 - Create a table, <w>, of the species variable. Display the class of w

w <- table(bio$species)
print(w)
class(w)

#9 - Convert <w> to a data frame named <t> and display the results

t <- as.data.frame(w)
print(t)

#10 - Extract and display the frequency values from the <t> data frame
t$Freq

#11 - Create a table named <cSpec> from the bio species attribute (variable)

cSpec <- table(bio$species)
print(cSpec)
class(cSpec)

#12 - Create a table named <cSpecPct> that displays the species and percentage of records for each species

cSpecPct <- (table(bio$species)/676)
print(cSpecPct)
class(cSpecPct)

#13 - Convert the table, <cSpecPct>, to a data frame named <u>
  
u <- as.data.frame(cSpecPct)
print(u)
class(u)

summary(u)

#14 - Create a barplot of <cSpec> 

barplot(cSpec,
        main = "Fish Count",
        col = "LightGreen",
        ylab = "COUNTS",
        cex.axis = 1,
        cex.lab=1, las=2,cex.names = 0.6)

#15 - Create a barplot of <cSpecPct>

barplot(cSpecPct, 
        main="Fish Relative Frequency",
        ylim=c(0.0,0.4), 
        col="lightblue", 
        cex.names=0.5,
        las=2, ylab="%" )

#16 - Rearrange the <u> cSpec Pct data frame in descending order of relative frequency.

d <- arrange(u, desc(Freq))
print(d)

#17 - Rename the <d> columns Var 1 to Species, and Freq to RelFreq

colnames(d) <- c('Species', 'RelFreq')
print(d)

#18 - Add new variables to <d> and call them cumfreq, counts, and cumcounts

d <- mutate(d, cumfreq = cumsum(RelFreq), 
            counts = RelFreq*676, 
            cumcounts = cumsum(counts))
print(d)

#19 - Create a parameter variable <def_par> to store parameter variables

def_par <- par(no.readonly = TRUE)

#20 - Create a barplot, <pc>, with the following specifications:

pc <- barplot(d$counts, width = 1, space = 0.15, border = NA, axes = F,
              main = "Species Pareto",
              ylim = c(0, 3.05 * max(d$counts, na.rm = TRUE)), 
              ylab = "Cumulative Counts", 
              cex.names = 0.7, 
              names.arg = d$Species, 
              las = 2 )

#21 - Add a cumulative counts line to the <pc> plot

lines (pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col = "#008B8B")

#22 - Place a grey box around the pareto plot

box(col="grey")

#23 - Add a left side axis with the following specifications

axis (side = 2, at = c(0, d$cumcounts), 
      las = 1, col.axis="grey62", col="grey62", cex.axis = 0.8)

#24 - Add axis details on right side of box with the specifications

axis (side = 4, at = c(0, d$cumcounts), 
      labels = paste(c(0, round(d$cumfreq * 100)), "%", sep =""), 
      las = 1, col.axis="#008B8B", col="#008B8B", cex.axis = 0.8)

#25 - Display the finished Species Pareto Plot 

axis (side = 4, at = c(0, d$cumcounts),
      labels = paste(c(0, round(d$cumfreq * 100)), "%", sep =""), 
      las = 1, col.axis="#008B8B", col="#008B8B", cex.axis = 0.8)


