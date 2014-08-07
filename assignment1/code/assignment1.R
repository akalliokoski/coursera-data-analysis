# Coursera Data Analysis - Assignment 1
# https://www.coursera.org/course/dataanalysis
#
# See ../writing/assignment1-analysis.pdf for more detailed documentation.

# Load the libraries
library(VIM)
library(ggplot2)
library(plyr)
library(RColorBrewer)

# Download the peer-to-peer loans data
# NOTE: Data no longer available online.
# download.file(url="https://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda", destfile="./data/loansData.rda", method="curl")

# Load the data
load(file="../data/loansData.rda")
loansRaw <- loansData

# Data munging
###############################

# Exploratory analysis
summary(loansData)
lapply(loansData, class)

# Interest Rate to numeric value
string_rates <- as.character(loansData$Interest.Rate)
numeric_rates <- as.numeric(sub("%", "", string_rates))
loansData$Interest.Rate.Numeric <- numeric_rates

# Debt To Income Ratio to numeric value
string_ratio <- as.character(loansData$Debt.To.Income.Ratio)
numeric_ratio <- as.numeric(sub("%", "", string_ratio))
loansData$Debt.To.Income.Ratio.Numeric <- numeric_ratio

# FICO Range to numeric value
fico_string <- as.character(loansData$FICO.Range)
fico_to_numeric <- function(x){as.numeric(substr(x, 1, 3))+2}
fico_numeric <- sapply(fico_string, fico_to_numeric)
loansData$FICO.Range.Numeric <- fico_numeric

# Remove the missing values.
# First, list the rows that have one or more missing values,
# and then remove them.
loansData[!complete.cases(loansData),]
sum(is.na(loansData$Interest.Rate.Numeric))
aggr(loansData, prop=FALSE, numbers=TRUE)
# Remove missing values
loansData_complete <- loansData[complete.cases(loansData),]
ld <- loansData_complete

# Analysis
################################

# Model selection on the basis of exploratory analysis 
# and prior knowledge of the relationship between loan length and interest rate.

# Standard multiple regression
lm <- lm(ld$Interest.Rate.Numeric 
         ~ ld$FICO.Range.Numeric + ld$Loan.Length + ld$FICO.Range.Numeric*ld$Loan.Length)
summary(lm)
confint(lm)

# PDF figure making
# See ../figures/assignment1-figure.pdf 
# and ../figures/assignment1-figurecaption.pdf.
################################

# Set up a function that makes colors prettier
mypar <- function(a = 1, b = 1, brewer.n = 8, brewer.name = "Dark2", ...) {
  par(mar = c(2.5, 2.5, 1.6, 1.1), mgp = c(1.5, 0.5, 0))
  par(mfrow = c(a, b), ...)
  palette(brewer.pal(brewer.n, brewer.name))
}

# Set size of axes
cx = 1.3
# Colors
color_36months = FALSE*1+1
color_60months = TRUE*1+1

# Save figure to PDF file
# NOTE: Close the file with dev.off() when finished 
pdf(file = "../figures/assignment1-figure.pdf", height = 4, width = 3 * 4)
mypar(mfrow = c(1, 3))

# Kernel density plot to view the distribution of the interest rate 
# with 36 and 60 months loan lengths
dens <- density(ld$Interest.Rate.Numeric)
dens_36months <- density(ld$Interest.Rate.Numeric[which(ld$Loan.Length=="36 months")])
dens_60months <- density(ld$Interest.Rate.Numeric[which(ld$Loan.Length=="60 months")])
plot(dens,lwd=3,col="black", xlab="Interest Rate (%)", ylab="Density", 
     cex.axis = cx, cex.lab = cx, ylim=c(0,0.105), main="")
lines(dens_36months,lwd=3,col=color_36months)
lines(dens_60months,lwd=3,col=color_60months)
legend(20, 0.1, 
       legend=c("Total density", "60 months", "36 months"), 
       lwd=c(2.5, 2.5, 2.5), 
       col=c("black", color_60months, color_36months))
 
# Scatterplot with linear model coefficient lines
plot(ld$FICO.Range.Numeric, 
     ld$Interest.Rate.Numeric, 
     col=((ld$Loan.Length=="60 months")*1+1), 
     pch = 19, 
     xlab = "FICO Score", ylab = "Interest Rate (%)", 
     cex.axis = cx, cex.lab = cx)
abline(c(lm$coeff[1],lm$coeff[2]),col=((FALSE)*1+1),lwd=3)
abline(c(lm$coeff[1] + lm$coeff[3],lm$coeff[2] +lm$coeff[4]),col=((TRUE)*1+1),lwd=3)
legend(780, 24, 
       legend=c("60 months", "36 months"), 
       pch=c(19, 19), 
       col=c(color_60months, color_36months))

# Residuals
plot(ld$Interest.Rate.Numeric, 
     lm$residuals, 
     col=((ld$Loan.Length=="60 months")*1+1), 
     pch = 19, 
     xlab = "Interest Rate (%)", ylab = "Residuals", 
     cex.axis = cx, cex.lab = cx)
legend(20, -5,
       legend=c("60 months", "36 months"), 
       pch=c(19, 19), 
       col=c(color_60months, color_36months))

# Close the PDF file
dev.off()
