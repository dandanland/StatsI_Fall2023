#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# Question 1.1
# Run a regression considering voteshare and difflog variables
regression1<-lm(voteshare ~ difflog, data=inc.sub)
summary(regression1)
# Interpretation: In this model, difflog has a positive effect on voteshare, with each unit increase increasing the voteshare estimate by 0.04167. The baseline voteshare estimate by 0.57903 when difflog is zero.

# Question 1.2
# Draw a scatter plot of voteshare and difflog
pdf("Scatter plot and regression line of voteshare and difflog in R.pdf")
plot(inc.sub$difflog,inc.sub$voteshare,main="Relationship between voteshare and difflog in R",
     xlab="difflog",
     ylab="voteshare",
     pch=20,
     col="blue")
# Add the regression line
abline(regression1$coefficients[1],regression1$coefficients[2],col="red")
dev.off()

# Question 1.3
# Save the residuals of the model
residuals_reg1<-residuals(regression1)
head(residuals_reg1)

# Question 1.4
# Write the prediction equation
# voteshare= 0.579031 + 0.041666*difflog

# Question 2.1
# Run a resgression considering presvote and difflog variables
regression2<-lm(presvote ~ difflog, data=inc.sub)
summary(regression2)

# Question 2.2
# Draw a scatter plot of presvote and difflog
pdf("Scatter plot and regression line of presvote and difflog in R.pdf")
plot(inc.sub$difflog,inc.sub$presvote,main="Relationship between presvote and difflog in R",
     xlab="difflog",
     ylab="presvote",
     pch=20,
     col="blue")
# Add the regression line
abline(regression2$coefficients[1],regression2$coefficients[2],col="red")
dev.off()

# Question 2.3
# Save the residuals of the model
residuals_reg2<-residuals(regression2)
head(residuals_reg2)

# Question 2.4
# Write the prediction equation
# presvote= 0.507583 + 0.023837*difflog

# Question 3.1
# Run a resgression considering voteshare and presvote variables
regression3<-lm(voteshare ~ presvote, data=inc.sub)
summary(regression3)

# Question 3.2
# Draw a scatter plot of voteshare and presvote
pdf("Scatter plot and regression line of voteshare and presvote in R.pdf")
plot(inc.sub$presvote,inc.sub$voteshare,main="Relationship between voteshare and presvote in R",
     xlab="presvote",
     ylab="voteshare",
     pch=20,
     col="blue")
# Add the regression line
abline(regression3$coefficients[1],regression3$coefficients[2],col="red")
dev.off()

# Question 3.3
# Write the prediction equation
# voteshare= 0.441330 + 0.388018*presvote

# Question 4.1
# Run a regression considering residuals of regression1 and residuals of regression2
regression4<-lm(residuals_reg1 ~ residuals_reg2)
summary(regression4)

# Question 4.2
# Draw a scatter plot of residuals_reg1 and residuals_reg2
pdf("Scatter plot and regression line of residuals_reg1 and residuals_reg2 in R.pdf")
plot(residuals_reg2,residuals_reg1,main="Relationship between residuals_reg1 and residuals_reg2 in R",
     xlab="residuals_reg2",
     ylab="residuals_reg1",
     pch=20,
     col="blue")
# Add the regression line
abline(regression4$coefficients[1],regression4$coefficients[2],col="red")
dev.off()

# Question 4.3
# Write the prediction equation
# residuals_reg1 = -1.942e-18 + (2.569e-01)*residuals_reg2

# Question 5.1
# Run a regression considering voteshare, difflog and presvote variables
regression5<-lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(regression5)

# Question 5.2
# Write the prediction equation
# voteshare = 0.4486442 + 0.0355431*difflog + 0.2568770*presvote

# Question 5.3
# From the regression results of regression5 and regression4, we can see that the Residuals, Coefficients, Residual standard error, Multiple R-squared, Adjusted R-squared, F-statistic and other indicators of regression4 and regression5 are the same.
# The reason for this situation is that the model of regression4 is obtained by regressing residuals_reg1 on residuals_reg2, while the model of regression5 is obtained by directly regressing voteshare on difflog and presvote. Since residuals_reg1 and residuals_reg2 are respectively the residuals of voteshare on difflog and the residuals of presvote on difflog, the residuals, coefficients, etc. in the regression results will be the same. This is also why you see these same values in both regression results.











