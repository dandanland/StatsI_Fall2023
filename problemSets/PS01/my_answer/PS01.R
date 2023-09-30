#####################
# load libraries
# set wd
# clear global .envir
#####################
getwd()
setwd("/Users/danzhang/Documents/GitHub/StatsI_Fall2023/problemSets/PS01/my_answer")
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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Question-1
alpha_1<-0.1
n<-length(y)
mean_y <-mean(y)

# standard error
se<-sd(y)/sqrt(n)

# critical value of t distribution
t_score<-qt(1-alpha_1/2,df=n-1)

#calculate confidence interval
lower_90<-mean_y-t_score*se
upper_90<-mean_y+t_score*se
cat("The 90% confidence interval for the average student IQ in the school is :[",round(lower_90,2),",",round(upper_90,2),"]")

# Question-2
#Hypothesis test
mu<-100 #average IQ in all the country
alpha_2 <- 0.05
t_test_result<-t.test(y,mu=mu,alternative = "greater",alpha_2=0.05)
t_test_result
cat("t-statistic=", round(t_test_result$statistic,2),"\n")
cat("p_value=",round(t_test_result$p.value,2),"\n")

#determine whether to reject null hypothesis or not
if (t_test_result$p.value < alpha_2){
  cat("Reject null hypothesis, the school average IQ socre is higher than the average IQ score in the country.")
}else{
  cat("Failed to reject null hypothesis, there is no enough evidence to support that the school average IQ socre is higher than the average IQ score in the country.")
}


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

##  Question-1  ##

#Create scatter plots
pdf("Problem2_Question1_ScatterPlots.Pdf")
par(mfrow=c(2,3))
#Y vs X1
plot(expenditure$X1,expenditure$Y,xlab = "X1",
     ylab = "Y",
     main = "Y vs X1")
#Y vs X2
plot(expenditure$X2,expenditure$Y,xlab = "X2",
     ylab = "Y",
     main = "Y vs X2")
#Y vs X3
plot(expenditure$X3,expenditure$Y,xlab = "X3",
     ylab = "Y",
     main = "Y vs X3")
#X1 vs X2
plot(expenditure$X2,expenditure$X1,xlab = "X2",
     ylab = "X1",
     main = "X1 vs X2")
#X1 vs X3
plot(expenditure$X3,expenditure$X1,xlab = "X3",
     ylab = "X1",
     main = "X1 vs X3")
#X2 vs X3
plot(expenditure$X3,expenditure$X1,xlab = "X3",
     ylab = "X2",
     main = "X2 vs X3")
par(mfrow=c(1,1))
dev.off()
cat("As we can see from the scatter plots between Y,X1,X2 and X3. There are positive correlations between Y and X1, Y and X3, X1 and X3, X2 and X3,respectively. When the former variable increases,
    the latter variable also shows an increasing trend. The relationships between X1 and X3, X2 and X3, are stronger than the relationships between Y and X1, Y and X3.
    Besides, the correlations between Y and X2, X2 and X2 seem to be non-linear. When X2 is less than about 300, there are negative relationships between Y and X2,X1 and X2. 
    While X2 is greater than about 300, there are positive relationships between Y and X2,X1 and X2.")

##  Question-2  ##
#Create box plot
pdf("Problem2_Question2_BoxPlot.pdf")
boxplot(expenditure$Y~expenditure$Region,xlab = "Region",
        ylab = "Per Capita Expenditure on Shelters/Housing Assistance (Y)",
        xaxt="n",
        main="Y vs Region",
        col="orange",
        border="blue")
axis(1, at = 1:4, labels =c ("Northeast", "North", "South", "West"))
dev.off()
cat("According to the boxplot,we can see that region 4 has the highest per capita expenditure on housing assistance")

##  Question-3  ##
#Plot Y vs X1 again
pdf("Problem2_Question3_Plot_Y_vs_X1.pdf")
plot(expenditure$X1,expenditure$Y,xlab = "Per Capita Personal Income (X1)",
     ylab = "Per Capita Expenditure on Shelters/Housing Assistance (Y)",
     main = "Y vs X1")
dev.off()
cat("As can be observed from the scatter plot, the X-axis represents variable Per Capita Personal Income (X1) and the Y-axis represents
    variable Per Capita Expenditure on Shelters/Housing Assistance (Y). We can see that there is a positive correlation between Y and X1, 
    as X1 increases, Y also shows an upward trend. The strength of this relationship is weak and not very close. There might be some sorrelation between Y and X1.
    Further statistical analysis may help quantify the strength and significance of this relationship.")

#Reproduce a plot include region variable
pdf("Problem2_Question3_Plot_Y_X1_Region.pdf")
plot(expenditure$X1,expenditure$Y,xlab = "Per Capita Personal Income (X1)",
     ylab = "Per Capita Expenditure on Shelters/Housing Assistance (Y)",
     main = " ")

#create symbol and color vectors for different regions
regions<-unique(expenditure$Region)
symbols<-c("A","B","C","D")
colors<-c("red","green","purple","blue")

#Using different symbol and color to plot scatter points for each region
for (i in 1:length(regions)) {
  region_points<-expenditure[expenditure$Region==regions[i] , ]
  points(region_points$X1,region_points$Y,
         pch=symbols[i],
         col=colors[i])
  
}
legend("topright",legend=(names(regions)<-c ("Northeast", "North", "South", "West")),col=colors,pch=symbols,title="Region")
title(main=" Scatter Plot of Y vs X1 by Region")
dev.off()