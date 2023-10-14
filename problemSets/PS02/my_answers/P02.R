getwd()
setwd("/Users/danzhang/Documents/GitHub/StatsI_Fall2023/problemSets/PS02/my_answers")

##Question 1##
## Q1-a ##
#create the resulting data table
resulting_data<-matrix(c(14,6,7,7,7,1),nrow = 2,byrow = TRUE)
colnames(resulting_data)<-c("Not Stopped","Bribe requested","Stopped/given warning")
rownames(resulting_data)<-c("Upper class","Lower class")

#calculate the expected frequencies
calculate_expected_freq<-function(data_table){
  #calculate the sum of rows and columns of the data table
  total_samples<-sum(data_table)
  row_sums<-rowSums(data_table)
  col_sums<-colSums(data_table)
  #calculate the frequencies
  expected_freq<-outer(row_sums,col_sums)/total_samples
  return(expected_freq)
  }
  
#calculate Chi square statistic
chi_square_statistic<-sum((resulting_data - calculate_expected_freq(resulting_data))^2/calculate_expected_freq(resulting_data))

#calculate degree of freedom
degree_of_freedom<-(nrow(resulting_data)-1)*(ncol(resulting_data)-1)

#output the test statistic and degree of freedom
cat("Chi-square statistic: ",chi_square_statistic,"\n")
cat("Degree of freedom: ",degree_of_freedom,"\n")

#examine the results
chisq_result<-chisq.test(resulting_data)
chisq_result

## Q1-b ##
alpha<-0.1
p_value<-pchisq(chi_square_statistic,degree_of_freedom,lower.tail = F)
p_value

if (p_value<alpha){
  cat("P-Value is approximately equal to :",p_value,2,"<",alpha,"Based on the chi-square test, at a 99% confidence level,
      we reject the null hypothesis indicating that there is a statistically significant dependence between
      driver class and whether officers were more or less likely to solicit a bribe from drivers.")
}else if (p_value<alpha) {
  cat("P-Value is approximately equal to :",p_value,2,">",alpha,"Based on the chi-square test, at a 99% confidence level,
      we cannot reject the null hypothesis. Which indicates that we do not have enough evidence to support 
      there is a statistically significant dependence between driver class and whether officers were 
      more or less likely to solicit a bribe from drivers")
}

## Q1-c ##
#calculate the standardized residuals for each cell
residuals_for_cell<-(resulting_data - calculate_expected_freq(resulting_data))/sqrt(calculate_expected_freq(resulting_data))
residuals_for_cell

## Q1-c ##
'A standardized residual with a larger absolute value indicates that a cell contributes more significantly to the chi-square statistic.
In this sample, the observations of ‘Not stopped’ from two classed cannot give more significant contributions to the find whether there is
a statistically significant dependence between driver class and police officer bribe. Also, the sign of a standardized residual can help indicate 
the direction of the deviation of the observed frequency and the expected frequency. A positive value implies that the observed count is greater 
than expected, while a negative value suggests that the observed count is less than expected。'


##Question 2##
## Q2-a ##

#State a null and alternative (two-tailed) hypothesis
'
Null hypohesis: The reservation policy does not have an effect on the number of new or repaired drinking water facilities in the
villages: beta=0.
Alternative hypothesis: The reservation policy has an effect on the number of new or repaired drinking water facilities in the
villages: beta!=0.
'

## Q2-b ##
#load the subset data from West Bengal
data<-read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

#use bivariate regression
reserved_data<-as.factor(data$reserved)
bivariate_reg<-lm(data$water ~ reserved_data,data=data)
summary(bivariate_reg)

## Q2-c ##
"
Based on the data, we can see from the bivariate regression results. When there is no reserved policy, 
the estimated mean value of new or repaired drinking water facilities is 14.738. When there is reserved policy, the estimated 
mean number of new or repired drinking water facilities in villages is 9.252 higher than when there is not.

In conclusion, the reservation of female politicians has a positive impact on the number of new or repqired drinking water 
facilities in villages, compared to the case where no female politicians are reserved. The p-value for this result is 
less than the significance level of 0.05, indicating that the effect is significant.
"

     