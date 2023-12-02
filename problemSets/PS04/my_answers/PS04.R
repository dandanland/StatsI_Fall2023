# Install packages and load the data
install.packages(car)
library(car)
data(Prestige)
help(Prestige)

# install.packages("texreg")
library(texreg)

# Set working directory for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#####################
# Question 1
#####################

# (a) Create a new variable "professional"
Prestige$professional<-ifelse(Prestige$type=="prof",1,0)

# (b) Run a linear model
# Run a linear model without interaction
model<-lm(prestige ~ income + professional + income:professional ,data = Prestige)

# (c) Write the prediction equation based on the result
texreg(list(model), digits=3)
# presitge= 20.804 + 0.003*income + 38.120*professional - 0.002*income*professional

# (f) The effect of a $1,000 increase in income on prestige score for professional occupations
# Extract the income coefficient and interaction coefficient
income_coe<-coef(model)["income"]
interaction_coe<-coef(model)["income:professional"]
# Calculate the marginal effects
# marginal effect= alpha(prestige)/alpha(income)
marginal_effect_1000<-income_coe+interaction_coe*1000
cat("Estimated change in professional occupation's prestige score when income increases by $1,000:", marginal_effect_1000, "\n")

# (g) The effect of changing one’s occupations from non-professional to professional when her income is $6,000
# Extract income coefficient and interaction coefficient
income_value<-6000
marginal_effect_6000<-income_coe+interaction_coe*income_value
cat("Prestige change for switching from non-professional to professional when income is $6,000:", marginal_effect_6000, "\n")


#——————————
# Marginal effect
#__________


#####################
# Question 2
#####################

# (a) Does placing these yard signs in a precinct affect vote share?
# The null hypothesis and alternative hypothesis for hypothesis testing are as follows:
# H0: beta_lawn_signs=0
# Ha: beta_lawn_signs!=0
# beta_lawn_signs is the coeffiecient of "Precinct assigned lawn signs" variable 

# Get the coefficients and standard error from the model results
coefficients<-c(0.042,0.042,0.302)
standard_error<-c(0.016,0.013,0.011)

# Calculate the t-statistic
t_stat_1<- coefficients[1]/standard_error[1]

# Calculate the two-sides p-value
p_value_1<-2*pt(-abs(t_stat_1),df=128)

cat("t-statistic:", t_stat_1, "\n")
cat("p-value:", p_value_1, "\n")
# As we can see p-value<0.05, we can reject null hypothesis. Which means that the precinct where yard signs are placed has a significant effect on vote share. 

# (b) Whether being next to precincts with these yard signs affects vote share
# The null hypothesis and alternative hypothesis for hypothesis testing are as follows:
# H0: beta_adjacent_to_lawn_signs=0
# Ha: beta_adjacent_to_lawn_signs!=0
# beta_adjacent_to_lawn_signs is the coeffiecient of "Precinct adjacent lawn signs" variable 

#Calculate the t-statistic
t_stat_2<-coefficients[2]/standard_error[2]

# Calculate the two-sides p-value
p_value_2<-2*pt(-abs(t_stat_2),df=128)

cat("t-statistic:", t_stat_2, "\n")
cat("p-value:", p_value_2, "\n")
# AS we can see p-value<0.05, we can reject the null hypothesis. Which means that being adjacent to a precinct where yard signs are placed has a significant impact on vote share.

# (c) Interpret the coefficient for the constant term substantively
# The coefficient of the constant term is the estimate of the response variable (vote share) when all other variables are zero. In this model, the coefficient on the constant term is 0.302, indicating that, controlling for other variables, the average vote share in precincts that are not assigned yard signs or are not adjacent to precincts that are assigned yard signs is 0.302.

# (d) Evaluate the model fit for this regression
# R square is used to evaluate the model fit. R^2 reflects the proportion of the model’s explanatory variables to the variation in the dependent variable.
# In this model, R^2=0.094. Indicates that the model can explain 9.4% of the variation in the dependent variable. This suggests that the model may not have captured other important factors that were not modeled, such as other preferences or backgrounds of voters.












