# Loading the data. The first 2 row deleted manually in excel before loading the data. 
library(readxl)
ISSSR <- read_excel("ISSSR_dataset.xlsx", 
                            na = "NA")



# Keeping important variables only:

dat <- ISSSR[,c("GNI per capita (constant 2010 US$)", 
              "CHE / GDP",
              "Any BF age 12m",
              "DALY Asthma",
              "DALY Diabetes",
              "DALY IBD",
              "DALY Anxiety")]
# Exploring the data head
head(dat)

# Exploring the variable types
str(dat)

# Rounding the Any BF age 12m 
dat$`Any BF age 12m` <- round(dat$`Any BF age 12m`,0)

# Summary statistic for each variabl
summary(dat)


#########################
### For DALY Asthma #####
#########################

# Subsetting for Asthma
dat1 <- dat[,c("GNI per capita (constant 2010 US$)", 
                 "CHE / GDP",
                 "Any BF age 12m",
                  "DALY Asthma")]

# Bi-variate analysis:
library(GGally)
ggpairs(dat1)


# Model fitting:
fit1 <- lm(`DALY Asthma`~., data = dat1)

# Checking the assumption of the model graphically
shapiro.test(fit1$residuals)

# The null hypothesis for shapiro test is "Data follows normal distribution". So, the 
# p-value need to be greater than level of significance to interpret that the data is normal. In this
# case the p-value is less than 0.05 (if we consider 5% level of significance), so the null
# hypothesis is rejected and conclude that the residual of fit1 are not normal.
# So, need transformation of dependent variable.

###  The distribution of dependent variable is is positively skweed,
# as a result we choose log transformation.

# Model fitting: Model after taking log transformation

fit2 <- lm(log(`DALY Asthma`)~., data = dat1)

# Checking normality
shapiro.test(fit2$residuals) # seems fine


# Exploring the summary of the model
summary(fit2)

# None of the independent variable have any statistically significant impact on DALY Asthma at 5% level of significance,
# because all the p-values are greater than 0.05


# Checking multicollinearity
car::vif(fit2)

# vif values greater than 5 is problematic. So, in our case it's fine.



# Checking heteroscedasticity
lmtest::bptest(fit2)  # Breusch-Pagan test


# Result interpretation for heteroscedasticity

# The test have a p-value greater that a significance level of 0.05, 
# therefore we can not reject the null hypothesis that the variance of the residuals is constant
# and infer that heteroscedasticity is not indeed present. 




#########################
### For DALY Diabetes ###
#########################

# Subsetting for Asthma
dat2 <- dat[,c("GNI per capita (constant 2010 US$)", 
               "CHE / GDP",
               "Any BF age 12m",
               "DALY Diabetes")]

# Bi-variate analysis:
library(GGally)
ggpairs(dat2)


# Model fitting:
fit3 <- lm(`DALY Diabetes`~., data = dat2)

# Checking normality
shapiro.test(fit3$residuals)

# Residual of fit3 not normal. So, we need to use transformation. 


# Model fitting:
fit4 <- lm(log(`DALY Diabetes`)~., data = dat2)

# Checking normality
shapiro.test(fit4$residuals)  # seems fine


# Exploring the summary of the model
summary(fit4)

# None of the independent variable have any statistically significant impact on DALY Diabetes at 5% level of significance,
# because all the p-values are greater than 0.05


# Checking multicollinearity
car::vif(fit4)

# vif values greater than 5 is problematic. So, in our case it's fine.

# Checking heteroscedasticity
lmtest::bptest(fit4)  # Breusch-Pagan test



####################
### For DALY IBD ###
####################

# Subsetting for Asthma
dat3 <- dat[,c("GNI per capita (constant 2010 US$)", 
               "CHE / GDP",
               "Any BF age 12m",
               "DALY IBD")]

# Bi-variate analysis:
library(GGally)
ggpairs(dat3)


# Model fitting:
fit5 <- lm(`DALY IBD`~., data = dat3)

# Checking normality
shapiro.test(fit5$residuals)

# So, we need to use transformation. 

# Model fitting:
fit6 <- lm(log(`DALY IBD`)~., data = dat3)

# Checking normality
shapiro.test(fit6$residuals) # residual still not normal.
# So, we explore the data transformation graph on pdf file and try to get idea by 
# which transformation we can make the residual normal. And we found 1/sqrt() transformation
# would work for our DALY IBD.

# Model fitting:
fit7 <- lm(1/sqrt(`DALY IBD`)~., data = dat3)

# Checking normality
shapiro.test(fit7$residuals) # Data seems normal now

# Model summary:

summary(fit7)


# Checking multicollinearity
car::vif(fit7)

# Checking heteroscedasticity
lmtest::bptest(fit7)  # Breusch-Pagan test



#########################
### For DALY Anxiety ###
#########################

# Subsetting for Asthma
dat4 <- dat[,c("GNI per capita (constant 2010 US$)", 
               "CHE / GDP",
               "Any BF age 12m",
               "DALY Anxiety")]

# Bi-variate analysis:
library(GGally)
ggpairs(dat4)


# Model fitting:
fit8 <- lm(`DALY Anxiety`~., data = dat4)

# Checking normality
shapiro.test(fit8$residuals) # Not normal, so need transformation.

# Model fitting:
fit9 <- lm(log(`DALY Anxiety`)~., data = dat4)

# Checking normality
shapiro.test(fit9$residuals) # Seems fine

# Model summary:

summary(fit9)

# Checking multicollinearity
car::vif(fit9)

# Checking heteroscedasticity
lmtest::bptest(fit9)  # Breusch-Pagan test


