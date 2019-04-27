# Loading the data. The first 2 row deleted manually in excel before loading the data. 
library(readxl)
ISSSR <- read_excel("OLS Regression Project/ISSSR_dataset.xlsx", 
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

# Making sure variables are in correct class:
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

# Exploring the summary of the model
summary(fit1)

# Checking the assumption of the model graphically
plot(fit1)


# Checking multicollinearity
car::vif(fit1)

# vif values greater than 5 is problematic. So, in our case it's fine.



# Checking heteroscedasticity
lmtest::bptest(fit1)  # Breusch-Pagan test


# Result interpretation for heteroscedasticity

# The test have a p-value greater that a significance level of 0.05, 
# therefore we can not reject the null hypothesis that the variance of the residuals is constant
# and infer that heteroscedasticity is not indeed present, 
# thereby confirming our graphical inference.


### Model after taking log transformation. The distribution of dependent variable is 
# is positively skweed, as a result we choose log transformation.

# Model fitting:
fit2 <- lm(log(`DALY Asthma`)~., data = dat1)
# Exploring the summary of the model
summary(fit2)
# Checking the assumption of the model graphically
plot(fit2)
# Checking multicollinearity
car::vif(fit2)
# Checking heteroscedasticity
lmtest::bptest(fit2)  # Breusch-Pagan test

