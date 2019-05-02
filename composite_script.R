############################
### Data version 2 #########
############################


library(readxl)
datv2 <- read_excel("ISSSR_datasetR_M2.xlsx")




# Rescaling in a similar scale of 1 to 100

library(dplyr)
datv2 <- datv2%>%
  mutate(Diabetes=scales::rescale(`DALY Diabetes`,to=c(1,100)))
datv2 <- datv2%>%
  mutate(IBD=scales::rescale(`DALY IBD`,to=c(1,100)))
datv2 <- datv2%>%
  mutate(Asthma=scales::rescale(`DALY Asthma`,to=c(1,100)))



# Create the composite variable by taking sum of three DALY variables. For this composite
# variable range is 3 to 300. 


datv2$composite_daly <- datv2$Diabetes+datv2$IBD+datv2$Asthma


# Summary statistics
summary(datv2)


# Bi-variate analysis:
library(GGally)
ggpairs(datv2[,c(2,3,10)])


# Model fitting:
fit_composite <- lm(datv2$composite_daly~datv2$`CHE / GDP`+datv2$`Any BF age 12m`)

fit_composite_gdp <- lm(datv2$composite_daly~datv2$`CHE / GDP`)
fit_composite_bf <- lm(datv2$composite_daly~datv2$`Any BF age 12m`)

# We can use this normal q q plot to find the outliers and check the normality
plot(fit_composite)




# Seems 15, 27 and 52 data points are outliers. Let's remove and run the model again
dim(datv2)

datv2 <- datv2[-c(15,52,27), ]

dim(datv2) # making sure data removed

# Again Model fitting:
fit_composite2 <- lm(1/datv2$composite_daly~datv2$`CHE / GDP`+datv2$`Any BF age 12m`)


# Checking the assumption of the model graphically
shapiro.test(fit_composite2$residuals)



# Still the residual are not normal even after removing the outliers and making 
# inverse transformation. 

# Exploring histogram:
hist(datv2$composite_daly,20)
hist(log(datv2$composite_daly),20)
hist(1/sqrt(datv2$composite_daly),20)
hist(1/datv2$composite_daly,20)

# seems non is appropritate to make the data normal. Rather we should explain it in the report.
# and I am reporting the actual model after removing the outliers. 

# Checking multicollinearity
car::vif(fit_composite)

# vif values greater than 5 is problematic. So, in our case it's fine.

# Checking heteroscedasticity
lmtest::bptest(fit_composite)  # Breusch-Pagan test


write.csv(datv2,"Data_versiton2.csv") # exporting the composite score to maek the transformation plot.


#install.packages("stargazer")  #Use this to install it, do this only once
library(stargazer)
stargazer(fit_composite,fit_composite_gdp,fit_composite_bf, type="text", out="composite.htm")

