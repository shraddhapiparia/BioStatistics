#### 1. 95%CI for BMI, Patient Activation
library(readxl)
pa <- read_excel("./Datasets/PatientActivation.xlsx")
# pa = PatientActivation
View(pa)                                                       

## Select DM2 participants
table(pa$Disease)            # Disease=1 for DM2, N=422
pa.dm2 = pa[pa$Disease==1,]  # select only people with DM2, all columns
dim(pa.dm2)

## Test H0: mean(BMI) = 27.8 in this group
summary(pa.dm2$BMI)          # xbar = 28.61, N=422-1=421
sd(pa.dm2$BMI, na.rm=TRUE)   # s = 4.2845

## Compute 95% CI for BMI
t.test(pa.dm2$BMI)           # 27.8 < (28.2, 29.0)
with(pa.dm2, t.test(BMI))    # same as above

## Test H0: mean(BMI) = 27.8 in this group
qt(0.975, df=420)            # 1.965
t.test(pa.dm2$BMI, mu=27.8)  # t_stat = 3.887

## Test H0: mean(BMI) = 28.8 in this group
#....