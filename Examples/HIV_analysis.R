hiv = read.csv("HIVNeurocognition.csv")

# check dimensions of dataframe
dim(hiv)

# check if fields are strings or integers
str(hiv)

# check mean, sd, five-number summary of Age
mean(hiv$Age)
sd(hiv$Age)
summary(hiv$Age)

# get frequency of races and their relative freq

# get summary of female patients
female_pat = hiv[hiv$Sex=='F',]
summary(female_pat)

# get summary of CD4 for older patients >= 50
older_pat = hiv[hiv$Age >= 50,]
summary(older_pat$CD4)

# plot histogram of CD4 for older patients >= 50
hist(older_pat$CD4)
