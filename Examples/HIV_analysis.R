hiv = read.csv("./Datasets/HIVNeurocognition.csv")

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

### Data Analysis Script in R

hiv[1:5, ]		# take a quick look at the data
attach(hiv)	# attach the dataset

# 1.1 Summary statistics for continuous variables
mean(CD4); sd(CD4)
summary(hiv)

# 1.2 Summary statistics for binary and categorical variables
tab1 = table(Ethnicity)
tab1
prtab1 = prop.table(tab1); prtab1
round(prtab1, digits=3)

tab2 = table(Sex, Ethnicity); tab2
addmargins(tab2)
round(prop.table(tab2, margin = 1), digits = 3)   # row sums = 1
round(prop.table(tab2, margin = 2), digits = 3)	 # column sums round(prop.table(tab2), dig = 3) # total = 1 if no margin given

hiv$Age50 = ifelse(hiv$Age >= 50, "50+", "<50")
attach(hiv)	# so the new variable Age50 is attached
tab3 = table(Age50, Ethnicity, Sex); tab3
# See also xtabs(): 	xtabs( ~ Age50 + Ethnicity + Sex, data=hiv)
# See also ftable(): 	ftable(tab3)


# Class exercise 1
# 1
hiv$Impaired = ifelse(hiv$GDS >= 0.5, "impaired", "normal")
hiv[1:5,]    	# look at the first 5 rows of the data
# 2
tab4 = table(hiv$Sex, hiv$Impaired) 
	#or: tab4 = with(hiv, table(Sex, Impaired))
prtab4 = prop.table(tab4, margin = 1) # marginal frequencies by row (by Sex)
prtab4	 	# examine the table
# 3
barplot(prtab4, beside = T, ylim = c(0,0.8), xlab = "Neurocognitive Impairment", ylab = "Frequency", col = c("red", "blue"))
legend("topright", legend = c("Women", "Men"), fill = c("red", "blue"))
# end of exercise


# 2 Graphics

# 2.1 Categorical variables: barcharts
prtab1 = prop.table(table(Ethnicity))
round(prtab1, dig=3)
barplot(prtab1, ylim = c(0, 0.5), ylab = "Frequency", xlab = "Ethnicity")

# 2.2 Continuous variables: histograms
hist(CD4, col="red", main="CD4 for the sample of 268 subjects")
hist(CD4, col="light blue", main="CD4 counts for the sample of 268 subjects", breaks=16)

install.packages("lattice")         	# download lattice package
library(lattice) 					# load lattice
histogram(~ CD4 | Sex, type="percent", main="CD4 count by gender")

# 2.3 Boxplots
boxplot(CD4, col=23, main="CD4")	# color indicated by number 23
boxplot(CD4 ~ Sex, col="pink", ylab = "CD4 T-cell Count", xlab="Gender", main="CD4 counts by gender", boxwex=.5, data=hiv)
bwplot(CD4 ~ Ethnicity | Sex, main="CD4 counts by gender and ethnicity", data=hiv) 
install.packages("lattice")
# 2.4 Scatterplots
plot(x=CD4, y=GDS, col="blue")
plot(GDS ~ CD4, main="Scatterplot of GDS vs. CD4 counts", ylab = "Global Deficit Score", xlab = "CD4 T-cell Count", col="blue", data=hiv)

xyplot(GDS ~ CD4, main="GDS vs CD4 counts")
xyplot(GDS ~ CD4 | Sex, main="GDS vs CD4 counts by gender", data=hiv)

# 2.5 Scatterplot matrix
hiv2 = hiv[,c("Age", "Education", "CD4", "GDS", "Tscore")]
hiv2[1:5,]
pairs(hiv2)


# Class exercise 2
# 1a
hist(GDS, col = "seagreen", las = 1)
# 1b
boxplot(GDS, col = "steelblue", ylab = "GDS", las = 1, cex.lab = 1.5, cex.axis = 1.5)
# 1c 
boxplot(GDS ~ Ethnicity, col = "coral", ylab = "GDS", xlab = "Ethnicity", las = 1, 
        cex.lab = 1.5, cex.axis = 1.3, main = "GDS by ethnicity", 
        names = c("Asian", "Black", "Hispanic", "Other", "White"))
# 2a
plot(Education ~ Age, pch = 19, col = "blue", cex.axis = 1.5, cex.lab = 1.5, las = 1)
# 2b
cor.test(Age, Education)
# 2c
cor.test(Age, Education, method="spearman")
# end of exercise

detach(hiv)

