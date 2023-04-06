# 1. Data reading and manipulation

read.table()     # read text files
read.csv()       # read.table() for .csv files
edit()           # view spreadsheet-like display of a data or data frame 
read.xls()       # read Excel files. Requires "gdata" library

# 2. Working directory

setwd("/path/to/folder/")
getwd()          # displays path of working directory

# 3. Examining the data

dim(df)           # displays dimensions of the dataframe (rows, columns)
names(df)         # column names of dataframe
head(df, n=3)     # displays first 3 rows of the dataframe
                  # above can also be accomplished using df[1:3,]
tail(df, n=3)     # displays last 3 rows of the dataframe

# 4. Data frame subsets

df[1:5, c(1,6:8)] # selects rows 1-5, cols 1,6-8
df[1:5, ]         # selects rows 2-5, all columns
df[, -c(2:5)]     # selects all rows, all coulmns except 2-5

df[df$sex == 'F',]# selects only Females all columns

# 5. Adding and removing variables in a dataframe

df$Impaired = (df$GDS >= 0.5)
df$Impaired = (df$GDS >= 0.5) + 0 # plus 0 will convert it into numerical

df$GDSImp = ifelse(df$GDS >= 0.5, "impaired", "normal")

df$Impaired = NULL # removes a column from the table

rbind()            # stacks rows of dataframe
cbind()            # combines columns of dataframe
merge()            # matches and merges two dataframes

# 6. Writing ouutput files

write.csv(df, file="xxx.csv", row.names=FALSE)

# 7. Missing values

x = c(23, 45, 12, 13, NA) # create a vector of 5 numbers including one NA
is.na(x)             # displays F F F F T
mean(x)              # includes NA
mean(x, na.rm= TRUE) # NAs excluded

# 8. Variable names attaching datasets

names(df)
names(df)[c(3,4)] = c("Weight","height") # rename column names

table(weight) # gives error since it is a column in the df use attach() to resolve it.
attach(df)
table(weight) # this works
detach(df) # reduces the risk of confusion due to diff variables with same name hence attach is not recommended

# 9. Creating data

vector
x = c(20,21,22,23)
x = 20:23 # same as above

y1 = rep("yes",times=3) # replicates 3 times i.e. y = "yes","yes","yes"
y2 = c("no","no")
y = c(y1,y2) # concatenate y1 and y2

# 10. Graphics

# 10.1 Categorical variables: barcharts
prtab1 = prop.table(table(Ethnicity))
round(prtab1, dig=3)
barplot(prtab1, ylim = c(0, 0.5), ylab = "Frequency", xlab = "Ethnicity")

# 10.2 Continuous variables: histograms
hist(CD4, col="red", main="CD4 for the sample of 268 subjects")
hist(CD4, col="light blue", main="CD4 counts for the sample of 268 subjects", breaks=16)

install.packages("lattice")         	# download lattice package
library(lattice) 					# load lattice
histogram(~ CD4 | Sex, type="percent", main="CD4 count by gender")

# 10.3 Boxplots
boxplot(CD4, col=23, main="CD4")	# color indicated by number 23
boxplot(CD4 ~ Sex, col="pink", ylab = "CD4 T-cell Count", xlab="Gender", main="CD4 counts by gender", boxwex=.5, data=hiv)
bwplot(CD4 ~ Ethnicity | Sex, main="CD4 counts by gender and ethnicity", data=hiv) 

boxplot(ht$SBP.Baseline, ht$SBP.Followup)
abline(h=0)

# or use the following

with(ht, boxplot(SBP.Change, ylab="Change in SBP",
  col="coral", cex.lab=1.5, cex.axis=1.5))
abline(h=0, col=8)

install.packages("lattice")

# 10.4 Scatterplots
plot(x=CD4, y=GDS, col="blue")
plot(GDS ~ CD4, main="Scatterplot of GDS vs. CD4 counts", ylab = "Global Deficit Score", xlab = "CD4 T-cell Count", col="blue", data=hiv)

xyplot(GDS ~ CD4, main="GDS vs CD4 counts")
xyplot(GDS ~ CD4 | Sex, main="GDS vs CD4 counts by gender", data=hiv)

# 10.5 Scatterplot matrix
hiv2 = hiv[,c("Age", "Education", "CD4", "GDS", "Tscore")]
hiv2[1:5,]
pairs(hiv2)

# 10. Data distribution
prop.table(tab, margin=1) # shows percentage of rows (Adds upto 1)
prop.table(tab, margin=2) # shows percentage of column (adds upto 1)

# Numeric summaries:
summary() # five number summary
sd()      # standard deviaation
round(tab, 3) # rounds table values to 3 decimal places
barplot(tab1) # basic barplot
barplot(tab, ylim=c(0, 0.5), ylab="Proportiom", xlab="Race/Ethnicity", names.arg=c("asn",---))

# Student's t quantiles and probabilities
qnorm(0.975)       # Pr(Z>1.96) = 0.025
qt(0.975, df=25)   # Pr(T>2.06|df=25) = 0.025  
qt(0.975, df=40)   # Pr(T>2.02|df=40) = 0.025
qt(0.975, df=73)   # Pr(T>1.99|df=73) = 0.025

pnorm(-1)          # Pr(Z<-1) = 0.159
pt(-1, df=12)      # Pr(T<-1|df=12) = 0.169
pt(-1, df=73)      # Pr(T<-1|df=73) = 0.160

# Set seed value
set.seed(seed=72020)  # for reproducibility of simulations

# Clean working directory
rm(list=ls())








