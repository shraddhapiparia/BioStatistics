galton = read.csv("./Datasets/galton.csv") # parents & children heights
mgal = galton[galton$male==1,]  # male children only
mgal$son = mgal$height          # male child's height, rename

#### Plot sons vs father's height
plot(son ~ father, data=mgal,
     xlim=c(60,80), ylim=c(60,80), pch=16, col=4,
     ylab="Son's height", xlab="Father's height")
points(mean(mgal$father), mean(mgal$son), pch=3, col=6, cex=1.2)
abline(a=0, b=1, col=8)

## Father's height = 73in
points(son ~ father, data=mgal, pch=16, col=2, subset=(father==73))
mean.73 = with(mgal, mean(son[father==73]))
points(73, mean.73, pch=18, col=8, cex=3)

## Father's height = 65in
points(son ~ father, data=mgal, pch=16, col=3, subset=(father==65))
mean.65 = with(mgal, mean(son[father==65]))
points(65, mean.65, pch=18, col=8, cex=3)

#### Run simple linear regression
fit = lm(son ~ father, data=mgal)
summary(fit)  # reg line: y = 38.259 + 0.448*x

abline(fit, col=1, lwd=1.2)  # add reg line to plot

## compute standardized slope:
(r = 0.44775/sd(mgal$son)*sd(mgal$father))
cor.test(mgal$son, mgal$father)

## 95% CI for intercept, slope
confint(fit)
plot(fit) # check residual of wired points that don't satisfy assumption

## Test of no correlation
cor.test(mgal$son, mgal$father)

## Analysis of variance table
anova(fit) # not much need to use this coz summary table does it and more comprehensive

