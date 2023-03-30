###### Continuous outcomes: Autism study
aut = read.csv("./Datasets/Autism.csv")
dim(aut)
aut[1:5,]
table(aut$exprlang)  # group size

#### Explore data
boxplot(sage ~ exprlang, data=aut)
abline(h=13, col=8); 
abline(h=13/2, col=8)
abline(h=13/4, col=8)

boxplot(logsage ~ exprlang, data=aut)
abline(h=log(13), col=8)
abline(h=log(13/2), col=8)
abline(h=log(13/4), col=8)

(logsage.mean = with(aut, tapply(logsage, exprlang, mean)))
exp(logsage.mean)  # Vineland age, geometric mean 
with(aut, tapply(logsage, exprlang, sd))

#### One-way ANOVA, F-test
aut$group = as.factor(aut$exprlang) # factor with 3 levels: 1, 2, 3
fit = aov(logsage ~ group, data=aut)
summary(fit)  # F-test, ANOVA table

#### Pairwise comparisons
### Tukey correction
(tfit = TukeyHSD(fit))  # includes p-values, CI
plot(TukeyHSD(fit), las=1, col=4)
exp(tfit$group)  ## VSA13 mean ratios



### Pairwise comparisons, Bonferroni (p-values)
pairwise.t.test(aut$logsage, aut$group, p.adjust.method="bonferroni")

### Pairwise comparisons, Holm (p-values)
pairwise.t.test(aut$logsage, aut$group, p.adjust.method="holm")


#### Non-parametric 1-way ANOVA
### Overall test: Kruskal-Wallis, sage
kruskal.test(sage ~ group, data=aut)

### Pairwise comparisons, Bonferroni
pairwise.wilcox.test(aut$sage, aut$group, p.adj="bonf")

### Pairwise comparisons, Holm
pairwise.wilcox.test(aut$sage, aut$group, p.adj="holm")
