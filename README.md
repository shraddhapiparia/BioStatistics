# Statistical Analysis Toolbox

This repository contains a collection of statistical analysis methods for different types of data. It provides implementations and explanations for various statistical tests and regression models. Below is a summary of the methods available:

## Continuous Outcome - One Group

- Confidence Interval (CI)
- One-sample t-test
- Binomial test

## Two Paired Samples

- Paired-samples t-test
- Confidence Interval (CI)

## Two Independent Samples

- Independent samples t-test
- Confidence Interval (CI)
- Wilcoxon Rank-Sum Test (RST)

## 3+ Independent Samples

- One-way Analysis of Variance (ANOVA)
  - F-test
  - Pairwise comparisons with multiplicity correction

## Correlation and Simple Linear Regression

- Pearson and Spearman correlation
- Regression equation and test of correlation

## Binary Outcome - One Group

- Confidence Interval (CI)
- Z-test
- Binomial test

## Two Paired Samples

- McNemar test
- Confidence Interval (CI)

## Two Independent Samples

- Pearson's chi-square test
- Fisher's exact test
- Confidence Interval (CI)
- Odds Ratio (OR)

## 3+ Independent Samples

- Overall Pearson's chi-square test
- Fisher's exact test
- Pairwise comparisons

## Regression Models

1. Linear regression for continuous outcomes
2. Logistic regression for binary outcomes
3. Cox proportional hazards regression for time-to-event data

### Issues for every regression model:

- Model equation
- Parameter interpretation
- Binary, continuous, categorical predictors
- Confidence interval, Wald test for H0: βj = 0
- Analysis of variance/deviance, F test/likelihood ratio test, degrees of freedom
- Overall test
- Test for individual predictors (categorical, binary, continuous)
- Comparison of nested models
- Wald tests and CI's for contrasts and linear combinations
- Models with interactions
- Model assumptions, diagnostics, and troubleshooting
- Residual plot, added variable plot
- Transformation of variables and non-linear variable effects
- Causal inference for the effect of the primary predictor
- Confounders, mediators, causal diagrams
- Methods for adjusting for confounders
- Observational studies vs randomized controlled trials

# The repository is structured as follows:
## Resources Folder

The `Resources` folder contains the following:

### Basic Commands

The `Basic Commands` file introduces the fundamentals of statistical inference. It covers topics such as data summarization, data visualization, probability, normal and binomial distribution, sampling, central limit theorem, confidence intervals, and hypothesis testing.

### Statistical Tests

The `Statistical Tests` file includes commands for commonly used statistical analyses. It covers tests for continuous (scale) and binary response variables in the case of one, two, and three or more groups. The statistical methods covered include independent samples and paired samples t-tests, tests based on ranks, Pearson's chi-square and Fisher's exact tests for binary variables, paired tests for continuous and binary variables, one-way ANOVA, correlation, and simple linear regression.

## Models Folder

The `Models` folder includes more advanced regression-based statistical analyses. It covers the following models:

- Simple linear regression and correlation analysis
- Multiple linear regression
- Logistic regression
- Cox proportional hazards models


Feel free to explore the repository and the individual method folders for detailed explanations and code examples.

