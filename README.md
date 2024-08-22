# Health Insurance Dataset Analysis

## Introduction

The Health Insurance Dataset contains information on personal attributes, geographic factors, and medical insurance charges for 1338 US citizens. This dataset is useful for studying how various features, such as age, gender, BMI, family size, smoking habits, and geographic region, impact medical insurance costs.

## Variables Description

- **Age**: The insured person's age in years.
- **Sex**: Gender of the insured (male: 0, female: 1).
- **BMI**: Body Mass Index, a measure of body fat.
- **Children**: Number of dependents covered.
- **Smoker**: Whether the insured is a smoker (1) or non-smoker (0).
- **Region**: Geographic area of coverage (southwest: 1, southeast: 2, northwest: 3, northeast: 4).
- **Charges**: Medical insurance costs incurred by the insured person in $.

### Additional Variables

- **Age Category**: Age intervals created from the "age" data. Ages are grouped into categories: 15-24, 25-34, 35-44, 45-54, 55-64.
- **BMI Category**: Categorizes BMI into intervals: 15-24, 25-34, 35-44, 45-54.

## Objective

To analyze the correlation between personal attributes (age, gender, BMI, family size, smoking habits), geographic factors, and medical insurance charges ($) among 1338 US citizens. This dataset enables exploration of how these variables affect insurance costs and facilitates the development of predictive models for estimating healthcare expenses.

## Testing for Normality

We use the Kolmogorov-Smirnov test to evaluate the normality of the variables in the dataset.

- **Null Hypothesis (H0)**: The data follows a normal distribution.
- **Alternative Hypothesis (H1)**: The data does not follow a normal distribution.

| Variable | Test Type | P-value     | Decision on H0 |
|----------|-----------|-------------|----------------|
| Age      | Kolmogorov-Smirnov | 1.143e-07 | Reject         |
| BMI      | Kolmogorov-Smirnov | 0.3218  | Accept         |
| Sex      | Kolmogorov-Smirnov | 2.2e-16 | Reject         |
| Children | Kolmogorov-Smirnov | 2.2e-16 | Reject         |
| Region   | Kolmogorov-Smirnov | 2.2e-16 | Reject         |
| Charges  | Kolmogorov-Smirnov | 2.2e-16 | Reject         |
| Smoker   | Kolmogorov-Smirnov | 2.2e-16 | Reject         |

## Summary Statistics

| Variable | Min   | Max    | Mean   | 1st Quartile | Median | 3rd Quartile | SD       | Mode |
|----------|-------|--------|--------|--------------|--------|--------------|----------|------|
| Age      | 18.00 | 64.00  | 39.21  | 27.00        | 39.00  | 51.00        | 14.05    | 18   |
| BMI      | 15.96 | 53.13  | 30.66  | 26.30        | 30.40  | 34.69        | 6.10     | 32.3 |
| Sex      | 0.00  | 1.00   | 0.495  | 0.0000       | 0.0000 | 1.0000       | 0.50     | 0    |
| Children | 0.00  | 5.00   | 1.095  | 0.000        | 1.000  | 2.000        | 1.21     | 0    |
| Region   | 1.00  | 4.00   | 2.484  | 2.000        | 2.000  | 3.000        | 1.10     | 2    |
| Smoker   | 0.00  | 2.00   | 0.205  | 0.0000       | 0.0000 | 0.0000       | 0.40     | 0    |
| Charges  | 1122  | 63770  | 13270  | 4740         | 9382   | 16640        | 12110.01 | 1639.56 |

## Test of Independence and Association

- **Null Hypothesis (H0)**: There is no association between the variable(I.e variable are independent). 
- **Alternative Hypothesis (H1)**: there is an association between variable (I.e variable are dependent)

### Correlation Analysis

| Variables         | Type of Test | r-value   | P-value     | Decision on H0 |
|-------------------|--------------|-----------|-------------|----------------|
| Age vs BMI        | Spearman      | 0.107736  | 7.859e-05   | Reject         |
| Charges vs BMI    | Spearman      | 0.119396  | 1.193e-05   | Reject         |
| Charges vs Age    | Spearman      | 0.534392  | < 2.2e-16   | Reject         |

### Chi-Square Tests

| Variables         | Type of Test | X-squared | df  | P-value     | Decision on H0 |
|-------------------|--------------|-----------|-----|-------------|----------------|
| Sex vs Region     | Chi-squared   | 0.43514   | 3   | 0.9329      | Accept         |
| Sex vs Age        | Chi-squared   | 1.6405    | 46  | 1           | Accept         |
| Sex vs BMI        | Chi-squared   | 529.17    | 547 | 0.7001      | Accept         |
| Sex vs Children   | Chi-squared   | 0.73521   | 5   | 0.981       | Accept         |
| Sex vs Smoker     | Chi-squared   | 7.3929    | 1   | 0.006548    | Reject         |
| Children vs Age   | Chi-squared   | 450.98    | 230 | 2.2e-16     | Reject         |
| Children vs BMI   | Chi-squared   | 2771.4    | 2735| 0.3091      | Accept         |
| Children vs Region| Chi-squared   | 13.773    | 15  | 0.5428      | Accept         |
| Children vs Smoker| Chi-squared   | 6.8877    | 5   | 0.2291      | Accept         |
| Region vs Age     | Chi-squared   | 136.78    | 138 | 0.5133      | Accept         |
| Region vs BMI     | Chi-squared   | 2940.8    | 1641| 2.2e-16     | Reject         |
| Region vs Smoker  | Chi-squared   | 7.3435    | 3   | 0.06172     | Accept         |
| Smoker vs Age     | Chi-squared   | 52.429    | 46  | 0.2388      | Accept         |
| Smoker vs BMI     | Chi-squared   | 571.72    | 547 | 0.2247      | Accept         |
| Charges vs Sex    | Kruskal-Wallis| 0.1204    | 1   | 0.7286      | Accept         |
| Charges vs Children| Kruskal-Wallis| 29.487   | 5   | 1.86e-05    | Reject         |
| Charges vs Smoker | Kruskal-Wallis| 588.52    | 1   | < 2.2e-16   | Reject         |
| Charges vs Region | Kruskal-Wallis| 4.7342    | 3   | 0.1923      | Accept         |

### Linear Regression Model

**Model**:
Charges = -13361.12 + 257.29(age) + 131.11(sex) + 332.57(bmi) + 479.37(children) + 23820.43(smoker) + 353.64(region)



**Interpretation**:
- **Intercept**: -13361.12 (charges when all variables are zero)
- **Age**: Each additional year increases charges by $257.29.
- **Sex**: Not statistically significant.
- **BMI**: Each unit increase in BMI raises charges by $332.57.
- **Children**: Each additional child increases charges by $479.37.
- **Smoker**: Smokers incur significantly higher charges.
- **Region**: Charges vary by geographic region.

## Charge Split Analysis

### Kruskal-Wallis Test for Charge-Split

Hypotheses:

- **Null Hypothesis (H0)**: There is no significant difference in central tendencies of the predictor across charge-split

- **Alternative Hypothesis (H1)**: There is a significant difference in central tendencies of the predictor across charge-split

| Variable | Chi-squared | df | P-value   | Decision on H0 |
|----------|-------------|----|-----------|----------------|
| Age      | 348.75      | 1  | < 2.2e-16 | Reject         |
| BMI      | 10.649      | 1  | 0.001102  | Reject         |
| Sex      | 0.011951    | 1  | 0.9159    | Accept         |
| Children | 94.506      | 5  | < 2.2e-16 | Reject         |
| Region   | 6.2817      | 3  | 0.0976    | Accept         |
| Smoker   | 612.98      | 1  | < 2.2e-16 | Reject         |

## Conclusion

- **Significant Correlations**: Age, BMI, and Smoking Status have significant impacts on charges.
- **Normality Tests**: Age, Sex, Children, Region, and Charges do not follow normal distribution. BMI follows a normal distribution.
- **Chi-Square Tests**: Most categorical variables do not show significant associations except for the smoking status.
- **Linear Regression Model**: Provides insights into how different factors influence medical insurance charges.

## References

1. Insurance Charges Dataset from Kaggle
2. R Documentation: [Kolmogorov-Smirnov Test](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/ks.test), [Spearman Correlation](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/cor.test), [Chi-Square Test](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/chisq.test), [Kruskal-Wallis Test](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/kruskal.test)

## How to Run

1. Load the dataset into R.
2. Install necessary packages: `install.packages("dplyr")`, `install.packages("ggplot2")`, `install.packages("Hmisc")`.
3. Run the provided R script to perform data cleaning, exploratory analysis, and statistical tests.
