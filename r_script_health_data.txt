sink("output.txt")
getwd()
print(getwd())
data <- read.csv("Health-Insurance-Dataset.csv", header = TRUE)
attach(data)
head_data <- head(data, 10)
print(head_data)
#data cleaning
# Check for missing values in the charges variable
missing_values <- sum(is.na(data$charges))

# Impute missing values with the mean
data$charges[is.na(data$charges)] <- mean(data$charges, na.rm = TRUE)

print(length(charges))
library(VIM)
library(lmtest)
library(car)
library(ggplot2)
library(corrplot)
library(smatr)
library(nlme)
library(MASS)
library(dunn.test)
mp <- aggr(data, col = c('navyblue' , 'yellow'),
                    numbers = TRUE, sortVars = TRUE,
                    labels = names(data), cex.axis = .7,
                    gap=3, ylab=c("Missing data","Pattern"))




data$sex <- ifelse(data$sex == "male", 0, 1)

data$smoker <- ifelse(data$smoker == "yes", 1, 0)

data$region <- as.numeric(factor(data$region, levels = c("southwest", "southeast", "northwest", "northeast")))
counts_sex <-table(data$sex)
counts_children <-table(children)
counts_smoker <-table(data$smoker)
counts_region <-table(data$region)


hist(age, col="skyblue", xlab = "Age", main = "Distribution of Age")

barplot(counts_sex, col="skyblue", xlab = "Sex (0 - Male, 1 - Female)", ylab = "Frequency", main = "Distribution of Sex")

hist(bmi, col = "skyblue", main = "Distribution of BMI", ylab = "BMI")

barplot(counts_children, col="skyblue", xlab = "Children", main = "Distribution of Children")

barplot(counts_smoker, col="skyblue", xlab = "Smoker(0 - No, 1 - yes)", main = "Distribution of Smoker")

barplot(counts_region, col="skyblue", xlab = "Region", main = "Distribution of Region")

hist(charges, col = "skyblue", main = "Distribution of Charges", ylab = "Charges")

print("SD")
sd_age<-sd(age)
sd_bmi<-sd(bmi)
sd_children<-sd(children)
sd_sex<-sd(data$sex)
sd_region<-sd(data$region)
sd_charges<-sd(charges)
sd_smoker<-sd(data$smoker)

print(sd_age)
print(sd_bmi)
print(sd_children)
print(sd_sex)
print(sd_region)
print(sd_charges)
print(sd_smoker)

mode_percent <- function(x) {
  tab <- table(x)
  mode_val <- as.numeric(names(tab[tab == max(tab)]))
  print(mode_val)
  percentage <- prop.table(tab) * 100
  max_percentage <- max(percentage)
  return(max_percentage)
}

print("mode percentage")

mode_percent_age<-mode_percent(age)
mode_percent_bmi<-mode_percent(bmi)
mode_percent_children<-mode_percent(children)
mode_percent_sex<-mode_percent(data$sex)
mode_percent_region<-mode_percent(data$region)
mode_percent_charges<-mode_percent(charges)
mode_percent_smoker<-mode_percent(data$smoker)

print(mode_percent_age)
print(mode_percent_bmi)
print(mode_percent_children)
print(mode_percent_sex)
print(mode_percent_region)
print(mode_percent_charges)
print(mode_percent_smoker)

print("range")
range_age<-diff(range(age))
range_bmi<-diff(range(bmi) )
range_children<-diff(range(children))
range_sex<-diff(range(data$sex))
range_region<-diff(range(data$region))
range_charges<-diff(range(charges))
range_smoker<-diff(range(data$smoker))

print(range_age)
print(range_bmi)
print(range_children)
print(range_sex)
print(range_region)
print(range_charges)
print(range_smoker)
print(summary(data))
print(str(data))


# question 2
# variable sex
# H0: The variable sex and other variable are independent. 
# Create age categories
age_intervals <- cut(age, breaks = c(15, 25, 35, 45, 55, 65), labels = FALSE, right = FALSE)


data$age_category <- age_intervals
# Create BMI categories
bmi_intervals <- cut(bmi, breaks = c(15, 25, 35, 45, 55), labels = FALSE, right = FALSE)


data$bmi_category <- bmi_intervals
print(head(data))
sex_children<-ggplot(data, aes(x = sex, y = children)) +
  geom_point(aes(color = factor(children)),size=4)+
  labs(x = "sex", y = "children", title = "sex vs children") +
  geom_smooth(method = lm)
print(sex_children)

sex_region<-ggplot(data, aes(x = sex, y = region)) +
  geom_point(aes(color = factor(region)),size=4) +
  labs(x = "sex", y = "region", title = "sex vs region") +
  geom_smooth(method = lm)
print(sex_region)

sex_smoker<-ggplot(data, aes(x = sex, y = smoker)) +
  geom_point(aes(color = factor(smoker)),size=4) +
  labs(x = "sex", y = "smoker", title = "sex vs smoker") +
  geom_smooth(method = lm)
print(sex_smoker)

sex_bmi_category<-ggplot(data, aes(x = sex, y = bmi_category)) +
  geom_point(aes(color = factor(bmi_category)),size=4) +
  labs(x = "sex", y = "bmi_category", title = "sex vs bmi_category") +
  geom_smooth(method = lm)
print(sex_bmi_category)

sex_age_category<-ggplot(data, aes(x = sex, y = age_category)) +
  geom_point(aes(color = factor(age_category)),size=4) +
  labs(x = "sex", y = "age_category", title = "sex vs age_category") +
  geom_smooth(method = lm)
print(sex_age_category)

children_age_category<-ggplot(data, aes(x = children, y = age_category)) +
  geom_point(aes(color = factor(age_category)),size=4) +
  labs(x = "children", y = "age_category", title = "children vs age_category") +
  geom_smooth(method = lm)
print(children_age_category)
children_bmi_category<-ggplot(data, aes(x = children, y = bmi_category)) +
  geom_point(aes(color = factor(bmi_category)),size=4) +
  labs(x = "children", y = "bmi_category", title = "children vs bmi_category") +
  geom_smooth(method = lm)
print(children_bmi_category)

children_region<-ggplot(data, aes(x = children, y = region)) +
  geom_point(aes(color = factor(region)),size=4) +
  labs(x = "children", y = "region", title = "children vs region") +
  geom_smooth(method = lm)
print(children_region)

children_smoker<-ggplot(data, aes(x = children, y = smoker)) +
  geom_point(aes(color = factor(smoker)),size=4) +
  labs(x = "children", y = "smoker", title = "children vs smoker") +
  geom_smooth(method = lm)
print(children_smoker)

region_smoker<-ggplot(data, aes(x = region, y = smoker)) +
  geom_point(aes(color = factor(smoker)),size=4) +
  labs(x = "region", y = "smoker", title = "region vs smoker") +
  geom_smooth(method = lm)
print(region_smoker)

region_age_category<-ggplot(data, aes(x = region, y = age_category)) +
  geom_point(aes(color = factor(age_category)),size=4) +
  labs(x = "region", y = "age_category", title = "region vs age_category") +
  geom_smooth(method = lm)
print(region_age_category)

region_bmi_category<-ggplot(data, aes(x = region, y = bmi_category)) +
  geom_point(aes(color = factor(bmi_category)),size=4) +
  labs(x = "region", y = "bmi_category", title = "region vs bmi_category") +
  geom_smooth(method = lm)
print(region_bmi_category)

smoker_bmi_category<-ggplot(data, aes(x = smoker, y = bmi_category)) +
  geom_point(aes(color = factor(bmi_category)),size=4) +
  labs(x = "smoker", y = "bmi_category", title = "smoker vs bmi_category") +
  geom_smooth(method = lm)
print(smoker_bmi_category)

smoker_age_category<-ggplot(data, aes(x = smoker, y = age_category)) +
  geom_point(aes(color = factor(age_category)),size=4)  +
  labs(x = "smoker", y = "age_category", title = "smoker vs age_category") +
  geom_smooth(method = lm)
print(smoker_age_category)
age_bmi<-ggplot(data, aes(x = age, y = bmi)) +
  geom_point(aes(color = factor(age_category)),size=4) +
  labs(x = "age", y = "bmi", title = "age vs bmi") +
  geom_smooth(method = lm)
print(age_bmi)
charges_children<-ggplot(data, aes(x = charges, y = children)) +
  geom_point(aes(color = factor(children)),size=4)+
  labs(x = "charges", y = "children", title = "charges vs children") +
  geom_smooth(method = lm)
print(charges_children)
charges_sex<-ggplot(data, aes(x = charges, y = sex)) +
  geom_point(aes(color = factor(sex)),size=4)+
  labs(x = "charges", y = "sex", title = "charges vs sex") +
  geom_smooth(method = lm)
print(charges_sex)
charges_region<-ggplot(data, aes(x = charges, y = region)) +
  geom_point(aes(color = factor(region)),size=4) +
  labs(x = "charges", y = "region", title = "charges vs region") +
  geom_smooth(method = lm)
print(charges_region)

charges_smoker<-ggplot(data, aes(x = charges, y = smoker)) +
  geom_point(aes(color = factor(smoker)),size=4) +
  labs(x = "charges", y = "smoker", title = "charges vs smoker") +
  geom_smooth(method = lm)
print(charges_smoker)

charges_bmi_category<-ggplot(data, aes(x = charges, y = bmi)) +
  geom_point(aes(color = factor(bmi_category)),size=4) +
  labs(x = "charges", y = "bmi_category", title = "charges vs bmi_category") +
  geom_smooth(method = lm)
print(charges_bmi_category)

charges_age_category<-ggplot(data, aes(x = charges, y = age)) +
  geom_point(aes(color = factor(age_category)),size=4) +
  labs(x = "charges", y = "age_category", title = "charges vs age_category") +
  geom_smooth(method = lm)
print(charges_age_category)




chi_sex_region<-table(sex, region)
chi_test_sex_region<-chisq.test(chi_sex_region)
print(chi_test_sex_region)
#accept H0 p>0.05 varible are independent

chi_sex_age<-table(sex, data$age_category)
chi_test_sex_age<-chisq.test(chi_sex_age)
print(chi_test_sex_age)
#accept H0 p>0.05 varible are independent

chi_sex_bmi<-table(sex,data$bmi_category)
chi_test_sex_bmi<-chisq.test(chi_sex_bmi)
print(chi_test_sex_bmi)
#accept H0 p>0.05 varible are independent

chi_sex_children<-table(sex,children)
chi_test_sex_children<-chisq.test(chi_sex_children)
print(chi_test_sex_children)
#accept H0 p>0.05 varible are independent

chi_sex_smoker<-table(sex,smoker)
chi_test_sex_smoker<-chisq.test(chi_sex_smoker)
print(chi_test_sex_smoker)
#reject H0 p<0.05 varible are dependent

# variable children
# H0: The variable children and other variable are independent. 
chi_children_age<-table(children, data$age_category)
chi_test_children_age<-chisq.test(chi_children_age)
print(chi_test_children_age)
#reject H0 p<0.05 varible are dependent

chi_children_bmi<-table(children, data$bmi_category)
chi_test_children_bmi<-chisq.test(chi_children_bmi)
print(chi_test_children_bmi)
#accept H0 p>0.05 varible are independent

chi_children_region<-table(children, region)
chi_test_children_region<-chisq.test(chi_children_region)
print(chi_test_children_region)
#accept H0 p>0.05 varible are independent

chi_children_smoker<-table(children, smoker)
chi_test_children_smoker<-chisq.test(chi_children_smoker)
print(chi_test_children_smoker)
#accept H0 p>0.05 varible are independent



# variable region
# H0: The variable region and other variable are independent. 
chi_region_age<-table(region, data$age_category)
chi_test_region_age<-chisq.test(chi_region_age)
print(chi_test_region_age)
#accept H0 p>0.05 varible are independent

chi_region_bmi<-table(region, data$bmi_category)
chi_test_region_bmi<-chisq.test(chi_region_bmi)
print(chi_test_region_bmi)
#accept H0 p>0.05 varible are independent

chi_region_smoker<-table(region, smoker)
chi_test_region_smoker<-chisq.test(chi_region_smoker)
print(chi_test_region_smoker)
#accept H0 p>0.05 varible are independent


# variable smoker
# H0: The variable smoker and other variable are independent. 
chi_smoker_age<-table(smoker, data$age_category)
chi_test_smoker_age<-chisq.test(chi_smoker_age)
print(chi_test_smoker_age)
#accept H0 p>0.05 varible are independent

chi_smoker_bmi<-table(smoker, data$bmi_category)
chi_test_smoker_bmi<-chisq.test(chi_smoker_bmi)
print(chi_test_smoker_bmi)

#using kruskal test

kt_sex_charges<- kruskal.test(charges~sex,data=data)
print(kt_sex_charges)
kt_children_charges<- kruskal.test(charges~children,data=data)
print(kt_children_charges)
kt_smoker_charges<- kruskal.test(charges~smoker,data=data)
print(kt_smoker_charges)
kt_region_charges<- kruskal.test(charges~region,data=data)
print(kt_region_charges)


#H0: data are normaliy distributed
charges_normality_test <-ks.test(charges, "pnorm", mean = mean(charges), sd = sd(charges))
print(charges_normality_test)
# we reject  H0  p-value < 0.05 data are not normally distributed

#nomality test  ks test
#H0: data are normaliy distributed
age_nomality_test <-ks.test(age,"pnorm", mean = mean(age), sd = sd(age))
print(age_nomality_test)
# we reject  H0  p-value < 0.05  data are not normally distributed

# H0: data are normaliy distributed
sex_nomarlity_test<-ks.test(data$sex,"pnorm", mean = mean(data$sex), sd = sd(data$sex))
print(sex_nomarlity_test)
# we reject  H0  p-value < 0.05  data are not normally distributed

#H0 : data are normally distributed
bmi_nomarlty_test <- ks.test(bmi,"pnorm", mean = mean(bmi), sd = sd(bmi))
#accept H0 p-value > 0.05 data are normally distributed
print(bmi_nomarlty_test)

#H0 : Data are normally distributed
children_normality_test <- ks.test(children, "pnorm",mean = mean(children),sd=sd(children))
#reject H0 p value<0.05 Children are not normally distributed
print(children_normality_test)

#H0 : data are  normally distributed
smoker_normality_test <- ks.test(data$smoker, "pnorm",mean = mean(data$smoker),sd=sd(data$smoker))
print(smoker_normality_test)
#reject H0  p <0.05 data are not normally distributed

# Normality test for Region variable
#H0 data are normally distributed
region_normality_test <- ks.test(data$region, "pnorm",mean = mean(data$region),sd=sd(data$region))
print(region_normality_test)
#reject H0 p<0.05 data are not normally distributed



#correlation
# H0: There is no relationship between age and charges
#is my correlation statically signficant?
print("cor test age and bmi")
cor_age_bmi <-cor(age, bmi, method = "spearman")
print(cor_age_bmi)

r_age_bmi<-cor.test(age,bmi, method="spearman" ,exact=FALSE)
print(r_age_bmi)
#reject H0 p-value < 0.05 r=0.534 there is a strong positive signficant relationship
age_vs_bmi<-qplot(age, bmi, data=data, xlab="age", ylab="bmi", main="age vs  bmi")+stat_smooth(method = lm)+geom_point()
print(age_vs_bmi)

#accept H0 p>0.05 varible are independent
cor_all<-cor(data)
print(cor_all)

corrplot(cor_all, method="circle") #color circle fill

qqnorm(charges, main = "charges")
qqline(charges)



#varible age 
#normality test qqplot
qqnorm(age, main = "age")
qqline(age)



#correlation
# H0: There is no relationship between age and charges
#is my correlation statically signficant?

cor_age_charges <-cor(age, charges, method = "spearman")
print(cor_age_charges)

r_age_charges<-cor.test(age,charges, method="spearman" ,exact=FALSE)
print(r_age_charges)
#reject H0 p-value < 0.05 r=0.534 there is a strong positive signficant relationship
age_vs_charges<-qplot(age, charges, data=data, xlab="age", ylab="charges", main="age vs  charges")+stat_smooth(method = lm)+geom_point()
print(age_vs_charges)

#variable sex
#normality test qqplot

qqnorm(data$sex, main = "sex")
qqline(data$sex)

#

# Is the data significally important
# H0: Sex has no significant effect on Charges
# sex_and_charges_test<-wilcox.test(data$sex,charges,paired = TRUE)
# #reject H0  p<0.05 , Wilcoxon signed rank test sex has a significant effect on charges
# sex_vs_charges<-qplot(data$sex, charges, data=data, xlab="sex", ylab="charges", main="sex vs  charges")
# print(sex_and_charges_test)
# print(sex_vs_charges)


#variable BMI
qqnorm(bmi,main="BMI")
qqline(bmi)


#correlation
# H0: There is no relationship between bmi and charges
#is my correlation statically signficant?

cor_bmi_charges<-cor(bmi,charges, method = "spearman" )
print(cor_bmi_charges)
r_bmi_charges<-cor.test(bmi,charges ,method="spearman", exact=TRUE)
# there is a weak positive significant  relationship between bmi and charges (r=0.119)
print(r_bmi_charges)
bmi_vs_charges<-qplot(bmi, charges, data=data, xlab="bmi", ylab="charges", main="bmi vs  charges")+stat_smooth(method = lm)+geom_point()
print(bmi_vs_charges)


#variable children
qqnorm(children, main ="Children")
qqline(children)
str(children)


# Is the data significally important
# H0: children has no significant effect on Charges
# children_and_charges_test<-wilcox.test(children,charges,paired = TRUE)
# # reject H0 p <0.05 Children have a significant effect on charges
# print(children_and_charges_test)
# color <- cut(children, breaks = c(-Inf, 1, 2, 3, 4, 5, Inf), labels = c("0", "1", "2", "3", "4", "5"))
# children_vs_charges <- ggplot(data = data, aes(x = children, y = charges, color = color)) +
#   geom_point() +
#   labs(x = "Children", y = "Charges", title = "Children vs. Charges") +stat_smooth(method = lm)+geom_point()
# print(children_vs_charges)


#variable smoker
qqnorm(data$smoker ,main = "Smoker")
qqline(data$smoker)


# is the data signficantlly important?
# H0: Smoker does not have any significant effect on Charges
# smoker_and_charges_test <- wilcox.test(data$smoker, charges ,paired=TRUE)
# print(smoker_and_charges_test)
# reject H0 p <0.05 data has significant important on charges
smoker_vs_charges<-qplot(data$smoker, charges, data=data, xlab="smoker", ylab="charges", main="smoker vs  charges")+stat_smooth(method = lm)+geom_point()
print(smoker_vs_charges)


#variable region
qqnorm(data$region, main="Region")
qqline(data$region)

#is the data signfically  important?
#H0: Region does not have any significant effect on charges
# region_and_charges_test <- wilcox.test(data$region, charges, paired = FALSE)  
# print(region_and_charges_test)
#reject H0  p <0.05 there is a significant important effect  of region on charges
region_vs_charges<-qplot(data$region, charges, data=data, xlab="region", ylab="charges", main="region vs  charges")+stat_smooth(method = lm)+geom_point()
print(region_vs_charges)


#question 3
#age regression model


fit<-lm(charges~.,data=data)
summary_fit<-summary(fit)
print(summary_fit)
vif_fit<-vif(fit)
#The VIF value indicates how much multicollinearity each predictor has in our final model. 
print(vif_fit)
#removing age_categories and bmi catergories
new_data <- data[, !(names(data) %in% c("age_category", "bmi_category"))]
fit2<-lm(charges ~.,data=new_data)
summary_fit2<-summary(fit2)
vif_fit2<-vif(fit2)
print(vif_fit2)
#The VIF value indicates how much multicollinearity each predictor has in our final model. 
print(summary_fit2)
#p-value of sex >0.05 => no significant difference between male/female groups
#Charges = -13361.12 + 257.29(age)  + 332.57(bmi) + 479.37(children) + 23820.43(smoker) + 353.64(region)
#R_squared is 0.7507 this model account for 75.07% variance in the dataset

# print(head(new_data))
# age_model<-lm(age~charges)
# summary_age_model <- summary(age_model)
# plot(age,charges)
# par(mfrow = c(2, 2))
# print(summary_age_model)
# #test for linear regresion model

# # Test for Autocorrelated/non-independence of Errors
# #Ho: There is no auto-correlation bw errors (errors r independent)
# dwtest_age<-dwtest(age_model)
# print(dwtest_age)
# #accept  H0 if p>0.05 error are independent

# #H0: hypothesis of constant error variance, i.e.  NO heteroscedasticity
# #variance around the regression line is the same for all values of the predictor variable (X)
# bp_age<-bptest(age_model)
# print(bp_age)
# #Reject H0 p<0.05 is heteroscedasticity

# #linear regression model failed as there is heteroscedasticity
# #transformation model
# age_log<-log(age)
# age_sq<-sqrt(age)
# charges_log<-log(charges)
# charges_sq<-sqrt(charges)
# hist(age_log)
# hist(charges_log)
# hist(age_sq)
# hist(charges_sq)
# # H0 data are normally distributed
# charges_log_normality_test <- ks.test(charges_log, "pnorm",mean = mean(charges_log),sd=sd(charges_log))
# print(charges_log_normality_test)
# # accept H0 p>0.05 data are normally distributed
# # H0 data are normally distributed
# age_log_normality_test <- ks.test(age_log, "pnorm",mean = mean(age_log),sd=sd(age_log))
# print(age_log_normality_test)
# plot(age ,charges)
# # Reject H0 p < 0.05 data not normally distributed

# #Non linear regression
# polynomial_age_model<-gls(charges~poly(age,3),method="ML")
# summary_polynomial_age_model<-summary(polynomial_age_model)
# print(summary_polynomial_age_model)
# R_squared <- 1 - (polynomial_age_model$sigma^2 / var(charges))
# print(R_squared)

# age_rlm_test <-rlm(charges ~ age, data = data)
# summary(age_rlm_test)

# par(mfrow=c(2, 2)) 
# plot(age_rlm_test)
# print(bptest(age_rlm_test))
# print(summary(age_rlm_test))
# R_squared_age <- cor(charges, predict(age_rlm_test))^2
# print(R_squared_age)
# # charges=−1731.6322+263.2407×age


# #bmi model
# bmi_model<-lm(bmi~charges_log)
# summary_bmi_model<-summary(bmi_model)
# print(summary_bmi_model)
# par(mfrow = c(2, 2))
# plot(bmi_model)


# # Test for Autocorrelated/non-independence of Errors
# #Ho: There is no auto-correlation bw errors (errors r independent)
# dwtest_bmi<-dwtest(bmi_model)
# print(dwtest_bmi)
# #accept  H0 if p>0.05 error are independent

# #H0: hypothesis of constant error variance, i.e.  NO heteroscedasticity
# #variance around the regression line is the same for all values of the predictor variable (X)
# bp_bmi<-bptest(bmi_model)
# print(bp_bmi)
# #Reject H0 p<0.05 there is Homoscedasticity 

# bmi_log<-log(bmi)
# hist(charges_log)
# hist(bmi_log)

# bmi_model<-rlm(charges_log~bmi )
# summary_bmi_model<-summary(bmi_model)
# print(summary_bmi_model)
# par(mfrow = c(2, 2))
# plot(bmi_model)
# r_square_bmi_model <- cor(charges_log, predict(bmi_model))^2
# print(r_square_bmi_model)

# #sex
# sex_model<-lm(data$sex~ charges)
# summary_sex_model <- summary(sex_model)

# print(summary_sex_model)


#question 4

# Assuming the charges are divided into high and low categories based on a threshold value
data$CHARGE_split <- ifelse(charges > median(charges), "High", "Low")
count_split<-table(data$CHARGE_split)



plot_density_age<-ggplot(data = data, aes(x = age, fill = CHARGE_split)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Density plot of age by CHARGE-split")
print(plot_density_age)
plot_density_sex<-ggplot(data = data, aes(x = sex, fill = CHARGE_split)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Density plot of sex by CHARGE-split")
print(plot_density_sex)
plot_density_bmi<-ggplot(data = data, aes(x = bmi, fill = CHARGE_split)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Density plot of bmi by CHARGE-split")
print(plot_density_bmi)

plot_density_children<-ggplot(data = data, aes(x = children, fill = CHARGE_split)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Density plot of children by CHARGE-split")
print(plot_density_children)

plot_density_smoker<-ggplot(data = data, aes(x = smoker, fill = CHARGE_split)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Density plot of smoker by CHARGE-split")
print(plot_density_smoker)

plot_density_region<-ggplot(data = data, aes(x = region, fill = CHARGE_split)) + 
  geom_density(alpha = 0.5) + 
  labs(title = "Density plot of region by CHARGE-split")
print(plot_density_region)
print(summary(data))
boxplot(age ~ CHARGE_split, data = data, main = "Boxplot of Age by CHARGE-split")

box_bmi<-ggplot(data = data, aes(x = CHARGE_split, y = bmi, fill = CHARGE_split)) +
  geom_boxplot() +
  labs(title = "Boxplot of bmi by CHARGE-split") +
  scale_fill_manual(values = c("High" = "lightpink", "Low" = "skyblue")) # Customize colors as needed
print(box_bmi)
box_age<-ggplot(data = data, aes(x = CHARGE_split, y = age, fill = CHARGE_split)) +
  geom_boxplot() +
  labs(title = "Boxplot of age by CHARGE-split") +
  scale_fill_manual(values = c("High" = "lightpink", "Low" = "skyblue")) # Customize colors as needed
print(box_age)

#statistical test
data$CHARGE_split <- ifelse(data$CHARGE_split =="High", 1,0)
print(head(data))

#Null Hypothesis (H0): There is no difference in the central tendency of the predictor variables between the High and Low charge groups.
k_test_age<-kruskal.test(age ~ CHARGE_split, data = data)
summary_k_test_age<- summary(k_test_age)
print(k_test_age)
#reject

k_test_sex<-kruskal.test(sex ~ CHARGE_split, data = data)
summary_k_test_sex<- summary(k_test_sex)
print(k_test_sex)
#accept

k_test_bmi<-kruskal.test(bmi ~ CHARGE_split, data = data)
summary_k_test_bmi<- summary(k_test_bmi)
print(k_test_bmi)
#reject
k_test_children<-kruskal.test(children ~ CHARGE_split, data = data)
summary_k_test_children<- summary(k_test_children)
print(k_test_children)
#accept

k_test_smoker<-kruskal.test(smoker ~ CHARGE_split, data = data)
summary_k_test_smoker<- summary(k_test_smoker)
print(k_test_smoker)
#reject

k_test_region<-kruskal.test(region ~ CHARGE_split, data = data)
summary_k_test_region<- summary(k_test_region)
print(k_test_region)
#accept

#question 5

kruskal_age <- kruskal.test(age ~ region, data = data)
kruskal_bmi <- kruskal.test(bmi ~ region, data = data)

print(kruskal_age)
print(kruskal_bmi)


# Perform Dunn post-hoc tests for pairwise comparisons
dunn_age <- dunn.test(age, region, method = "bonferroni")
dunn_bmi <- dunn.test(bmi, region, method = "bonferroni")

print(dunn_age)
print(dunn_bmi)

sink()