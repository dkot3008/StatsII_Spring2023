# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))
data <- climateSupport

# changing from integer to logical
data$choice <- as.logical(as.numeric(as.factor(data$choice))-1) 

# additive model
model_add <- glm(choice ~ ., data = data, family = 'binomial')
summary(model_add)

# same as rough work
model_additive1 <- glm(choice ~ 1, data = data, family = 'binomial')
data$choice <- ifelse(data$choice == 1, TRUE, FALSE)
summary(model_additive1)

# Null model
nullMod <- glm(choice ~ 1, data = data, family = "binomial")

# Null vs. not-null model
anova(nullMod, model_add, test = "Chisq")
anova(nullMod, model_add, test = "LRT")

# exponential
exp(confint(model_add))

# extra: create a plot of coefficients with confidence intervals
confMod <- data.frame(lower = exp(confint(model_add)[,1]), 
                      coefs = exp(coef(model_add)), 
                      upper = exp(confint(model_add)[,2]))
library(ggplot2)
ggplot(data = confMod, mapping = aes(x = row.names(confMod), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip() +
  labs(x = "Terms", y = "Coefficients")


##question 2
topc <- filter(data,countries == '160 of 192')
topc5 <- filter(topc,sanctions == '5%')
#topc15 <- filter(topc,sanctions == '15%')


#lm(topc$choice~topc$sanctions)
glm(choices~sanctions,family = binomial(link = 'logit'),data = topc)

topc5$sanctions


# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))
data <- climateSupport
typeof(data$choice) 
##changing from integer to logical
data$choice<- as.logical(as.numeric(as.factor(data$choice))-1) 
#additive model
model_add <- glm(choice ~ ., data = data, family = 'binomial')
summary(model_additive)
##same as rough work
model_additive1 <- glm(choice1 ~ 1., data = data, family = 'binomial')
data$choice1 <- as.logical(data$choice == 'Supported','Not supported',1,0)
summary(model_additive1)
##Null model
nullMod <- glm(choice ~ 1, # 1 = fit an intercept only (i.e. sort of a "mean") 
               data = data, 
               family = "binomial")
##Null v not
anova(nullMod, model_add, test = "Chisq")
anova(nullMod, model_add, test = "LRT")
##exponential
exp(confint(model_add))
exp(confint(model_add)))

##some extra
confMod <- data.frame(cbind(lower = exp(confint(model_add)[,1]), 
                            coefs = exp(coef(model_add)), 
                            upper = exp(confint(model_add)[,2])))

# Then use this to make a plot
ggplot(data = confMod, mapping = aes(x = row.names(confMod), y = coefs)) +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), colour = "red") + 
  coord_flip() +
  labs(x = "Terms", y = "Coefficients")


log_reg_int_model <- glm(choice ~ countries * sanctions  , data = climateSupport, family = "binomial")
anova(model_add, log_reg_int_model, test = "Chi")



# Subset the data for policy with 160 countries
policy_160 <- filter(climateSupport, countries == "160 of 192")

# Fit logistic regression model
model <- glm(choice ~ sanctions, data = policy_160, family = binomial)

# Estimate the change in odds when sanctions increase from 5% to 15%
odds_ratio <- exp(coef(model)[2] * (15 - 5) / 100)

# Print the result
cat("Increasing sanctions from 5% to 15% increases the odds of supporting the policy by a factor of", round(odds_ratio, 2), "\n")

summary(model)

#####Q2B
# Subset the data for policy with 80 countries
policy_80 <- filter(climateSupport, countries == "80 of 192")

# Fit logistic regression model
model80 <- glm(choice ~ sanctions, data = policy_80, family = binomial)

summary(model80)



log_reg_int_model <- glm(choice ~ countries + sanctions + countries:sanctions , data = climateSupport, family = "binomial")
anova(model_add, log_reg_int_model, test = "Chi")


The output of the ANODEV test shows that the p-value is 0.3912, which is greater than the usual significance level of 0.05. This suggests that including the interaction term does not significantly improve the model fit, and the additive model is adequate. Therefore, we can conclude that the relationship between the response variable and the explanatory variables is well-captured by the additive model that includes only the main effects.

summary(model_add$R)
model_add$R
model_add$qr
model_add$linear.predictors


##summary(model_add)
The coefficients of the model represent the change in the log odds of supporting the policy associated with a one-unit change in the independent variable. For example, a one-unit increase in "countries" is associated with a 0.458452 increase in the log odds of supporting the policy, holding all other variables constant.
##anova
The output of the ANODEV test shows that the p-value is much smaller than 0.05, which indicates that the addition of the explanatory variables significantly improves the model fit. Therefore, we can reject the null hypothesis that the intercept-only model is adequate, and we can conclude that the model including the number of participating countries and sanctions is a better fit for the data.
