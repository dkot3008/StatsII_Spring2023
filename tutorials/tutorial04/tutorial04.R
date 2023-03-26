#######################
# Stats 2: tutorial 4 #
#######################

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

## More on logits: visualising and goodness of fit

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)

# 1. This time, let's analyse the data in more detail. Run some checks to see if 
#    the data are well distributed. Try some simple plots to get an idea of the 
#    relationship between variables. Drop those errors too.

grad_inc <- graduation$hsgrad~ graduation$income

##bad type of chart better as box
ggplot(data = graduation ,aes(graduation$hsgrad, graduation$income))+
  geom_line()

ggplot(data = graduation ,aes(graduation$hsgrad, graduation$income))+
  geom_boxplot()

ggplot(data = graduation ,aes(graduation$hsgrad, graduation$income))+
  geom_()


# 2. Last week we created a kitchen sink model, with nsibs as a binned factor. 
#    Here was the code:
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

mod_1 <- glm(hsgrad ~., 
             data = graduation[,!names(graduation) %in% c("nsibs")], 
             family = "binomial")

summary(mod_1)
# Create a more parsimonious model of your own choice. Select three predictor 
# variables, run the regression, and check with summary.

mod_2 <-  glm(hsgrad ~ intact + nsibs_cut + asvab, 
                      data = graduation[,!names(graduation) %in% c("nsibs")], 
                      family = "binomial")

summary(mod_2)


mod_3 <- glm(hsgrad ~ intact * nsibs_cut * asvab, 
             data = graduation[,!names(graduation) %in% c("nsibs")], 
             family = "binomial")
summary(mod_3)

anova(mod_3,mod_2, test = 'LRT')
# 3. a) Create a new data frame comprising the outcome variable and two columns 
#       of fitted values, one from mod_1 and another from mod_2. 
predicted_data <- data.frame(
  hsgrad = graduation$hsgrad,
  mod_1_hat = mod_1$fitted.values,
  mod_2_hat = mod_2$fitted.values
)

help("filter")
# 3. b) Create a pipe (without reassigning) whereby you reorder your new 
#       dataframe according to the fitted values of mod_1, create a new rank 
#       variable, then create a scatterplot of rank by fitted value, 
#       colored by the outcome variable.


new_mod1 <- as.vector(predicted_data$mod_1_hat)
pred_data <- cbind(new_mod1,predicted_data)

pred_data %>%
  arrange(new_mod1)
  
#wrong needs to be a vector
#new_mod1 <- as.logical(predicted_data$mod_1_hat)

##graph it
pred_data %>%
  arrange(new_mod1)
  mutate(rank = row_number())%>%
  ggplot(aes(rank,new_mod1))+
  geom_point()+
  #geom_line()+
  scale_y_continuous()

##attempt
pred_data %>%
  mutate(rank = row_number())%>%
  ggplot(aes(rank,mod_1_hat)+
  geom_point(aes(colour = hsgrad),alpha =0.5)+
  #geom_line()+
  scale_y_continuous()

###martyyn-ish
predicted_data %>%
  arrange(mod_1_hat)%>%
  mutate(rank = row_number())+
  ggplot(aes(rank,mod_1_hat))+
  geom_point(aes(colour = hsgrad),alpha =0.5)+
  scale_y_continuous(limits = c(0,1))

predicted_data %>%
  arrange(mod_1_hat)%>%
  mutate(rank = row_number())+
  ggplot(aes(rank,mod_1_hat))+
  geom_point(aes(colour = hsgrad),alpha=0.5)+
  scale_y_continuous(limits = c(0,1))

#reference point
filter(genre == "Horror") %>%
  mutate(month = month.abb[thtr_rel_month]) %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  arrange(desc(n))
# 3. c) Do the same for mod_2. Compare the results.

# 4. Calculate McFadden's Pseudo R squared for both models. 
#    Which model explains more variance?
#    What are the p values?

