
library(tidyverse)
library(lubridate)
library(reshape2)
library(fitdistrplus)
Ins_data = read_csv('US_Health_Insurance.csv')
glimpse(Ins_data) 
install.packages("data.table")
summary(Ins_data)

Ins_data <- Ins_data %>%
  mutate(sex=as.factor(sex), region=as.factor(region),
         smoker=as.factor(smoker))


#Question1

#Graph1

BMI_Underweight <- subset(Ins_data,Ins_data$bmi<18.5 )
freq_Underweight<-nrow(BMI_Underweight)
freq_Underweight

BMI_Normal <- subset(Ins_data,(Ins_data$bmi>=18.5 & Ins_data$bmi<25))
freq_Normal <- nrow(BMI_Normal)
freq_Normal

BMI_Overweight <- subset(Ins_data,(Ins_data$bmi>=25 & Ins_data$bmi<30))
freq_Overweight <- nrow(BMI_Overweight)
freq_Overweight



BMI_Obese <- subset(Ins_data,(Ins_data$bmi>=30))
freq_Obese <- nrow(BMI_Obese)
freq_Obese

Under_Smoker <- subset(BMI_Underweight,BMI_Underweight$smoker=='yes')
Under_Smoker<-nrow(Under_Smoker)
Under_Smoker

Normal_Smoker <- subset(BMI_Normal,BMI_Normal$smoker=='yes')
freq_Normal_Smoker<-nrow(Normal_Smoker)
freq_Normal_Smoker

Overweight_Smoker <- subset(BMI_Overweight,BMI_Overweight$smoker=='yes')
freq_Overweight_Smoker <- nrow(Overweight_Smoker)
freq_Overweight_Smoker

dat <- data.frame(category= factor(c("Normal","Overweight","Underweight"), levels=c("Normal","Overweight","Underweight")),
                  count = c(freq_Normal_Smoker, freq_Overweight_Smoker,Under_Smoker))

p <- ggplot(data=dat, aes(x=category, y=count)) +
  geom_bar(stat="identity")

p


#Graph 2
ggplot(data=Ins_data)+
  geom_histogram(mapping = aes(x = age),binwidth = 2,
                 fill = "lightblue",col=I("black"))

#Graph 3

ggplot(data=Ins_data)+
  geom_histogram(mapping = aes(x = bmi), boundary = 0,binwidth = 2, color= "blue", fill = 'Orange')



#Graph4

ggplot(data = Ins_data)+
  geom_bar(mapping = aes(x=region,fill = smoker))



ggplot(data = Ins_data)+
  geom_bar(mapping = aes(x=smoker,fill = sex))




#Question2


data_age <- Ins_data %>% mutate(agegroup = case_when(
                                              age >= 10  & age <= 19 ~ '10-19',
                                             age >= 20  & age <= 29 ~ '20-29',
                                             age >= 30  & age <= 39 ~ '30-39',
                                             age >= 40  & age <= 49 ~ '40-49',
                                             age >= 50  & age <= 59 ~ '50-59',
                                             age >= 60 ~ '60+'))


data_age

age_freq = data_age %>% dplyr::select(agegroup) %>% group_by(agegroup) %>% summarise(Count = n()) %>%
  mutate(age_pmf = Count/sum(Count)) %>%
  mutate(age_cdf = cumsum(age_pmf))
age_freq



child_freq = Ins_data %>% dplyr::select(children) %>% group_by(children) %>% summarise(Count = n()) %>%
  mutate(child_pmf = Count/sum(Count)) %>%
  mutate(child_cdf = cumsum(child_pmf))
child_freq


joint_freqq = outer(age_freq$Count, child_freq$Count, FUN = "+")
rownames(joint_freqq) = age_freq$agegroup[1:6]
colnames(joint_freqq) = child_freq$children[1:6]

joint_freqq


joint_df1 = reshape2 :: melt(joint_freqq)
colnames(joint_df1) = c('agegroup','child','count')
head(joint_df1)



#Question3

cor(age_freq$Count, child_freq$Count)



joint_df = reshape2 :: melt(joint_freqq)
colnames(joint_df) = c('agegroup','child','count')
head(joint_df, 100)


ggplot(data = joint_df,aes(x=agegroup, y = child,fill = count))+
  geom_tile()+
  scale_fill_gradient(low = 'brown' , high = 'orange')



#Question4

glimpse(Ins_data)

req_col <- select(Ins_data, age, children,  region, charges)

req_row <- dplyr::filter(req_col, children == 0 &  region  == "southeast")

req_row

req_row1 <- dplyr::filter(req_col, children > 0 &  region  == "southeast")
req_row1

a=req_row$charges

b=req_row1$charges

z_test2 = function(a, b, var_a, var_b){
  n.a = length(a)
  n.b = length(b)
  z = (mean(a) - mean(b)) / (sqrt((var_a)/n.a + (var_b)/n.b))
  return(z)
}


z_test2(req_row$charges, req_row1$charges, var(req_row$charges), var(req_row1$charges))



ggplot(data = req_row)+
  geom_boxplot(mapping = aes( y= charges))
ggplot(data = req_row1)+
  geom_boxplot(mapping = aes( y= charges))

#fail to reject, +1.96 to 1.96 fail to reject


#Question5


req_col_5 <- select(Ins_data, age, region, charges,smoker)
req_col_5


req_row_5 <- dplyr::filter(req_col_5, smoker == 'yes' &  region  == "southeast") 

req_row_5


req_row_5_1 <- dplyr::filter(req_col_5, smoker == 'yes' &  region  == "northeast") 

req_row_5_1

a1 = count(req_row_5)
q2 =count(Ins_data)
n1 = count(req_row_5_1)


res <- prop.test(x = c(91, 67), n = c(1338, 1338))
res


res$p.value

#we cant reject

#Question6

Above50 <- Ins_data %>% dplyr::select(age,region,charges) %>%
  dplyr::filter(age > 50)

count(Above50)


below50 <- Ins_data %>% dplyr::select(age,region,charges) %>%
  dplyr::filter(age < 50)

count(below50)

var.test(Above50$age,below50$age,alternative = "two.sided",conf.level = 0.95)
glimpse(Ins_data)




#Questiion7

ggplot(data=Ins_data)+
  geom_histogram(mapping = aes(x = bmi), boundary = 0,binwidth = 2, color= "blue", fill = 'Orange')


#parameter estimates for negative binomial distribution

fit_nbinomial <- fitdist(Ins_data$children, 'nbinom')
summary(fit_nbinomial)

#parameter estimates for poission distribution

fit_poison <- fitdist(Ins_data$children, 'pois')
summary(fit_poison)

#parameter estimates for geom distribution

fit_geom <- fitdist(Ins_data$children, 'geom')
summary(fit_geom)


# Goodness-of-Fit tests
gofstat(list(fit_nbinomial,fit_poison,fit_geom))

#From this test, I conclude negative binomial distribution fit the data well.


#Descriptive Statisitics
descdist(Ins_data$bmi)


#normal distribution
fit_normal <- fitdist(Ins_data$bmi,"norm")
summary(fit_normal)

# lognormal distribution
fit_lognormal <- fitdist(Ins_data$bmi,"lnorm")
summary(fit_lognormal)

#Gamma Distribution
fit_gamma <- fitdist(Ins_data$bmi,"gamma")
summary(fit_gamma)


# Gamma distribution is better fit for BMI since AIC and BIC values are smaller 
#and Loglikelihood is higher

#Goodness-of-Fit plots for normal distribution
?par()
par(mfrow=c(2,2))
denscomp(list(fit_normal), legendtext = "normal", xlab = 'Body Mass index(BMI)')
cdfcomp (list(fit_normal), legendtext = "normal", xlab = 'Body Mass index(BMI)')
qqcomp (list(fit_normal), legendtext = "normal", xlab = 'Body Mass index(BMI)')
ppcomp (list(fit_normal), legendtext = "normal", xlab = 'Body Mass index(BMI)')

#Goodness-of-Fit plots for lognormal distribution
par(mfrow=c(2,2))
denscomp(list(fit_lognormal), legendtext = "lognormal", xlab = 'Body Mass index(BMI)')
cdfcomp (list(fit_lognormal), legendtext = "lognormal", xlab = 'Body Mass index(BMI)')
qqcomp (list(fit_lognormal), legendtext = "lognormal", xlab = 'Body Mass index(BMI)')
ppcomp (list(fit_lognormal), legendtext = "lognormal", xlab = 'Body Mass index(BMI)')

#Goodness-of-Fit plots for gamma distribution
par(mfrow=c(2,2))
denscomp(list(fit_gamma), legendtext = "Gamma", xlab = 'Body Mass index(BMI)')
cdfcomp (list(fit_gamma), legendtext = "Gamma", xlab = 'Body Mass index(BMI)')
qqcomp (list(fit_gamma), legendtext = "Gamma", xlab = 'Body Mass index(BMI)')
ppcomp (list(fit_gamma), legendtext = "Gamma", xlab = 'Body Mass index(BMI)')





#Question:8
# 8. One extra analysis based on your preference.



ggplot(data = Ins_data)+
  geom_point(aes(x = bmi, y = charges, color = smoker))+
  geom_hline(yintercept =mean(Ins_data$charges))+
  geom_vline(xintercept = mean(Ins_data$bmi))+
  annotate("text", label = 'mean_bmi', x = 33 , y = 62000)+
  annotate("text", label = 'mean_charges', x = 52 , y = 14550)+
  annotate("text", label = 'Highly charged people', x = 45 , y = 50050)

