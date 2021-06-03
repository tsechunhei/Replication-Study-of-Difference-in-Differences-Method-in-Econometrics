#Replication study of 'Minimum Wages and Employment: A Case Study of the 
#Fast-Food Industry in New Jersey and Pennsylvania,' American Economic Review (1994,, vol. 84: 772-793)



#Preliminaries
library(tidyverse)
library(haven)
library(estimatr)
minwage = read.csv('minwage.csv')
minwage = minwage %>%
  filter(sample == 1) %>%
  rename(treat = state)%>%
  mutate(state = case_when(
    treat == 0 ~ 'PA',
    treat == 1 ~ 'NJ'
  ),
  low_wage = 1*(wage_st<5)
  )
summary(minwage)


###Baseline difference in differences estimate: starting wages
DinD_wage = minwage %>%
  group_by(state) %>%
  summarize(mean_wage_st = mean(wage_st),
            mean_wage_st2 = mean(wage_st2))%>%
  mutate(diff = mean_wage_st2 - mean_wage_st)
DinD_wage


##PA¡¦s within-state time-difference = -0.034848
#NJ¡¦s within-state time-difference = 0.469158
#DiD = 0.469 ¡V(-0.0348) = 0.504
#It assumes parallel trend between NJ and PA. 
#That means every factor that affects the wage have a parallel trend besides the minimum wage law.


#Baseline DID Estimate: full time equivalent employment.
DinD_employment = minwage %>%
  group_by(state) %>%
  summarize(mean_fte = mean(fte),
            mean_fte2 = mean(fte2))%>%
  mutate(diff = mean_fte2 - mean_fte)
DinD_employment

#PA¡¦s within-state time-difference = -2.01515
#NJ¡¦s within-state time-difference = 0.286842
#Difference in differences = 0.287-(-2.02) = 2.89
#It assumes parallel trend between NJ and PA. 
#That means every factor that affects the wage have a parallel trend besides the minimum wage law.


#Reshape minwage for DID regression estimation:
wave1 = minwage %>%
  select(state, treat, wage_st, fte, chain, co_owned, low_wage) %>%
  mutate(post = 0)
wave2 = minwage %>%
  select(state, treat, wage_st2, fte2, chain, co_owned, low_wage) %>%
  mutate(post = 1) %>%
  rename(wage_st = wage_st2, fte = fte2)
both_waves = bind_rows(wave1,wave2)


#DID Regression Estimates: 
#Consider the following regression model using the variables treat and post constructed above:
library(stargazer)
reg_wage1 = lm(wage_st ~ treat + post + treat:post, both_waves)
reg_emp1 = lm(fte ~ treat +post + treat:post, both_waves)
stargazer(reg_wage1, reg_emp1, type = 'text', digits =2,
          dep.var.labels =c('Staring wage','Full-time Equiv. Employment')
)
#DiD for wage = 0.5.
#DiD for employment is 2.3.
#The treatment is not statistically significant and not close to the number in the difference in differences table above. 
#There is no robust evidence that the employment would go down (the classic demand and supply model) or up (monopsonist model).







reg_wage2 = lm(wage_st ~ treat + post + treat:post + co_owned + as.factor(chain), both_waves)
reg_emp2 = lm(fte ~ treat + post + treat:post + co_owned + as.factor(chain), both_waves)
stargazer(reg_wage2, reg_emp2, type = 'text', digits =2,
          dep.var.labels = c('Starting Wage', 'Full-time Equiv. Employment')
)
#The conclusion for wage is the same as the sample variance and coefficient is the same. 
#For employment, the DID coefficient is still the same. But the sample variance goes down. 
#There is no effect on employment.





#Probing the DID Assumption:
nj_only = both_waves %>%
  filter(state == 'NJ')
pa_only = both_waves %>%
  filter(state == 'PA')
nj_wage1 = lm(wage_st ~ low_wage + post + low_wage:post, nj_only)
nj_emp1 = lm(fte ~ low_wage + post + low_wage:post, nj_only)
stargazer(nj_wage1, nj_emp1, type = 'text', digits = 2,
          dep.var.labels = c('Staring Wage', 'Full-time Equiv. Employment')
)


#Control:
nj_wage2 = lm(wage_st ~ low_wage + post + low_wage:post + co_owned + as.factor(chain), nj_only)
nj_emp2 = lm(fte ~ low_wage + post + low_wage:post + co_owned + as.factor(chain), nj_only)
stargazer(nj_wage2, nj_emp2, type = 'text', digits = 2,
          dep.var.labels = c('Staring Wage', 'Full-time Equiv. Employment')
)
#Parallel trend occurs in low-wage and high-wage restaruants in NJ. 
#After the minimum wage law is passed, workers would go to restaurants with better working condition but offering same wage.
pa_wage1 = lm(wage_st ~ low_wage + post + low_wage:post, pa_only)
pa_emp1 = lm(fte~ low_wage + post + low_wage:post, pa_only)
stargazer(pa_wage1, pa_emp1, type = 'text', digits = 2,dep.var.labels = c('Starring Wage', 'Full-time Equiv. Employment')
)

#Control:
pa_wage2 = lm(wage_st ~ low_wage + post + low_wage:post + co_owned + as.factor(chain), pa_only)
pa_emp2 = lm(fte ~ low_wage + post + low_wage:post + co_owned + as.factor(chain), pa_only)
stargazer(pa_wage2, pa_emp2, type = 'text', digits = 2,
          dep.var.labels = c('Staring Wage', 'Full-time Equiv. Employment')
)
#DID assumption may not hold in this case. 
#After the minimum wage law, restaurant with low-wage in PA raises their wages. 
#Low wage restaurants are affected by the minimum wage law in their decision as the overall wage in PA decreases. 









#Case2: Minimum Legal Drinking Age (MLDA) revisit
############################################################################################################################
##Estimate the effect of legal on mrate including state andyear effects:
library(tidyverse)
library(haven)
library(estimatr)
mlda = read.csv('deaths.csv')
mlda = mlda %>%
  filter(year<=1983, agegr== '18-20 yrs' , dtype=='all')%>%
  mutate(year_factor=factor(year), state=factor(state))
summary(mlda)

reg1 = lm_robust(mrate~legal+state+year_factor-1, data=mlda, clusters=state, se_type='stata')
reg2 = lm_robust(mrate~legal+state+year_factor+state:year-1, data=mlda, clusters=state, se_type='stata')



#Setting weights = pop:
reg3<-lm_robust(mrate~legal+state+year-1, data=mlda,
                weights=pop, clusters=state, se_type='stata')
estimates = c(coef(reg1)[1], coef(reg2)[1], coef(reg3)[1])
std_errors = c(reg1$std.error[1], reg2$std.error[1], reg3$std.error[1])
results = cbind(estimates, std_errors)
row.names(results)<-paste0('reg',1:3)
results

#Allow for state-specific e??ects by including an interaction between state and year:
reg4 = lm_robust(mrate~legal+state+year+state:year-1, data=mlda, weights =pop,
                 clusters=state, se_type='stata')
estimates2 = c(coef(reg1)[1], coef(reg2)[1], coef(reg3)[1], coef(reg4)[1])
std_errors2 = c(reg1$std.error[1], reg2$std.error[1], reg3$std.error[1],reg4$std.error[1])
results2 = cbind(estimates2, std_errors2)
row.names(results2)<-paste0('reg',1:4)
results2

#Reg1 and Reg3 both indicates that the legal effect is significant and robust. However, there is no interaction term as DiD does. 
#The causal effect is not sure.
#Reg4 includes an interaction between state and factor(year).
#Reg2 includes the interaction between state and year. It is hard to interpret the result.



#Control for beer taxes:
reg5 = lm_robust(mrate~legal+state+year_factor+beertaxa-1, data=mlda, clusters=state, se_type='stata')
reg6 = lm_robust(mrate~legal+state+year_factor+state:year+beertaxa-1, data=mlda, clusters=state, se_type='stata')
reg7<-lm_robust(mrate~legal+state+year-1+beertaxa, data=mlda,
                weights=pop, clusters=state, se_type='stata')
estimates3 = c(coef(reg5)[1], coef(reg6)[1], coef(reg7)[1])
std_errors3 = c(reg5$std.error[1], reg6$std.error[1], reg7$std.error[1])
results3 = cbind(estimates3, std_errors3)
row.names(results3)<-paste0('reg',5:7)
results3
#The beer tax is significant. 
#The estimates are higher and the standard errors are smaller, the t values are higher, although it is not DiD.
