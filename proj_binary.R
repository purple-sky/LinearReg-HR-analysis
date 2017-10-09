# if "Source" run will not work, please save the project folder into your Working Directory and run 2 lines below instead
# pref <- getwd()
# dat <- read.csv(file.path(pref,"project_team_BCLLM","HR_comma_sep2.csv"), header = TRUE, sep = ",")

# Source run
script.dir <- dirname(sys.frame(1)$ofile) # click on "Source" button in right upper corner ner "Run" while on this line
setwd(script.dir)
dat <- read.csv("HR_comma_sep2.csv",header = TRUE, sep = ",")
# comment up to here if source run doesn't work

names(dat)
print(summary(dat))
ntot<-nrow(dat)

# print tables to determine if we need to merge some groups
# we can possibly merge marketing and product management or marketing and sales
# print(table(dat$left,dat$dpt)); print(table(dat$left,dat$salary));
# plot(dat$salary~dat$dpt)

# new variable how many hrs people work during the day. Average 22 working days in a month
dat$dayHRS <- dat$monthHRS/22
# this is to plot the side-by-side boxplots and take a look at overlap
par(mfrow=c(3,2))
plot(factor(dat$left),dat$satisfaction_level, xlab = "left company", ylab="satisfaction lvl") # important
plot(factor(dat$left),dat$evaluation, xlab = "left company", ylab="evaluation") # probably important
plot(factor(dat$left),dat$dayHRS,xlab = "left company", ylab="hrs a day") # important
plot(factor(dat$left),dat$timeInComp,xlab = "left company", ylab="yrs in company") # important
plot(factor(dat$left),dat$projects,xlab = "left company", ylab="# projects") # probably important
# plot(factor(dat$left),dat$accidents, ylab="# accidents") # not important
# plot(factor(dat$left),dat$promotion, ylab="# promotions") # not important
par(mfrow=c(1,1))
# ask how to plot these guys
plot(factor(dat$left),dat$salary, xlab = "left company", ylab="salary")
plot(factor(dat$left)~dat$dpt, ylab = "left company", xlab="department")

# randomize the data
set.seed(12345)
# 10000 for training set, 4999 for test
iperm<-sample(ntot,ntot)
n<-10000
train<-dat[iperm[1:n],]
test<-dat[iperm[(n+1):ntot],] # holdout

# printing # of left people from training set
print(table(train$left))
#    0    1 
# 7594 2406

# translate to proportion
print(table(train$left)/n)
#      0      1 
# 0.7594 0.2406

print(mean(train$left))

names(train)
# correlations and side-by-side boxplots and tabular summaries
attach(train)
summcor<-cor(left,train[,c(1:3, 5, 11)])
print(summcor)
#      satisfaction_level evaluation   projects timeInComp     dayHRS
# [1,]          -0.383639 0.00253653 0.02323443   0.137666 0.07110964

# printing tables for categorical values
print(table(left,dpt)); print(table(left,salary)); print(table(left,accidents)); print(table(left,promotion));

#     dpt
# left accounting   hr   IT management marketing product_mng RandD sales support technical
#    0        369  340  644        382       429         486   432  2085    1080      1347
#    1        136  147  180         59       131         131    79   679     372       492
#     salary
# left high  low medium
#    0  763 3456   3375
#    1   56 1459    891
#     accidents
# left    0    1
#    0 6274 1320
#    1 2287  119
#     promotion
# left    0    1
#    0 7391  203
#    1 2393   13

# salaries across departments

print(table(dpt,salary))

#             high  low medium
# accounting    45  234    226
# hr            29  219    239
# IT            48  415    361
# management   159  127    155
# marketing     55  267    238
# product_mng   50  303    264
# RandD         37  229    245
# sales        171 1433   1160
# support       96  749    607
# technical    129  939    771

# plotting training data
par(mfrow=c(3,2))
plot(factor(left),satisfaction_level,xlab = "left company", ylab="satisfaction lvl") # important
plot(factor(left),evaluation,xlab = "left company", ylab="evaluation") # probably important
plot(factor(left),projects,xlab = "left company", ylab="# projects") # probably important
plot(factor(left),dayHRS,xlab = "left company", ylab="hrs a day") # important
plot(factor(left),timeInComp,xlab = "left company", ylab="yrs in company") # important
# two most important correlations time in company vs satisfaction lvl
timeInComp.jig<-timeInComp+runif(n,0.0,1)
ii=sample(n,5000); plot(satisfaction_level[ii],timeInComp.jig[ii], type = "n");
pchv<-c("+","*")
text(satisfaction_level[ii],timeInComp.jig[ii],label=pchv[left[ii]+1],col=left[ii]+1)
# there is a strange square with low satisfaction lvl, but working 4-6 years. 
# analyzing the data by departments:
par(mfrow=c(1,1))
my <- data.frame(dpt)

library(ggplot2)
# the data is consistent across all departments for time in company vs satisfaction level
# people with satisfaction lvl < 0.25 who left tended to work 4-5 years
# people with satisfaction lvl < 0.5 who left tended to work 3 years
# people with satisfaction lvl > 0.5 who left tended to work 5-6 years
g<- ggplot(my, aes(satisfaction_level, timeInComp, color=as.factor(left)))
g + geom_point() + facet_grid(dpt ~ .) + scale_color_manual(values=c("blue", "red"))

# the data is consistent across all departments for salary vs satisfaction level
# people who left tended to have satisfaction lvl < 0.5 and low/medium salary
# employees with high salary tend to stay and have higher satisfaction lvl
g1<- ggplot(my, aes(satisfaction_level, salary, color=as.factor(left)))
g1 + geom_point() + facet_grid(dpt ~ .) + scale_color_manual(values=c("blue", "red"))

# GLM

# including all the variables

full.glm<-glm(left~satisfaction_level+evaluation+projects+dayHRS+
                timeInComp+accidents+promotion+dpt+salary,family="binomial",data=train)
print(summary(full.glm))

# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.42245    0.23576  -6.033 1.60e-09 ***
# satisfaction_level -4.06523    0.11886 -34.202  < 2e-16 ***
# evaluation          0.62558    0.18021   3.471 0.000518 ***
# projects           -0.30364    0.02588 -11.731  < 2e-16 ***
# dayHRS              0.09937    0.01386   7.171 7.43e-13 ***
# timeInComp          0.26290    0.01901  13.830  < 2e-16 ***
# accidents          -1.50126    0.10715 -14.011  < 2e-16 ***
# promotion          -1.45042    0.30986  -4.681 2.86e-06 ***
# dpthr               0.19801    0.15978   1.239 0.215246    
# dptIT              -0.26806    0.14860  -1.804 0.071242 .  
# dptmanagement      -0.64249    0.19527  -3.290 0.001001 ** 
# dptmarketing       -0.11135    0.16194  -0.688 0.491700    
# dptproduct_mng     -0.22249    0.15802  -1.408 0.159123    
# dptRandD           -0.62883    0.17729  -3.547 0.000390 ***
# dptsales           -0.06781    0.12450  -0.545 0.585962    
# dptsupport          0.06606    0.13308   0.496 0.619616    
# dpttechnical        0.08549    0.12896   0.663 0.507367    
# salarylow           1.91086    0.15614  12.238  < 2e-16 ***
# salarymedium        1.42201    0.15706   9.054  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 11035.4  on 9999  degrees of freedom
# Residual deviance:  8667.3  on 9981  degrees of freedom
# AIC: 8705.3

null.glm<-glm(left~1,data=train,family="binomial")
print(null.glm)

# Coefficients:
#   (Intercept)  
# -1.149  
# 
# Degrees of Freedom: 9999 Total (i.e. Null);  9999 Residual
# Null Deviance:	    11040 
# Residual Deviance: 11040 	AIC: 11040

fit<-step(null.glm, scope=list(lower=null.glm, upper=full.glm),direction="both")
# steps are printed out but not reproduced here.
print(summary(fit))

# the best model is still one with all parameters included: AIC: 8705.29
# the second best is without evaluation: AIC: 8715.4

fit1 = full.glm
fit2 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + 
                    projects + dayHRS + dpt + promotion,data=train,family="binomial")
print(summary(fit2))

# second best model
# Coefficients:
#                    Estimate Std. Error z value Pr(>|z|)    
# (Intercept)        -1.25351    0.23053  -5.438 5.40e-08 ***
# satisfaction_level -3.97864    0.11547 -34.457  < 2e-16 ***
# accidents          -1.50042    0.10698 -14.026  < 2e-16 ***
# salarylow           1.90963    0.15583  12.255  < 2e-16 ***
# salarymedium        1.41757    0.15674   9.044  < 2e-16 ***
# timeInComp          0.26799    0.01890  14.179  < 2e-16 ***
# projects           -0.27545    0.02441 -11.284  < 2e-16 ***
# dayHRS              0.11085    0.01344   8.245  < 2e-16 ***
# dpthr               0.20658    0.15990   1.292  0.19636    
# dptIT              -0.26521    0.14860  -1.785  0.07431 .  
# dptmanagement      -0.63183    0.19512  -3.238  0.00120 ** 
# dptmarketing       -0.11205    0.16197  -0.692  0.48908    
# dptproduct_mng     -0.22339    0.15801  -1.414  0.15742    
# dptRandD           -0.62290    0.17721  -3.515  0.00044 ***
# dptsales           -0.06878    0.12454  -0.552  0.58076    
# dptsupport          0.07357    0.13309   0.553  0.58043    
# dpttechnical        0.08651    0.12899   0.671  0.50242    
# promotion          -1.47055    0.30973  -4.748 2.06e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 11035.4  on 9999  degrees of freedom
# Residual deviance:  8679.4  on 9982  degrees of freedom
# AIC: 8715.4

# Adding quadratic terms

fit3 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + projects + dayHRS + dpt + promotion+projects+evaluation+I(satisfaction_level^2)+evaluation:satisfaction_level+projects:satisfaction_level+timeInComp:satisfaction_level+dayHRS:satisfaction_level,  data=train,family="binomial")
print(summary(fit3))
# AIC: 7506.7
fit4 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + projects + dayHRS + dpt + promotion+projects+evaluation+I(evaluation^2)+ satisfaction_level: evaluation +projects: evaluation +timeInComp: evaluation +dayHRS: evaluation,  data=train,family="binomial")
print(summary(fit4))
# AIC: 4877.4
fit5 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + projects + dayHRS + dpt + promotion+projects+evaluation+I(projects ^2)+ satisfaction_level: projects + evaluation: projects +timeInComp: projects +dayHRS: projects,  data=train,family="binomial")
print(summary(fit5))
# AIC: 4873.6
fit6 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + projects + dayHRS + dpt + promotion+projects+evaluation+I(timeInComp ^2)+ satisfaction_level: timeInComp + evaluation: timeInComp + projects: timeInComp +dayHRS: timeInComp,  data=train,family="binomial")
print(summary(fit6))
# AIC: 4311.1
fit7 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + projects + dayHRS + dpt + promotion+projects+evaluation+I(dayHRS ^2)+ satisfaction_level: dayHRS + evaluation: dayHRS + projects: dayHRS +timeInComp:dayHRS,  data=train,family="binomial")
print(summary(fit7))
# AIC: 5115.7
plot(factor(left),timeInComp^2,xlab = "left company", ylab="yrs in company squared")

# Changing the baseline for departments
train$dpt<-relevel(factor(train$dpt), ref="hr")
fit8 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + projects + dayHRS + dpt + promotion+projects+evaluation, data=train,family="binomial")
print(summary(fit8))

# Merging departments into the broader categories
dpat <- rep("accounting", n)
hr <- (dpt == "hr")
it <- (dpt == "IT")
mngm <- (dpt == "management")
market <- (dpt == "marketing")
prod_m <- (dpt == "product_mng")
randd <- (dpt == "RandD")
mrg1 <- (train$dpt=="sales" | train$dpt=="marketing")
mrg2 <- (train$dpt=="technical" | train$dpt=="support")
dpat[hr] <- "HR"
dpat[it] <- "IT"
dpat[mngm] = "management"; dpat[prod_m] = "product_mng"; dpat[randd] = "RandD";
dpat[mrg1]<-"sales/marketing"
dpat[mrg2]<-"tech_support"
train$dpt<-dpat  # add to dataframe
print(table(train$dpt))

# apply best GLM w/o quadratic terms
fit9 <- glm(left~satisfaction_level+evaluation+projects+dayHRS+
           timeInComp+accidents+promotion+dpt+salary, data=train, family="binomial")
print(summary(fit9))

# apply best GLM with quadratic terms
fit10 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + projects + dayHRS + dpt + promotion+projects+evaluation+I(timeInComp ^2)+ satisfaction_level: timeInComp + evaluation: timeInComp + projects: timeInComp +dayHRS: timeInComp,  data=train,family="binomial")
print(summary(fit10))

# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.4799  -0.2328  -0.0391   0.0000   5.6936  
# 
# Coefficients:
#                                Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    30.25295    1.07788  28.067  < 2e-16 ***
# satisfaction_level            -27.38077    1.04410 -26.224  < 2e-16 ***
# accidents                      -1.22967    0.13684  -8.986  < 2e-16 ***
# salarylow                       1.61685    0.19048   8.488  < 2e-16 ***
# salarymedium                    1.24946    0.19249   6.491 8.52e-11 ***
# timeInComp                     -2.90689    0.34481  -8.430  < 2e-16 ***
# projects                       -3.99925    0.18921 -21.137  < 2e-16 ***
# dayHRS                         -1.21020    0.10444 -11.587  < 2e-16 ***
# dptHR                           0.21326    0.22905   0.931   0.3518    
# dptIT                          -0.07897    0.21324  -0.370   0.7111    
# dptmanagement                  -0.26734    0.27974  -0.956   0.3392    
# dptproduct_mng                 -0.48900    0.22018  -2.221   0.0264 *  
# dptRandD                       -0.57487    0.24377  -2.358   0.0184 *  
# dptsales/marketing              0.06829    0.17420   0.392   0.6950    
# dpttech_support                 0.20384    0.17424   1.170   0.2421    
# promotion                      -0.74470    0.39001  -1.909   0.0562 .  
# evaluation                    -21.90665    1.35088 -16.217  < 2e-16 ***
# I(timeInComp^2)                -1.40886    0.05460 -25.801  < 2e-16 ***
# satisfaction_level:timeInComp   5.82961    0.24740  23.564  < 2e-16 ***
# timeInComp:evaluation           6.00393    0.34613  17.346  < 2e-16 ***
# timeInComp:projects             0.98070    0.04858  20.186  < 2e-16 ***
# timeInComp:dayHRS               0.36503    0.02710  13.468  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 11035.4  on 9999  degrees of freedom
# Residual deviance:  4263.3  on 9978  degrees of freedom
# AIC: 4307.3

# apply best GLM with quadratic terms w/o department and promotion
# this part wasn't considered for the final report, only to check the AIC
fit11 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + projects + dayHRS +projects+evaluation+I(timeInComp ^2)+ satisfaction_level: timeInComp + evaluation: timeInComp + projects: timeInComp +dayHRS: timeInComp,  data=train,family="binomial")
print(summary(fit11))

detach(train)

# compare fit1, fit2 for in-sample misclassification rates
pred1<-predict(fit1,type="response")
pred2<-predict(fit2,type="response")

print(summary(pred1)); # both have same mean for left
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0009993 0.0751800 0.1713000 0.2406000 0.3623000 0.9154000

print(summary(pred2))
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0009234 0.0758700 0.1696000 0.2406000 0.3631000 0.9243000

#boundaries of 0.5 0.3 0.1 for misclassification
tab1a<-table(train$left,pred1>0.5)
tab1b<-table(train$left,pred1>0.3)
tab1c<-table(train$left,pred1>0.1)
tab2a<-table(train$left,pred2>0.5)
tab2b<-table(train$left,pred2>0.3)
tab2c<-table(train$left,pred2>0.1)
print(tab1a); print(tab1b); print(tab1c)
print(tab2a); print(tab2b); print(tab2c)

# tab1a                tab2a
#    FALSE TRUE           FALSE TRUE  threshold=0.5
# 0  7042  552         0  7039  555
# 1  1575  831         1  1545  861
# 
# tab1b                tab2b
#   FALSE TRUE           FALSE TRUE   threshold=0.3
# 0  6108 1486         0  6124 1470
# 1   754 1652         1   755 1651
# 
# tab1c                tab2c
#   FALSE TRUE           FALSE TRUE   threshold=0.1
# 0  3154 4440         0  3140 4454
# 1    91 2315         1    96 2310

# convert to rates
print(tab1a/apply(tab1a,1,sum));    print(tab2a/apply(tab2a,1,sum))

#        FALSE       TRUE
# 0 0.92731104 0.07268896
# 1 0.65461347 0.34538653
# 
#        FALSE       TRUE
# 0 0.92691599 0.07308401
# 1 0.64214464 0.35785536

print(tab1b/apply(tab1b,1,sum));    print(tab2b/apply(tab2b,1,sum))

#       FALSE      TRUE
# 0 0.8043192 0.1956808
# 1 0.3133832 0.6866168
# 
#       FALSE      TRUE
# 0 0.8064261 0.1935739
# 1 0.3137988 0.6862012

print(tab1c/apply(tab1c,1,sum));    print(tab2c/apply(tab2c,1,sum))
#        FALSE       TRUE
# 0 0.41532789 0.58467211
# 1 0.03782211 0.96217789
# 
#        FALSE       TRUE
# 0 0.41348433 0.58651567
# 1 0.03990025 0.96009975

# full model is slightly better than the model without 1 parameter based on the
# in-sample misclassification

# Compare out-of-samle misclassification
pred1.test <- predict(fit1,type="response",newdata=test)
pred2.test <- predict(fit2,type="response",newdata=test)
htab1a<-table(test$left,pred1.test>0.5)
htab1b<-table(test$left,pred1.test>0.3)
htab1c<-table(test$left,pred1.test>0.1)
htab2a<-table(test$left,pred2.test>0.5)
htab2b<-table(test$left,pred2.test>0.3)
htab2c<-table(test$left,pred2.test>0.1)

print(htab1a); print(htab1b); print(htab1c)
print(htab2a); print(htab2b); print(htab2c)

# check the boxplots
par(mfrow=c(1,2))
summarydata1 <- list(stats=matrix(c(0.0009993, 0.0751800, 0.1713000, 0.3623000, 0.9154000)), n = 1000)
bxp(summarydata1, xlab = "fit1")

summarydata2 <- list(stats=matrix(c(0.0009234, 0.0758700, 0.1696000, 0.3631000, 0.9243000)), n =1000)
bxp(summarydata2, xlab = "fit2")
par(mfrow=c(1,1))

# convert summaries to rates
print(htab1a/apply(htab1a,1,sum));    print(htab2a/apply(htab2a,1,sum))
#      FALSE     TRUE
# 0 0.933229 0.066771
# 1 0.632618 0.367382
# 
#        FALSE       TRUE
# 0 0.93140323 0.06859677
# 1 0.62231760 0.37768240
print(htab1b/apply(htab1b,1,sum));    print(htab2b/apply(htab2b,1,sum))
#       FALSE      TRUE
# 0 0.8166406 0.1833594
# 1 0.3124464 0.6875536
# 
#       FALSE      TRUE
# 0 0.8163798 0.1836202
# 1 0.3115880 0.6884120
print(htab1c/apply(htab1c,1,sum));    print(htab2c/apply(htab2c,1,sum))
#        FALSE       TRUE
# 0 0.42905582 0.57094418
# 1 0.03948498 0.96051502
# 
#        FALSE       TRUE
# 0 0.42749087 0.57250913
# 1 0.04120172 0.95879828
# Model 2 (without 1 parameter) appears to be better for out-of-sample missclassification

# Calibrationd check Ke Miao
prcateg1<-cut(pred1,breaks=c(0,.01,.02,.03,.04,.06,.08,.10,.13,.2,.5,1))
print(table(prcateg1))
#(0,0.01] (0.01,0.02] (0.02,0.03] (0.03,0.04] (0.04,0.06] (0.06,0.08]  (0.08,0.1]  (0.1,0.13] 
#190         363         354         338         725         677         598         789 
#(0.13,0.2]   (0.2,0.5]     (0.5,1] 
#1495        3088        1383 

HLsumm1<-tapply(train$left, prcateg1, mean)
print(HLsumm1)
#(0,0.01] (0.01,0.02] (0.02,0.03] (0.03,0.04] (0.04,0.06] (0.06,0.08]  (0.08,0.1]  (0.1,0.13] 
#0.005263158 0.011019284 0.016949153 0.023668639 0.044137931 0.023633678 0.040133779 0.108998733 
#(0.13,0.2]   (0.2,0.5]     (0.5,1] 
#0.193311037 0.359132124 0.600867679

prcateg2<-cut(pred2,breaks=c(0,.01,.02,.03,.04,.06,.08,.10,.13,.2,.5,1))
print(table(prcateg2))
#(0,0.01] (0.01,0.02] (0.02,0.03] (0.03,0.04] (0.04,0.06] (0.06,0.08]  (0.08,0.1]  (0.1,0.13] 
#177         349         364         338         712         684         612         815 
#(0.13,0.2]   (0.2,0.5]     (0.5,1] 
#1499        3034        1416 

HLsumm2<-tapply(train$left,prcateg2,mean)
print(HLsumm2)
#(0,0.01] (0.01,0.02] (0.02,0.03] (0.03,0.04] (0.04,0.06] (0.06,0.08]  (0.08,0.1]  (0.1,0.13] 
#0.005649718 0.008595989 0.021978022 0.026627219 0.040730337 0.024853801 0.047385621 0.111656442 
#(0.13,0.2]   (0.2,0.5]     (0.5,1] 
#0.206137425 0.345748187 0.608050847

#Number of correctly calibrated intervals for model1: 7. Number of correctly calibrated intervals for model2: 6
#Therefore model1 seems to be better calibrated. 

#try to see if quadratic terms will make any improvement on model1:
fit2 <- glm(left ~ satisfaction_level + accidents + salary + timeInComp + 
              projects + dayHRS + dpt + promotion+monthHRS+projects+evaluation,
            data=train,family="binomial")

install.packages("ResourceSelection")
install.packages("Metrics")

#alternative way of doing Hosmer-Lemeshow(learned from online):
library(ResourceSelection)
hl1 = hoslem.test(train$left, fitted(fit1), g=10) 
cbind(hl1$expected, hl1$observed)
library("Metrics")
thermse<-rmse(train$left, pred1)
theresidsd<-sqrt(deviance(fit1)/df.residual(fit1))
