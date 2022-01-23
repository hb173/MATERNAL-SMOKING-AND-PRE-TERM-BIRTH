
library(arm)
library(pROC)
library(e1071)
library(caret)
library(ggplot2)
require(gridExtra)


smoking <-smoking %>%
  mutate(
    prematury = case_when(
    gestation < 270 ~ '1',
    gestation >= 270 ~ '0',
    TRUE ~ "unknown"
   )
 )

smoking <-smoking %>%
  mutate(
    morace = case_when(
    mrace %in% c(0,1,2,3,4,5) ~ 'white',
    mrace == 6 ~ 'mexican',
    mrace == 7 ~ 'black',
    mrace == 8 ~ 'asian',
    mrace == 9 ~ 'mix',
    TRUE ~ 'other'
    )
  )
smoking <-smoking %>%
  mutate(
    meduc = case_when(
      med == 0 ~ 'less than 8th grade',
      med == 1 ~  '8th to 12th',
      med == 2 ~ 'high school but no other schooling',
      med == 3 ~ 'high school + trade school',
      med == 4 ~ 'high school + college',
      med == 5 ~ 'college',
      med %in% c(6,7) ~ 'trade school',
      TRUE ~ 'unknown'
    )
  )
  
#factor=boxplot, continous= scatter
#factoring categorical variables
smoking$morace1<-factor(smoking$morace)
smoking$meduc1<-factor(smoking$meduc)
smoking$smoke1<-factor(smoking$smoke)
smoking$prematury1<-factor(smoking$prematury)
smoking$inc1<-factor(smoking$inc)
smoking
str(smoking)

#convering into integers

smoking$prematury2<-as.numeric(smoking$prematury1)
smoking$prematury2 <- smoking$prematury2-1
str(smoking)


#Let's mean center the numerical predictors to avoid Mutli
smoking$parity <- smoking$parity - mean(smoking$parity)
smoking$mage <- smoking$mage - mean(smoking$mage)
smoking$mht<- smoking$mht - mean(smoking$mht)
smoking$mpregwt <- smoking$mpregwt - mean(smoking$mpregwt)

#EDA
ggplot(smoking,aes(x=prematury1, y=parity, fill=parity)) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues") +
  labs(title="Parity vs prematury",x="Parity",y="prematury")  + 
  theme_classic() + theme(legend.position="none")


ggplot(smoking,aes(x=prematury1, y=mage, fill=prematury)) +
  geom_boxplot()+
  scale_fill_brewer(palette="Blues") +
  labs(title="Mage vs prematury",x="mage",y="prematury")  + 
  theme_classic() + theme(legend.position="none")



ggplot(smoking,aes(x=prematury1, y=mht, fill=mht)) +
  geom_boxplot() + 
  scale_fill_brewer(palette="Blues") +
  labs(title="Mht vs prematury",x="Mht",y="prematury")  + 
  theme_classic() + theme(legend.position="none")


ggplot(smoking,aes(x=prematury1, y=mpregwt, fill=mpregwt)) + #not much difference
  geom_boxplot() + #coord_flip() +
  scale_fill_brewer(palette="Blues") +
  labs(title="Mpregwt vs prematury",x="Mpregwt",y="prematury")  + 
  theme_classic() + theme(legend.position="none")



## We can do tables for the factor variables
#less than 0.05 two variable dependent hay
table(smoking[,c("morace1","prematury1")])
table(smoking[,c("morace1","prematury1")])/sum(table(smoking[,c("morace1","prematury1")]))

apply(table(smoking[,c("morace1","prematury1")])/sum(table(smoking[,c("morace1","prematury1")])),
      2,function(x) x/sum(x)) 
# You can also use the tapply command for the same thing
tapply(smoking$morace1, smoking$prematury1, function(x) table(x)/sum(table(x)))
# Finally, we can even try a chi-squared test for independence.
chisq.test(table(smoking[,c("morace1","prematury1")])) 

table(smoking[,c("meduc1","prematury1")])
table(smoking[,c("meduc1","prematury1")])/sum(table(smoking[,c("meduc1","prematury1")]))

apply(table(smoking[,c("meduc1","prematury1")])/sum(table(smoking[,c("meduc1","prematury1")])),
      2,function(x) x/sum(x)) 
# You can also use the tapply command for the same thing
tapply(smoking$meduc1, smoking$prematury1, function(x) table(x)/sum(table(x)))
# Finally, we can even try a chi-squared test for independence.
chisq.test(table(smoking[,c("meduc1","prematury1")]))

table(smoking[,c("smoke1","prematury1")])
table(smoking[,c("smoke1","prematury1")])/sum(table(smoking[,c("smoke1","prematury1")]))

apply(table(smoking[,c("smoke1","prematury1")])/sum(table(smoking[,c("smoke1","prematury1")])),
      2,function(x) x/sum(x)) 
# You can also use the tapply command for the same thing
tapply(smoking$smoke1, smoking$prematury1, function(x) table(x)/sum(table(x)))
# Finally, we can even try a chi-squared test for independence.
chisq.test(table(smoking[,c("smoke1","prematury1")]))

table(smoking[,c("inc1","prematury1")])
table(smoking[,c("inc1","prematury1")])/sum(table(smoking[,c("inc1","prematury1")]))

apply(table(smoking[,c("inc1","prematury")])/sum(table(smoking[,c("inc1","prematury1")])),
      2,function(x) x/sum(x)) 
# You can also use the tapply command for the same thing
tapply(smoking$inc1, smoking$prematury1, function(x) table(x)/sum(table(x)))
# Finally, we can even try a chi-squared test for independence.
chisq.test(table(smoking[,c("inc1","prematury1")]))

table(smoking[,c("smoke1","morace1")])
table(smoking[,c("smoke1","morace1")])/sum(table(smoking[,c("smoke1","morace1")]))

apply(table(smoking[,c("smoke1","morace1")])/sum(table(smoking[,c("smoke1","morace1")])),
      2,function(x) x/sum(x)) 
# You can also use the tapply command for the same thing
tapply(smoking$smoke1, smoking$morace1, function(x) table(x)/sum(table(x)))
# Finally, we can even try a chi-squared test for independence.
chisq.test(table(smoking[,c("smoke1","morace1")]))

#binned plots for continious variable # looking if predictors are random or not
par(mfrow=c(1,1))
binnedplot(y=smoking$prematury2,smoking$mage,xlab="mage",ylim=c(0,1),col.pts="navy",
           ylab ="Premature",main="Binned Premature and Mage",
           col.int="white")

par(mfrow=c(1,1)) 
binnedplot(y=smoking$prematury2,smoking$parity,xlab="parity",ylim=c(0,1),col.pts="navy",
           ylab ="Premature",main="Binned Premature and Mage",
           col.int="white")
par(mfrow=c(1,1)) 
binnedplot(y=smoking$prematury2,smoking$mpregwt,xlab="pregwt",ylim=c(0,1),col.pts="navy",
           ylab ="Premature",main="Binned Premature and Mage",
           col.int="white")

str(smoking)
#model fit
smokyreg <- glm(prematury1 ~ parity + morace1 +	mage +	meduc1 + mpregwt + smoke1+income1+smoke1:morace1, data=smoking, family = binomial)
summary(smokyreg)
#aic
back<-step(smokyreg, direction = "both", trace=FALSE)
back$call
summary(smokyreg)
anova(smokyreg)
str(smoking)
#save the raw residuals

newregy<-glm(formula = prematury1 ~ morace1 + meduc1 + mpregwt + smoke1, 
             family = binomial, data = smoking)
#model assesment resiudals,
residy <- residuals(newregy,"resp")

#binned residual plots # transformation k jarurat hay ya nahi hay
binnedplot(x=fitted(newregy),y=residy,xlab="Pred. probabilities")
binnedplot(x=smoking$mage,y=residy,xlab="Pred. probabilities")
binnedplot(x=smoking$parity,y=residy,xlab="Pred. probabilities")
binnedplot(x=smoking$mpregwt,y=residy,xlab="Pred. probabilities")


###### Model validation
#let's do the confusion matrix with .5 threshold 
# tell us accuraacy of the model
#sensi positive weight kitne 1 sahi predict karre hay, specifi: true negative weight 0s
Confy <- confusionMatrix(as.factor(ifelse(fitted(newregy) >= 0.5, "1","0")),
                            as.factor(smoking$prematury1),positive = "1")
Confy$table
Confy$overall["Accuracy"];
Confy$byClass[c("Sensitivity","Specificity")] #True positive rate and True negative rate
#Maybe we can try to increase that accuracy.
#Also, the TNR looks low here.

#first, let's repeat with the marginal percentage in the data# we do 
mean(smoking$prematury2)
Confy <- confusionMatrix(as.factor(ifelse(fitted(newregy) >= mean(smoking$prematury2), "1","0")),
                            as.factor(smoking$prematury1),positive = "1")
Confy$table
Confy$overall["Accuracy"];
Confy$byClass[c("Sensitivity","Specificity")]
#huge difference!  seems a lot of predicted probabilities are in the .5 yo .58  range, so cutoff matters.
#either way, we have large off-diagonal numbers. specificity is sensitive to the cutoff

#look at ROC curve # model ko ptimize karney
roc(smoking$prematury1,fitted(newregy),plot=T,print.thres="best",legacy.axes=T,
    print.auc =T,col="red3")
exp(newregy$coefficients)

###model interpretations
are the odds of having premature birth for non smoking asian mother with education 8 to 12 grade. 

confint.default(newregy)   #on log odds scale
exp(confint.default(newregy))   #on odds scale
summary(newregy)
vif(newregy)

