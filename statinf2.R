##Question 1

library(ggplot2)
library(datasets)
data(ToothGrowth)
str(ToothGrowth)
head(ToothGrowth)
plot <- ggplot(ToothGrowth, 
               aes(x=factor(dose),y=len,fill=factor(dose)))
plot + geom_boxplot(notch=F) + facet_grid(.~supp) +
        scale_x_discrete("Dosage (Milligram)") +   
        scale_y_continuous("Length of Teeth") +  
        ggtitle("Exploratory Data Analyses")


##Question 2

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
summary(ToothGrowth)
table(ToothGrowth$supp, ToothGrowth$dose)

##Question 3

supp.t1 <- t.test(len~supp, paired=F, var.equal=T, data=ToothGrowth)
supp.t2 <- t.test(len~supp, paired=F, var.equal=F, data=ToothGrowth)
supp.result <- data.frame("p-value"=c(supp.t1$p.value, supp.t2$p.value),
                          "Conf-Low"=c(supp.t1$conf[1],supp.t2$conf[1]),
                          "Conf-High"=c(supp.t1$conf[2],supp.t2$conf[2]),
                          row.names=c("Equal Var","Unequal Var"))
supp.result

##Question 4

## Based on the analysis above, we can conclude that

## 1- The 2mg dose has larger impact on tooth growth than 1mg and 0.5mg, while 1mg dose has more impact than 0.5mg dose. 
## So there is a different in the growth of the tooth while the doses are larger.

##2- There is no doubt that orange juice and vitamin C have obvious different impact on tooth growth.




