brute<-read.csv("Documents/Studies/ESPRIT/4thYear/Machine Learning/Labs/Lab2/Final_Analysis/AssurancExpertsInc.txt",sep="\t",header = T)

levels(brute$CLASS)
brute$CLASS<-relevel(brute$CLASS,"Yes")
table(brute$CLASS)
learning.Set<-brute[which(brute$STATUS=="Learning"),]
test.Set<-brute[which(brute$STATUS=="Test"),]

library(dplyr)
learning.Set<-select(learning.Set,-STATUS)
test.Set<-select(test.Set,-STATUS)

modele<-glm(CLASS~ 1,data = learning.Set,family = binomial)
modele
str_constant <- "~ 1"
str_all <- "~SD1+SD2+SD3+SD4+SD5+SD6+SD7+SD8+SD9+SD10+SD11+SD12+SD13+SD14+SD15+SD16+SD17+SD18+SD19+SD20+SD21+SD22+SD23+SD24+SD25+SD26+SD27+SD28+SD29+SD30+SD31+SD32+SD33+SD34+SD35+SD36+SD37+SD38+SD39+SD40+SD41+SD42+SD43+PO44+PO45+PO46+PO47+PO48+PO49+PO50+PO51+PO52+PO53+PO54+PO55+PO56+PO57+PO58+PO59+PO60+PO61+PO62+PO63+PO64+PO65+PO66+PO67+PO68+PO69+PO70+PO71+PO72+PO73+PO74+PO75+PO76+PO77+PO78+PO79+PO80+PO81+PO82+PO83+PO84+PO85"

library(MASS)
mod2<-stepAIC(modele, data=learning.Set,scope = list(lower = str_constant, upper = str_all),trace = TRUE,direction = "both")

mod2
summary(mod2)
