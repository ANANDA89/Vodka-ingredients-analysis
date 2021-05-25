install.packages("readxl")
library("readxl")
vodka_data=read_excel("Vodka_data.xlsx")
head(vodka_data)
install.packages("naniar")
library("naniar")
miss_var_summary(vodka_data)
summary(vodka_data)

Regression1 = lm(Q5A_att1 ~ Q5A_att2,data=vodka_data)
summary(Regression1)

Regression2 = lm(Q5A_att1 ~ Q5A_att2+Q5A_att3+Q5A_att4+Q5A_att5+Q5A_att6+Q5A_att7+Q5A_att8,data=vodka_data)
summary(Regression2)

scaled_data=scale(vodka_data)
#scaled_att2=scale(vodka_data$Q5A_att2,vodka_data$Q5A_att3,vodka_data$Q5A_att4,vodka_data$Q5A_att5,vodka_data$Q5A_att6,vodka_data$Q5A_att7,vodka_data$Q5A_att8)
scaled_data=data.frame(scaled_data)
Regression3 = lm(Q5A_att1 ~ Q5A_att2+Q5A_att3+Q5A_att4+Q5A_att5+Q5A_att6+Q5A_att8,data=scaled_data)
summary(Regression3)
scaled_data1=scaled_data[-c(19)]
head(scaled_data1)
install.packages("corrplot")
library("corrplot")
COR=cor(vodka_data[,14:20])
COR
install.packages("nFactors")
library(nFactors)
nScree(vodka_data[,14:20], cor=TRUE) #This function help you determine the number of factors
install.packages("psych")

library("psych")
fit = principal(Vodka_data[,14:20], nfactors=5, rotate="none")
fit$loadings

fit = principal(Vodka_data[,14:20], nfactors=3, rotate="varimax")
fit$loadings

colnames(fit$weights) = c("Initial_Sensory", "Smoothness", "Aroma") # Naming the factors
fit$weights



colnames(fit$scores) = c("Initial_Sensory", "Smoothness", "Aroma") 
new_data=cbind(Vodka_data ,fit$scores)

head(new_data)

Regression = lm(Q5A_att1 ~ Initial_Sensory+Smoothness+Aroma, data=new_data)
summary(Regression)

