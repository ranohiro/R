setwd("~/Desktop/Qiita/figure")
library(multcomp) #多重比較につかう
library(psych) #多重比較前の等分散性の検定につかう

d <-read.csv("test_abc.csv", header = TRUE)
d$ecotype <- factor(d$ecotype)
attach(d)

leveneTest(chlorophyll, ecotype, center = mean) #等分散性の検定
anova(lm(chlorophyll ~ ecotype)) #一元配置分散分析
TukeyHSD(aov(chlorophyll ~ ecotype)) #Tukey’s t-test

res <- aov(chlorophyll ~ ecotype, data = d) #ANOVAをresという函に格納
tuk <- glht(res, linfct = mcp(ecotype = "Tukey")) #多重比較の結果をtukという函へ格納
cld(tuk, decreasing = F) #アルファベットとして検定結果を出力