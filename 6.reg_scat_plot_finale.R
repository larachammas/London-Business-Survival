#5. scatterplots 
library(dplyr)
library(tidyr)
library(ggplot2)

# variable names 
#recycling. + total.carb + number.car + cycle.perc + income.sup + AlevelGrad + 
 # children.l + female.lif + worthwhile + happiness + anxiety + BirthsRate + DeathsRate
#example code 

boundaries_scaled@data$Area <- as.factor(boundaries_scaled@data$Area )

boundaries_scaled@data %>%
  ggplot(aes(x=recycling., y=output))+ 
  geom_point(aes(fill=Area),pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  #scale_fill_distiller("BrBG", type="div", direction=1, guide="colourbar", limits=c(-0.29,0.29))+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("correlation:",round(cor.test(boundaries_scaled@data$recycling.,
                                              boundaries_scaled@data$output)$estimate,2)))

####################################################################################################
data.import <- data[,vif_model]
y = as.numeric(data.scale[,1])
Areas <- data$Area
data.import <- data.frame(y, data.import)
cols.num <- names(data.import)
data.import <- as.data.frame(lapply(data.import[cols.num],as.numeric))
data.import <- data.frame(Areas, data.import)
data.import$Areas = as.factor(data.import$Areas)

##########################################################################################
p2 <- data.import %>%
    ggplot(aes(x=total.carbon, y=y))+  
    geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  labs( x = "Total Carbon Emissions", y = 'Two Year Survival Rate of Businesses')+
    stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
    theme_bw()+
    theme(legend.position="none")+
    ggtitle(paste("Coeff:", round(coefficients[3], 2), 
                  "Corr:",round(cor.test(data.import$y,data.import$total.carbon)$estimate,2),
                  "P-value", round(p[3],2)))
                  
p3 <- data.import %>%
  ggplot(aes(x=number.cars, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs( x = "Number of Cars", y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[4], 2), 
                "corr:",round(cor.test(data.import$y,data.import$number.cars)$estimate,2),
                "P-value:", round(p[4],2))) 


p4 <-  data.import %>%
  ggplot(aes(x=cycle.percent, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= '% of adults who cycle at least once per month', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[5], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$cycle.percent)$estimate,2),
                "P-value:", round(p[5],2))) 

p5 <- data.import %>%
  ggplot(aes(x=income.suport, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Income Support Claimant Rate', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[6], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$income.suport)$estimate,2),
                "P-value:", round(p[6],2)))  

p6 <- data.import %>%
  ggplot(aes(x=AlevelGrades, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Achievement of 5 or more A* - C grades at GCSE', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[7], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$AlevelGrades)$estimate,2),
                "P-value:", round(p[8],2)))  

p7 <- data.import %>%
  ggplot(aes(x=children.looked, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Rates of Children Looked After ', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[8], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$children.looked)$estimate,2),
                "P-value:", round(p[8],2)))  

p8 <- data.import %>%
  ggplot(aes(x=pupils.percent.not.english, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= '% of Pupils Whose First Language is Not English', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[9], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$pupils.percent.not.english)$estimate,2),
                "P-value:", round(p[9],2))) 

p9 <- data.import %>%
  ggplot(aes(x=female.life, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Female Life Expectancy', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[10], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$female.life)$estimate,2),
                "P-value:", round(p[10],2))) 

p10 <- data.import %>%
  ggplot(aes(x=teenage.preg, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Teenage Conception Rate', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[11], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$teenage.preg)$estimate,2),
                "P-value:", round(p[11],2))) 

p11 <- data.import %>%
  ggplot(aes(x=worthwhileness, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Worthwhileness Score (out of 10)', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[12], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$worthwhileness)$estimate,2),
                "P-value:", round(p[12],2))) 

p12 <- data.import %>%
  ggplot(aes(x=happiness, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Happiness Score (out of 10)', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[13], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$happiness)$estimate,2),
                "P-value:", round(p[13],2))) 

p13 <- data.import %>%
  ggplot(aes(x=anxiety, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Anxiety Score (out of 10)', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[14], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$anxiety)$estimate,2),
                "P-value:", round(p[14],2))) 

p14 <- data.import %>%
  ggplot(aes(x=BirthsRate, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Rate of New Entrprises Opening', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[15], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$BirthsRate)$estimate,2),
                "P-value:", round(p[15],2))) 

p15 <- data.import %>%
  ggplot(aes(x=DeathsRate, y=y))+  
  geom_point(aes(fill=Areas),colour="#525252",pch=21) +
  stat_smooth(method=lm, se=FALSE, size=1, colour="#525252")+
  labs(x= 'Rate of Entrprises Closing', y = 'Two Year Survival Rate of Businesses')+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle(paste("Coef:", round(coefficients[16], 2), 
                "Corr:",round(cor.test(data.import$y,data.import$DeathsRate)$estimate,2),
                "P-value:", round(p[16],2))) 


grid.arrange(p2,p3,p4, nrow=2) 
grid.arrange(p5,p6,p7,p8, nrow = 2) 
grid.arrange(p9,p10, p11,p12, nrow = 2)


#source("grid_arrange_shared_legend.R")
#grid_arrange_shared_legend(p13, p14, p15)
