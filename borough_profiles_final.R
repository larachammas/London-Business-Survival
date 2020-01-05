#uploading new london brough data 
library(dplyr)
library(naniar)
library(ggbiplot)
library(pls)

#setwd("~/Desktop/OneDrive - City, University of London/Visual Analytics/BusinessSurvival")

############# Pre Processing of Data ##################
boroughdata <- read.csv('borough_profiles.csv')
businessdata <- read.csv("Business_2012.csv")
businessdata$Year <- NULL

##imput data for missing city of london
businessdata[, 2:16] <-
  lapply(businessdata[, 2:16], function(x)
    ifelse(is.na(x), mean(x, na.rm = TRUE), x))
boroughdata[, 3:65] <-
  lapply(boroughdata[, 3:65], function(x)
    ifelse(is.na(x), mean(x, na.rm = TRUE), x))

##get the variable of interest out
output <- boroughdata$Two.year.business.survival.rates.2012

##merge everyting
data <-
  merge(boroughdata, businessdata[, 1:5], by = 'Area') #dont include the other year survival rates as they correlate
data <- cbind(output, data)
data$Two.year.business.survival.rates.2012 <- NULL
sum(is.na(data)) #check everything okay

## get a dataframe with only the numerical variables
dataNum <- data[,c(-2,-3, -65)]
# #and ensure that all the data is stored as  numeric 
cols.num <- names(dataNum)
dataNum <- as.data.frame(lapply(dataNum[cols.num],as.numeric))

##scale the data for regression prep 
#https://stat.ethz.ch/R-manual/R-devel/library/base/html/scale.html
data.scale <- as.data.frame(scale(dataNum, center = TRUE, scale = TRUE))

#set row names as cities 
rownames(dataNum) <- data$Area
rownames(data) <- data$Area
rownames(boroughdata) <- boroughdata$Area
boroughdata$Area <- NULL

######################### 1. PCA #########################
#https://www.datacamp.com/community/tutorials/pca-analysis-r

### Calculate borough demographics PCA for all numerical values ##
borough.pca <- prcomp(boroughdata[,c(2:63)], center = TRUE,scale. = TRUE) #forget the first two cateogrical columns
summary(borough.pca)
str(borough.pca)

#plot the borough demographics pca
plot.new()
ggbiplot(borough.pca)

ggbiplot(borough.pca, labels=rownames(boroughdata), ellipse = TRUE, groups = boroughdata$Inner_Outer_London, var.axes=FALSE)

#plot 3: change pcs ploted 
ggbiplot(borough.pca, choices = c(3,4), labels = rownames(boroughdata), 
         groups=boroughdata$Inner_Outer_London, var.axes=FALSE, ellipse = TRUE) + 
  ggtitle("PCA 3 and 4")

### business data pca ##
business.pca <- prcomp(businessdata[,2:16], center = TRUE, scale. = TRUE)
summary(business.pca)

plot.new()
ggbiplot(business.pca, labels=businessdata$Area, var.axes=FALSE, groups=data$Inner_Outer_London, ellipse = TRUE)
 
## mereged dataset pca ##
total.pca <- prcomp(dataNum, center = TRUE, scale. = TRUE)
summary(total.pca)

plot.new()
ggbiplot(total.pca, labels=rownames(data), var.axes=FALSE, groups=data$Inner_Outer_London, ellipse = TRUE)

#### Calculate loadings of each PC in the total.pca
source('loadings.R')

################## 2. Principle component regression #############
require(pls)
set.seed (1000)

pcr_model <- pcr(Two.year.business.survival.rates.2012~., 
                 data = boroughdata[3:64], scale = TRUE, validation = "CV")
summary(pcr_model)

validationplot(pcr_model) #plot the root mean squared error #plot results of cv
validationplot(pcr_model, val.type = "MSEP") #plot mean squared error 
validationplot(pcr_model, val.type = "R2") #plot R squared

#plot predicted vs measured values 
predplot(pcr_model)

#plot regression coefficients 
coefplot(pcr_model)

#try to use pcr on a training test to evaulate its preformance using only 6 componenets 
train <- boroughdata[1:25,c(3:64)]
test <- boroughdata[26:32,c(3:33, 34:64)]
y_test <- boroughdata[26:32, 33]

pcr_trained <- pcr(Two.year.business.survival.rates.2012~., 
                   data = train,, scale = TRUE, validation = "CV")
pcr_pred <- predict(pcr_model, test, ncomp = 6)
mean((pcr_pred - y_test)^2)

################### 3. correlation matrix heat map ###############
#https://cran.r-project.org/web/packages/corrplot/vignettes/corrplot-intro.html

library(corrplot)
library(ggcorrplot)

# Compute a correlation matrix
corr_scale <- cor(data.scale)

# Plot the correlation matrix
plot.new()
corrplot(corr_scale, method = "circle", type = "lower", tl.pos = "n")

####################### 4a. a linear regression all variables #############
model_all <- lm(output ~ . , data = dataNum)
#saw lots of NAs and thus read means its due to collinearity 

#################### 4b. Removal of correlated variabels ################## 
#Variance inflation factors removal 
#https://www.r-bloggers.com/collinearity-and-stepwise-vif-selection/

require(MASS)
require(clusterGeneration)
source("vif_func.R")

data <- read.csv('imputed.csv')
vif_model <- vif_func(in_frame=data[,3:76],thresh=10,trace=T)

############################# 5. linear regression with selected VIF variables #################
explainatoryvariabels = data[,vif_model]
y = data[,2]

form.in<-paste('output ~',paste(vif_model,collapse='+'))

LMmodel <- lm(form.in, data = data)
summary(LMmodel)

#get coefficients 
coefficients <- coef(summary(LMmodel))[,"Estimate"]
#
p <- coef(summary(LMmodel))[,"Pr(>|t|)"]

############# 6. Regression Scatter Plots #################
#Run this code to generate the regression scatter plots seen in the report
source("6.reg_scat_plot_finale.R")

#################### 7. Geographically weighted statistics and chlorpleth graphs ########
#run this code to make the boundary files for the geographic weight statstics and generate 
#the faceted maps seen in the report 

#as well as conduct k-means clustering

source("gws_final.R")

### 