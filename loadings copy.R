library(factoextra)
library(rowr)

#discover from the mse and r-squared plots that have best model with 9 components 
#extract the top most correlated variables from each ofthe 9 components 

loadings_total <- as.data.frame(total.pca$rotation)
loadings_red <- as.data.frame(total.pca$rotation)

loadings$variables <- rownames(loadings)
loadings_red$variables <- rownames(loadings_red)
rownames(loadings) <- c(1:nrow(loadings))

important1 <- subset(loadings, PC1 > 0.15 | PC1 < - 0.15 , select = c(PC1, variables))

important1_red <- subset(loadings_red, PC1 > 0.017 | PC1 < -0.005 , select = c(PC1, variables))

important2 <- subset(loadings, PC2 > 0.15 | PC2 < - 0.15 , select = c(PC2, variables))

important3 <- subset(loadings, PC3 > 0.15 | PC3 < - 0.15 , select = c(PC3, variables))

important4 <- subset(loadings, PC4 > 0.15 | PC4 < - 0.15 , select = c(PC4, variables))

important5 <- subset(loadings, PC5 > 0.15 | PC5 < - 0.15 , select = c(PC5, variables))

important6 <- subset(loadings, PC6 > 0.15 | PC6 < - 0.15 , select = c(PC6, variables))

important7 <- subset(loadings, PC7 > 0.15 | PC7 < - 0.15 , select = c(PC7, variables))

important8 <- subset(loadings, PC8 > 0.15 | PC8 < - 0.15 , select = c(PC8, variables))

important9 <- subset(loadings, PC9 > 0.15 | PC9 < - 0.15 , select = c(PC9, variables))

importantVars <- cbind.fill(important1,important2, important3, important4,
                            important5, important6, important7, important8 , important9, fill = NA)

Variabels <- importantVars(variabels)

library(tidyr)
library(dplyr)
vars <- importantVars %>% 
  gather(PC, n, c(PC1)) 

