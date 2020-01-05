# For spatial data handling
library(rgdal)
library(spdep)
library(rgeos)
# For charting
library(tmap)
library(cartogram)
library(ggplot2)
library(gridExtra)
library(GGally)
# For data loding and munging
library(readr)
library(dplyr)
library(tidyr)
# For spatial stats
library(GWmodel)
library(spdep)
# For cluster analysis
library(cluster)

#get correct data labels 
data <- read.csv('imputed.csv')

# Read in shapefile containing GB LA boundaries which is merged already in python 
gb_boundaries_scaled <- readOGR(dsn = "shapefiles", layer = "Merged_Lon_scaled")
gb_boundaries_num <- readOGR(dsn = "shapefiles", layer = "Merged_Lon_num")

# Set coordinate system -- in this case OSGB: https://epsg.io/27700.
proj4string(gb_boundaries_num) <- CRS("+init=epsg:27700")
proj4string(gb_boundaries_scaled) <- CRS("+init=epsg:27700")

# Note that "gb_boundaries" is an R SpatialDataFrame. A DataFrame containing LA names, codes and
# summary statistics can be accessed through "gb_boundaries@data" 
gb_boundaries_num@data$Area <- as.character(gb_boundaries_num@data$Area)
gb_boundaries_scaled@data$Area <- as.character(gb_boundaries_scaled@data$Area)

boundaries_num <- gb_boundaries_num
boundaries_scaled <- gb_boundaries_scaled

# A SpatialDataFrame must always be supplied to tm_shape(). To tm_fill(), we identify the variable values 
# on which polygons should be coloured as well as information such as the colour mapping (sequential, diverging 
# or continuous) and palette to use. Many, many layout specifications are available in tm_layout. Type ?tm_layout
# into the Console for a complete list.

####### Print a map of london borough's colored by their business two year survival rates REAL NUMBER 
tm_shape(boundaries_num) +  
  tm_fill(col="output",style="cont",palette="BrBG", size=0.2, id="Area", 
          title="Survival Rate %" ) + 
  tm_layout(
    main.title="Two Year Survival Rates of Businesses by London Borough",
    title.snap.to.legend=TRUE,
    main.title.size=0.9,
    legend.text.size=0.8,
    legend.title.size=0.9,
    title.position = c("right", "center"),
    legend.position = c("right","center"),
    frame=FALSE,
    legend.outside=TRUE)
  

# Create models based a selection of variables (we chose by taking VIF scores and other contextual considerations into account)
model_all <- lm(output ~ recycling. + total.carb + number.car + cycle.perc + income.sup + AlevelGrad + 
                  children.l + pupils.per +female.lif + teenage.pr + worthwhile + happiness + 
                  anxiety + BirthsRate + DeathsRate + Y1.Rate + Y4.Rate
                , data = boundaries_scaled@data)
boundaries_scaled@data$resids_all <- resid(model_all)

# Create models based on the recycling rate 
recycling <- lm(output  ~ recycling., data=boundaries_scaled@data)
boundaries_scaled@data$resids_recycling <- resid(recycling)

#print pictures of residuals against recycling"
tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_recycling"),style="cont",palette="RdBu",
          id="Area",size=0.2, title="Residuals", midpoint = NA) + 
  tm_facets(by= 'Area', free.scales = FALSE) +
  tm_layout(
    frame=FALSE,
    main.title='Reycling Rate Residuals',
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_recycling", "resids_all"),style="cont",palette="RdBu",
          id="Area",size=0.2, title="Residuals", midpoint = NA) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame=FALSE,
    panel.labels=c('Recycling','Multivariate'),
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

# Create models based on percent of people cycling 
cycle <- lm(output  ~ cycle.perc, data=boundaries_scaled@data)
boundaries_scaled@data$resids_cycle <- resid(cycle)

tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_cycle", "resids_all"),style="cont",palette="RdBu",
          id="Area",size=0.2, title='', midpoint = NA) + 
  tm_facets(free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    panel.labels=c('% Cycling','Multivariate'),
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

# Create models based on happiness score
happy <- lm(output  ~ happiness, data=boundaries_scaled@data)
boundaries_scaled@data$resids_happy <- resid(happy)

#print pictures of residuals for happiness
tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_happy", "resids_all"),style="cont",palette="RdBu",
          id="Area",size=0.2, title='', midpoint = NA) + 
  tm_facets(free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    panel.labels = c("Happiness Score", "Refined Multivariate"),
    title="Residual",
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

### job density
jobs <- lm(output  ~ jobs.densi, data=boundaries_scaled@data)
boundaries_scaled@data$resids_jobs <- resid(jobs)

tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_jobs", "resids_all"),style="cont",palette="RdBu",
          id="Area",size=0.2, title='', , midpoint = 0) + 
  tm_facets(free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    panel.labels=c("Job Density","Refined Multivariate"),
    title = 'Residuals',
    title.size=1,
    #title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

# Create models based on carbon emissions
carbon <- lm(output  ~ total.carb, data=boundaries_scaled@data)
boundaries_scaled@data$resids_carbon <- resid(carbon)

tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_carbon", "resids_all"),style="cont",palette="RdBu",
          id="Area",size=0.2, title='', , midpoint = NA) + 
  tm_facets(free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    panel.labels=c("Total Carbon Emissions","Refined Multivariate"),
    title = 'Residuals',
    title.size=1,
    #title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)


# Create models based on the number of cars 
cars <- lm(output  ~ number.car, data=boundaries_scaled@data)
boundaries_scaled@data$resids_cars <- resid(cars)

tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_cars", "resids_all"),style="cont",palette="RdBu",
          id="Area",size=0.2, title='', , midpoint = NA) + 
  tm_facets(free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    panel.labels=c("Number of Cars","Refined Multivariate"),
    title = 'Residuals',
    title.size=1,
    #title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

# Create models based on female life expectancy
female <- lm(output  ~ female.lif, data=boundaries_scaled@data)
boundaries_scaled@data$resids_female <- resid(female)

tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_female", "resids_all"),style="cont",palette="RdBu",
          id="Area",size=0.2, title='', midpoint = NA) + 
  tm_facets(free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    panel.labels=c("Female Life Expectancy","Refined Multivariate"),
    title = 'Residuals',
    title.size=1,
    #title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

# Create models based on business death rates
death <- lm(output  ~ DeathsRate, data=boundaries_scaled@data)
boundaries_scaled@data$resids_death <- resid(death)

tm_shape(boundaries_scaled) + 
  tm_fill(col=c("resids_death"),style="cont",palette="RdBu",
          id="Area",size=0.2, title='', midpoint = 0) + 
  tm_facets(by = 'Area', free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    main.title=c("Residuals of Rate of Business Closing"),
    #title = 'Residuals',
    title.size=1,
    #title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)
 
#See them all 
tm_shape(boundaries_scaled) +
  tm_fill(col=c( "resids_all", "resids_carbon", "resids_cars" ,
                 "resids_death", "resids_female", "resids_recycling"),style="cont",palette="RdBu",
          id="Area",size=0.2, title='', midpoint = 0) +
  tm_facets(free.scales = FALSE)+
  tm_layout(
    frame=FALSE,
    panel.labels=c("Refined Multivariate", "Total Carbon Emissions","Number of Cars",
                   "Rate of Businesses Closing", "Female Life Expectancy", "Household Recycling Rate"),
    title = 'Residual',
    title.size=1,
    #title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)


##################### gws ###############################################
 
#The purpose here is to support understanding of how correlations between our outcome 
#and explanatory variable change over space. And as with last week, 
#we'll inspect these correlation coefficient values by plotting them in Choropleth maps.
  
gw_ss <- gwss(boundaries_scaled, vars=c('recycling.', 'number.car' , 'cycle.perc', 'income.sup' ,'AlevelGrad',  
                                            'children.l', 'pupils.per', 'female.lif', 'teenage.pr','worthwhile' ,'happiness', 'anxiety', 
                                            'BirthsRate', 'DeathsRate', 'Y1.Rate', 'Y4.Rate' ), 
              kernel = "bisquare", adaptive = TRUE, bw = 50, quantile = TRUE) #takes over 10min to run


tm_shape(gw_ss$SDF) +
  tm_fill(col=colnames(gw_ss$SDF@data[127:137]), title="gwr coefficients", style="cont",palette="PRGn", size=0.2) + 
  tm_facets(free.scales = FALSE) +
  tm_layout(
    panel.labels=colnames(gw_ss$SDF@data[127:137]),
    frame=FALSE,
    title.snap.to.legend=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

########################## CLUSTERING ##########################
# Select variables used as input to clustering: gw-correlation coefficients

# 'total.carb', 'number.car' , 'cycle.perc', 'income.sup' ,'AlevelGra',  
# 'children.l', 'female.lif', 'worthwhile' ,'happiness', 'anxiety', 
# 'BirthsRate', 'DeathsRate', 'Y2.Num'

gw_correlations <- gw_ss$SDF[,c(127:137)]
colnames(gw_correlations@data)[1:11] <- colnames(boundaries_scaled@data[c(50,51,53,56,58,59,62,65,66,67,69)])
gw_correlations$geo_label <- boundaries_scaled@data$geo_label

# Create distance matrix -- we z-score standardise the variables to ensure that no single variable 
# has undue weight due to its distribution.
dist_matrix <- gw_correlations@data %>%
  select(total.carb, number.car , cycle.perc, income.sup, AlevelGrad, children.l, female.lif, 
worthwhile ,happiness, anxiety,BirthsRate) %>%
  mutate_all(funs(z = .-mean(.)/sd(.)))
dist_matrix <- dist(dist_matrix[,6:10])

# HCA using the Ward's method.
hclustering_ward <- hclust(dist_matrix, method="ward.D2")
# PLot dendrogram
plot(hclustering_ward)

# Evaluate ASW values at different cuts of the dendrogram.
avg.sil.width <- function(cluster_solution, min_cut, max_cut, dist_matrix)
{
  for (i in min_cut:max_cut)
  {
    print(c(i,summary(silhouette(cutree(cluster_solution,k = i), dist_matrix))$avg.width))
  }
}
avg.sil.width(hclustering_ward, 2, 12, dist_matrix)
# Plot of silhuoette profile to evaluate stability of individual groups -- notice that cluster group 2 is 
# particularly poorly defined.
plot(silhouette(cutree(hclustering_ward,4),dist_matrix))
# Add cluster membership as a variable to gw_correlations SpatialDataFrame.
cluster_groups <- cutree(hclustering_ward,4)
gw_correlations@data$cluster_groups <- cluster_groups

tm_shape(gw_correlations) +
  tm_fill(col="cluster_groups", style="cat",id="geo_label", palette="Accent", size=0.2) + 
  tm_borders(col="#636363", lwd=0.2) +
  tm_facets(free.scales = FALSE) +
  tm_layout(
    frame=FALSE,
    title.size=1,
    title.position = c("left", "top"),
    inner.margins = c(0,0,0.15,0),
    legend.title.size=1,
    legend.text.size=0.6,
    legend.outside=TRUE)

gw_correlations@data %>%    
  gather(c(2,3,7,8,9),key="corr_type", value="corr_value") %>%
  group_by(corr_type) %>%
  ggplot(aes(x=corr_value, fill=as.factor(cluster_groups))) +
  geom_density(colour ="#636363") +
  xlim(-1,1) +
  geom_vline(xintercept = 0, colour ="#636363", linetype = "dashed") +
  scale_fill_brewer(type = "qual", palette = "Accent") +
  facet_wrap(~cluster_groups+corr_type) +
  theme_classic()+
  theme(legend.position="none", 
        strip.text.x = element_blank()
  )


