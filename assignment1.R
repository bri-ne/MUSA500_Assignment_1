
#--- load libraries ----
#install.packages("DAAG")
#install.packages("car")
#install.packages("MASS")
#install.packages("rsq")
#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("classInt")

library(tidyr)
library(dplyr)
library(DAAG)
library(car)  #to calculate VIF
library(MASS)
library(rsq)
library(kableExtra)
library(tidyverse) #for ggplot
library(sf) #for maps
library(cowplot) #for plotgrid
library(classInt)#for jenks breaks
library(rgdal)
library(ggplot2)
library(RColorBrewer)


display.brewer.pal(10, 'BuPu')
brewer.pal(9, 'BuPu')
options(scipen=999)

#---- Step 1: Upload Data ----

ourdata <- read.csv("https://raw.githubusercontent.com/bri-ne/MUSA500_Assignment_1/main/RegressionData.csv")

#---- Step 1A.i: Mean, SD Summary Stats ----

#print summary of ourdata to find mean and other things.
summary(ourdata)

#save the summary to a df (i have 0 idea how this code works, just a copy paste from the web)
#we really only need the mean and sd, so I will clean this up
summary <-as.data.frame(apply(ourdata,2,summary))
#checking the index
base::row(summary,1)
#dropping all rows except the mean
summary<-summary[-c(1,2,3,5,6),]
#transposing so variables (mean and sd) are columns
summary <- summary%>%t()%>%as.data.frame()

#getting sd and adding sd column
sdMedV <- sd(ourdata$MEDHVAL)
sdBach <- sd(ourdata$PCTBACHMOR)
sdNbel <- sd(ourdata$NBELPOV100)
sdVac <- sd(ourdata$PCTVACANT)
sdSing <- sd(ourdata$PCTSINGLES)
sdMedHHINC <- sd(ourdata$MEDHHINC)

sdcol <- list(0, 0, sdMedV, sdBach, sdMedHHINC, sdVac, sdSing, sdNbel)
summary$sd <- sdcol

#dropping the other things we don't need 
base::row(summary,1)
summary<- summary[-c(1,2,5),]
summary<- summary%>%as.data.frame()
summary


#create a copy of the summary table to give it nicer row names for the table below
summary_nice_names <- summary
rownames(summary_nice_names) <- c("Median House Value",
                                  "% of Indviduals with Bachelor's Degrees or Higher",
                                  '% of Vacant Houses',
                                  "% of Single House Units",
                                  "% Households Living in Poverty")
#final summary stats table
summary_table <- kable(summary_nice_names) %>%
  kable_styling() %>%
  footnote(general_title = "Summary Statistics\n",
           general = "Table 1")

summary_table

#---- Step 1A.ii: Histograms ----

#check them out, regular histogram

hists <- histogram( ~ MEDHVAL +PCTBACHMOR +NBELPOV100 +PCTSINGLES +PCTVACANT, layout=c(2,3), data = ourdata,
                    main='Distribution of Raw Variables', col="#8C96C6", sub = 'Figure 1', breaks = 50, scales='free')   
hists

#none look normal so we will examine if a log transformation will make them normal
#remember to use 1+ if any variable has zero values
#the only variable that does not have a 0 value is MEDHVAL

ourdata$LNMEDHVAL <- log(ourdata$MEDHVAL)

ourdata$LNPCTBACHMOR <- log(1+ourdata$PCTBACHMOR)

ourdata$LNBELPOV100 <- log(1+ourdata$NBELPOV100)

ourdata$LNPCTVACANT <- log(1+ourdata$PCTVACANT)

ourdata$LNPCTSINGLES <- log(1+ourdata$PCTSINGLES)


#**************histograms of transformed variables for markdown************************
#### Logged 

LNhists <- histogram( ~ LNMEDHVAL +LNPCTBACHMOR +LNBELPOV100 +LNPCTSINGLES +LNPCTVACANT,layout=c(2,3),data = ourdata, 
                    main='Distribution of Natural Log of Variables', sub= 'Figure 2', col="#B3CDE3", breaks = 50, scales='free')   
LNhists
#### Not logged
hist(ourdata$NBELPOV100)

hists
#after viewing our logged variables, it seems like these are best to keep: 
#this is my idea to save in a list (seems like it will help us remember), 
#but Eugene doesn't do this, so it might not be necessary
#see step 1 Aiib2 for explanation on why we keep these as predictors
predictors <- list(c("LNNBELPOV100", 
                     "PCTBACHMOR", 
                     "PCTVACANT", 
                     "PCTSINGLES"))

predictors <- as.data.frame(predictors)
dependentV <- "LNMEDHVAL"


#---- Step 1B: Variable Scatter Plots ----
#investigate to see if our predictors relationship with the dependent variable
# is linear by plotting as scatter plots

pLNBelpov <- ggplot(ourdata, aes(x = LNMEDHVAL, y= LNBELPOV100))+
            geom_point(size=2.5, color = "#4D004B", alpha = 0.5)+theme_minimal()
            
pBach <- ggplot(ourdata, aes(x = LNMEDHVAL, y= PCTBACHMOR))+
  geom_point(size=2.5, color = "#4D004B", alpha = 0.5)+theme_minimal()
  
  
pVac <- ggplot(ourdata, aes(x = LNMEDHVAL, y= PCTVACANT))+
  geom_point(size=2.5, color = "#4D004B", alpha = 0.5)+theme_minimal()
  
  

pSing <- ggplot(ourdata, aes(x = LNMEDHVAL, y= PCTSINGLES))+
  geom_point(size=2.5, color = "#4D004B", alpha = 0.5)+theme_minimal()



scattergrid <- plot_grid( 
           pLNBelpov, 
           pBach, 
           pVac, 
           pSing, 
           labels = c("% Below Poverty (LN)", 
                      "Bachelors Degree",
                      "% Vacant Homes", 
                      "% Single House Units"), 
           label_x =-.1,
           label_y = 1.01,
           scale= 0.9,
           align = 'hv',
           ncol = 2, nrow = 2)


#***********final grid to include in markdown *******************
scattergrid

#---- Step 1C: Pearson Correlations ----
#make new df that just has our predictors and dependent variable

pred_var <- ourdata%>%dplyr::select(LNMEDHVAL, LNBELPOV100, PCTBACHMOR, PCTSINGLES, PCTVACANT)
pcorr <- cor(pred_var, method="pearson")

#Observe that there isnt severe multicollinearity (i.e., no correlations where
# r>.8 or r<-.8), so we can include all four predictors in the regression.

#*********matrix for markdown*****************************************
pcorr

#---- Step 2A: Open Regression_Data shapefile (or GeoJSON in our case)----
#shapefiles are really hard to work with without having to download and mess with working directories in the code
#SO i converted the shapefile to a GeoJSON and will use that instead


ourdata_geom <- st_read("https://raw.githubusercontent.com/bri-ne/MUSA500_Assignment_1/main/RegressionData.geojson")


#getting Jenks Breaks for LNMEDHVAL 
classes <- classIntervals(ourdata_geom$LNMEDHVAL, n = 5, style = "jenks")
classes$brks

#we'll create a new column in our sf object using the base R cut() function to cut up our percent variable into distinct groups.

ourdata_geom <- ourdata_geom %>%
  mutate(LNMEDVHAL_class = cut(LNMEDHVAL, classes$brks, include.lowest = T))


#mapping
choro_LNMEDHVAL <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = LNMEDVHAL_class),
          alpha = 1,
          colour = "gray80",
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "LN Median House Value") +
  labs(x = NULL, y = NULL,
       title = "LN Median House Value in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

#making remaining JENKS for maps --------------------

#PctVacant Jenks
PVclasses <- classIntervals(ourdata_geom$PCTVACANT, n = 5, style = "jenks")

PVclasses$brks

ourdata_geom <- ourdata_geom %>%
  mutate(PCTVACANT_class = cut(PCTVACANT, PVclasses$brks, include.lowest = T))

#PctSingle Jenks
PSclasses <- classIntervals(ourdata_geom$PCTSINGLES, n = 5, style = "jenks")

PSclasses$brks

ourdata_geom <- ourdata_geom %>%
  mutate(PCTSINGLES_class = cut(PCTSINGLES, PSclasses$brks, include.lowest = T))

#PctBach Jenks
PBclasses <- classIntervals(ourdata_geom$PCTBACHMOR, n = 5, style = "jenks")

PBclasses$brks

ourdata_geom <- ourdata_geom %>%
  mutate(PCTBACHMOR_class = cut(PCTBACHMOR, PBclasses$brks, include.lowest = T))

#LNBelPov Jenks
BPclasses <- classIntervals(ourdata_geom$LNNBELPOV, n = 3, style = "jenks")

BPclasses$brks
ourdata_geom <- ourdata_geom %>%
  mutate(LNNBELPOV_class = cut(LNNBELPOV, BPclasses$brks, include.lowest = T))


#mapping the rest --------------------------------------

#PctVacant Map

choro_PctVac <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = PCTVACANT_class),
          alpha = 1,
          colour = "gray80",
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "Percent Houses Vacant") +
  labs(x = NULL, y = NULL,
       title = "Perecentage of Vacant Houses\n in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())



#PctSing Map
choro_PctSing <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = PCTSINGLES_class),
          alpha = 1,
          colour = "gray80",
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "Percent Single House Units") +
  labs(x = NULL, y = NULL,
       title = "Perecentage of Single House Units\n in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())


#PctBach Map
choro_PctBach <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = PCTBACHMOR_class),
          alpha = 1,
          colour = "gray80",
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "Percent Bachelors Degree") +
  labs(x = NULL, y = NULL,
       title = "Percentage of Individuals with a Bachelors Degree or Higher\n in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

#LNBelPov Map
choro_LNBelPov <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = LNNBELPOV_class),
          alpha = 1,
          colour = "gray80",
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "LN Percent Households in Poverty") +
  labs(x = NULL, y = NULL,
       title = "LN Percentage of Households Living in Poverty\n in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())


#************choropleths for markdown ************************
#--- Unfortunately I had to do three breaks b/c the values for the LN %BELPOV were not varied enough
choro_LNMEDHVAL
choro_PctVac 
choro_PctSing 
choro_PctBach 
choro_LNBelPov

mapgrid <- plot_grid( choro_PctVac, 
                      choro_PctSing, 
                      choro_PctBach, 
                      choro_LNBelPov,
                          ncol = 2, nrow = 2)

#---- Step 3 : Regression Analysis -------

#Run regression model
#fit <- lm(MEDHVAL ~ LNBELPOV100 + PCTBACHMOR + PCTVACANT + PCTSINGLES, data=ourdata)
fit <- lm(LNMEDHVAL ~ LNBELPOV100 + PCTBACHMOR + PCTVACANT + PCTSINGLES, data=ourdata)

#3A
summary(fit)

#Summary of Regression
#all predictors are significant - p value < 0.05
#R squared - 55.38% = % of variance in median house value explained 
    #by predictors 
#Adjusted R Squared - 55.28% = % of variance in median house value explained
    #BY predictors adjusted for the number of predictors
#P value associated with F-ratio of 532 is less than 0.0001 -
    #We can reject the H0 that all Beta coefficients for the predictors are 0

#3B
anova(fit)

#ANOVA table containing SSE and SSR
#SSR = SS(LNBELPOV100) + SS(PCTBACHMOR) +SS(PCTVACANTS) + SS(PCTSINGLES) = 
    # 869866100108 + 2345334804156 +  58030777316 + 154801231763 = 3428032913343
    # Amount of total variance in Median House value explained by model
#SSE = 2761620505240 = amount of total variance in median house value that is 
    # unexplained by the model

#3C
#new column saving predicted values 
ourdata$predvals <- fitted(fit)

#new column saving residuals
ourdata$resids <- residuals(fit)

#New column saving standardized residuals
ourdata$stdres <- rstandard(fit)

#3D Create a scatterplot with standardized residuals on Y axis and
    # Predicted Values on x axis. 

ResidualPlot<- ggplot(ourdata, aes(x = predvals, y= stdres))+
                  geom_point(size=1.5, color = 'darkslateblue', alpha = .5)+
                  geom_hline(yintercept=0, size = .5)+
                  labs(x = 'Predicted Median House Value ($)', 
                       y = 'Standardized Residuals')
stdr.reshist <- histogram( ~ stdres,data = ourdata, 
                         main='Standardized Regression Residuals Histogram', 
                         sub= 'Figure 4', 
                         col="#B3CDE3", 
                         breaks = 50, 
                         scales='free')  

stdr.reshist
#----Step 4 ----
#use step and step$anova commands in the MASS library to run stepwise
#regression and determine the best model based on the Akaike Information
#Criterion. Save the step$anova output

step <- stepAIC(fit, direction="both")



# Save output of step$anova for markdown!
stepoutput <- step$anova

steptable<- kbl(stepoutput)%>%
              kable_material

steptable
#----Step 5 ----

#In the output: 
#Predicted (Predicted values using all observations) 
#cvpred (cross-validation predictions)
#CV residual = y (in this case, MEDHHINC) - cvpred
cv <- CVlm(data=ourdata, fit, m=5)				       
#Extracting MSEs
mse <- attr(cv, "ms")
mse
rmse <- sqrt(mse)						 
rmse

#CROSS-VALIDATION
#Model 2
fit2 <- lm(LNMEDHVAL ~ MEDHHINC + PCTVACANT , data=ourdata)
summary(fit2)
anova(fit2)

#Model
#fit <- lm(MEDHHINC ~ PCTVACANT + MEDHVAL + PCTSINGLES, data=ourdata)
#summary(fit)
#anova(fit)
cv2 <- CVlm(data=ourdata, fit2, m=5)				        

#Extracting MSEs

mse2 <- attr(cv2, "ms")
mse2
rmse2 <- sqrt(mse2)					                    
rmse2

#----Step 6 ----
#new column saving predicted values 
ourdata$predvals2 <- fitted(fit2)
#adding to geom too for mapping
ourdata_geom$predvals2 <- fitted(fit2)
ourdata_geom$predvals <- fitted(fit) #### from our first model


#new column saving residuals
ourdata$resids2 <- residuals(fit2)
#adding to geom too for mapping
ourdata_geom$resids2 <- residuals(fit2)
ourdata_geom$resids <- residuals(fit) #### from our first model

#New column saving standardized residuals
ourdata$stdres2 <- rstandard(fit2)
#adding to geom too for mapping
ourdata_geom$stdres2 <- rstandard(fit2)
ourdata_geom$stdres <- rstandard(fit)#### from our first model
 

#### I'm not sure we need this scatter plot -- BRi
# Create a scatterplot with standardized residuals on Y axis and
# Predicted Values on x axis. 

ResidualPlot2<- ggplot(ourdata, aes(x = predvals2, y= stdres2))+
  geom_point(size=1.5, color = 'darkslateblue', alpha = .5)+
  geom_hline(yintercept=0, size = .5)+
  labs(x = 'Predicted Median House Value ($)', 
       y = 'Standardized Residuals')
ResidualPlot2


#### we do need this Histogram thought --- Bri

Residhists <- histogram( ~ stdres +stdres2,layout=c(2,1),data = ourdata, 
                      main='Standardized Regression Residuals From Both Models', xlab='regressing on all variables (LEFT) and regressing on only income and vacancy (RIGHT)', sub= 'Figure 3', col="#B3CDE3", breaks = 50, scales='free')  

Residhists

#getting Jenks Breaks for LNMEDHVAL 
standres2classes <- classIntervals(ourdata_geom$stdres2, n = 5, style = "jenks")
standres2classes$brks
typeof(ourdata_geom$stdres2)

#we'll create a new column in our sf object using the base R cut() function to cut up our percent variable into distinct groups.

ourdata_geom <- ourdata_geom %>%
  mutate(stdresclass2 = cut(stdres2, standres2classes$brks, include.lowest = T))

#mapping
choro_stdresclass2 <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = stdresclass2),
          alpha = 1,
          colour = NA,
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "LN Median House Value") +
  labs(x = NULL, y = NULL,
       title = "Standardized Residuals for Our Second Model\n in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census",
       caption= "Map 4") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

choro_stdresclass2

#### doing this for our first model's resids just in case,
#getting Jenks Breaks for LNMEDHVAL 
standresclasses <- classIntervals(ourdata_geom$stdres, n = 5, style = "jenks")
standresclasses$brks
typeof(ourdata_geom$stdres)

#we'll create a new column in our sf object using the base R cut() function to cut up our percent variable into distinct groups.

ourdata_geom <- ourdata_geom %>%
  mutate(stdresclass = cut(stdres, standresclasses$brks, include.lowest = T))

#mapping
choro_stdresclass <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = stdresclass),
          alpha = 1,
          colour = NA,
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "LN Median House Value") +
  labs(x = NULL, y = NULL,
       title = "Standardized Residuals for Our First Model\n in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census",
       caption= "Map 3") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

choro_stdresclass

stdres_maps_both <- plot_grid( choro_stdresclass,
                               choro_stdresclass2, 
                              #labels = c("% Below Poverty (LN)", 
                               #        "Bachelors Degree",
                                #       "% Vacant Homes", 
                                 #      "% Single House Units"), 
                            #label_x =-.1,
                            #label_y = 1.01,
                            scale= 0.9,
                            align = 'hv',
                            ncol = 2, nrow = 1)

stdres_maps_both



##### if we need this I have i there: 





```{r stdresANDmap.code, echo=FALSE, warning=FALSE, message=FALSE, results='hide', include =FALSE}

#In the output: 
#Predicted (Predicted values using all observations) 
#cvpred (cross-validation predictions)
#CV residual = y (in this case, MEDHHINC) - cvpred
cv <- CVlm(data=ourdata, fit, m=5)				       
#Extracting MSEs
mse <- attr(cv, "ms")
mse
rmse <- sqrt(mse)						 
rmse

#CROSS-VALIDATION
#Model 2
fit2 <- lm(LNMEDHVAL ~ MEDHHINC + PCTVACANT , data=ourdata)
summary(fit2)
anova(fit2)

#Model
#fit <- lm(MEDHHINC ~ PCTVACANT + MEDHVAL + PCTSINGLES, data=ourdata)
#summary(fit)
#anova(fit)
cv2 <- CVlm(data=ourdata, fit2, m=5)				        

#Extracting MSEs

mse2 <- attr(cv2, "ms")
mse2
rmse2 <- sqrt(mse2)					                    
rmse2

#----Step 6 ----
#new column saving predicted values 
ourdata$predvals2 <- fitted(fit2)
#adding to geom too for mapping
ourdata_geom$predvals2 <- fitted(fit2)
ourdata_geom$predvals <- fitted(fit) #### from our first model


#new column saving residuals
ourdata$resids2 <- residuals(fit2)
#adding to geom too for mapping
ourdata_geom$resids2 <- residuals(fit2)
ourdata_geom$resids <- residuals(fit) #### from our first model

#New column saving standardized residuals
ourdata$stdres2 <- rstandard(fit2)
#adding to geom too for mapping
ourdata_geom$stdres2 <- rstandard(fit2)
ourdata_geom$stdres <- rstandard(fit)#### from our first model


#### I'm not sure we need this scatter plot -- BRi
# Create a scatterplot with standardized residuals on Y axis and
# Predicted Values on x axis. 

#ResidualPlot2<- ggplot(ourdata, aes(x = predvals2, y= stdres2))+
#  geom_point(size=1.5, color = 'darkslateblue', alpha = .5)+
#  geom_hline(yintercept=0, size = .5)+
#  labs(x = 'Predicted Median House Value ($)', 
#       y = 'Standardized Residuals')
#ResidualPlot2


#### we do need this Histogram though right?--- Bri #########

#Residhists <- histogram( ~ stdres +stdres2,layout=c(2,1),data = ourdata, 
#                      main='Standardized Regression Residuals From Both Models', #xlab='regressing on all variables (LEFT) and regressing on only income and vacancy (RIGHT)', sub= 'Figure 3', col="#B3CDE3", breaks = 50, scales='free')  

#Residhists

##############
#getting Jenks Breaks for LNMEDHVAL 
standres2classes <- classIntervals(ourdata_geom$stdres2, n = 5, style = "jenks")
standres2classes$brks
typeof(ourdata_geom$stdres2)

#we'll create a new column in our sf object using the base R cut() function to cut up our percent variable into distinct groups.

ourdata_geom <- ourdata_geom %>%
  mutate(stdresclass2 = cut(stdres2, standres2classes$brks, include.lowest = T))

####### Second Resid Map if we need it ###################
choro_stdresclass2 <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = stdresclass2),
          alpha = 1,
          colour = NA,
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "LN Median House Value") +
  labs(x = NULL, y = NULL,
       title = "Standardized Residuals for Our Second Model\n in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census",
       caption= "Map 4") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

#choro_stdresclass2

##############################################################
#### doing this for our first model's resids just in case,
#getting Jenks Breaks for LNMEDHVAL 
standresclasses <- classIntervals(ourdata_geom$stdres, n = 5, style = "jenks")
standresclasses$brks
typeof(ourdata_geom$stdres)

#we'll create a new column in our sf object using the base R cut() function to cut up our percent variable into distinct groups.

ourdata_geom <- ourdata_geom %>%
  mutate(stdresclass = cut(stdres, standresclasses$brks, include.lowest = T))

#mapping
choro_stdresclass <- ggplot() +
  geom_sf(data = ourdata_geom,
          aes(fill = stdresclass),
          alpha = 1,
          colour = NA,
          size = 0.15) +
  scale_fill_brewer(palette = "PuBu",
                    name = "LN Median House Value") +
  labs(x = NULL, y = NULL,
       title = "Standardized Residuals in Philadelphia by Block Group",
       subtitle = "Source: U.S. Census",
       caption= "Map 3") +
  theme(line = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

choro_stdresclass

stdres_maps_both <- plot_grid( choro_stdresclass,
                               choro_stdresclass2, 
                               #labels = c("% Below Poverty (LN)", 
                               #        "Bachelors Degree",
                               #       "% Vacant Homes", 
                               #      "% Single House Units"), 
                               #label_x =-.1,
                               #label_y = 1.01,
                               scale= 0.9,
                               align = 'hv',
                               ncol = 2, nrow = 1)

stdres_maps_both

```


