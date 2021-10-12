#--- load libraries ----
install.packages("DAAG")
install.packages("car")
install.packages("MASS")
install.packages("rsq")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("classInt")

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

#check them out

hist(ourdata$MEDHVAL)
hist(ourdata$PCTBACHMOR)
hist(ourdata$NBELPOV100)
hist(ourdata$PCTVACANT)
hist(ourdata$PCTSINGLES)

#none look normal so we will examine if a log transformation will make them normal
#remember to use 1+ if any variable has zero values
#the only variable that does not have a 0 value is MEDHVAL

ourdata$LNMEDHVAL <- log(ourdata$MEDHVAL)
hist_LNMEDHVAL <- hist(ourdata$LNMEDHVAL)

ourdata$LNPCTBACHMOR <- log(1+ourdata$PCTBACHMOR)
hist_LNPCTBACHMOR <- hist(ourdata$LNPCTBACHMOR)

ourdata$LNBELPOV100 <- log(1+ourdata$NBELPOV100)
hist_LNBELPOV100 <- hist(ourdata$LNBELPOV100)

ourdata$LNPCTVACANT <- log(1+ourdata$PCTVACANT)
hist_LNPCTVACANT <- hist(ourdata$LNPCTVACANT)

ourdata$LNPCTSINGLES <- log(1+ourdata$PCTSINGLES)
hist_LNPCTSINGLES <- hist(ourdata$LNPCTSINGLES)

#**************histograms of transformed variables for markdown************************
hist_LNMEDHVAL
hist_LNPCTBACHMOR
hist_LNBELPOV100 
hist_LNPCTVACANT
hist_LNPCTSINGLES 


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

pBelpov <- ggplot(ourdata, aes(x = LNMEDHVAL, y= LNBELPOV100))+
            geom_point(size=2.5, color = 'darkslateblue', alpha = 0.5)
            
pBach <- ggplot(ourdata, aes(x = LNMEDHVAL, y= PCTBACHMOR))+
  geom_point(size=2.5, color = 'darkslateblue', alpha = 0.5)
  
  
pVac <- ggplot(ourdata, aes(x = LNMEDHVAL, y= PCTVACANT))+
  geom_point(size=2.5, color = 'darkslateblue', alpha = 0.5)
  
  

pSing <- ggplot(ourdata, aes(x = LNMEDHVAL, y= PCTSINGLES))+
  geom_point(size=2.5, color = 'darkslateblue', alpha = 0.5)



scattergrid <- plot_grid( 
           pBelpov, 
           pBach, 
           pVac, 
           pSing, 
           labels = c("% Below Poverty (LN)", 
                      "Bachelors Degree",
                      "% Vacant Homes", 
                      "% Single House Units"), 
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
fit <- lm(MEDHVAL ~ LNBELPOV100 + PCTBACHMOR + PCTVACANT + PCTSINGLES, data=ourdata)

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

#----Step 4 ----
#use step and step$anova commands in the MASS library to run stepwise
#regression and determine the best model based on the Akaike Information
#Criterion. Save the step$anova output

step <- stepAIC(fit, direction="both")


# Save output of step$anova for markdown!
step$anova


#----Step 5 ----

#CROSS-VALIDATION
#Model 1
fit <- lm(MEDHHINC ~ PCTVACANT + PCTSINGLES, data=ourdata)
summary(fit)
anova(fit)
                         
#In the output: 
#Predicted (Predicted values using all observations) 
#cvpred (cross-validation predictions)
#CV residual = y (in this case, MEDHHINC) - cvpred
cv <- CVlm(data=ourdata, fit, m=5)				        #m=5 sets it to 5 folds
#Extracting MSEs
mse <- attr(cv, "ms")
mse
rmse <- sqrt(mse)						  #Obtaining RMSE for model 1
rmse

#Model 2
fit <- lm(MEDHHINC ~ PCTVACANT + MEDHVAL + PCTSINGLES, data=ourdata)
summary(fit)
anova(fit)
cv <- CVlm(data=datadata, fit, m=5)				        #m=5 sets it to 5 folds
summary(cv)
#Extracting MSEs
mse <- attr(cv, "ms")
mse
rmse <- sqrt(mse)					                      #Obtaining RMSE for model 2
rmse




