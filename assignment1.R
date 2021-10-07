#--- load libraries ----
install.packages("DAAG")
install.packages("car")
install.packages("MASS")
install.packages("rsq")
install.packages("gridExtra")
install.packages("cowplot")
install.packages('BAMMtools')

library(tidyr)
library(dplyr)
library(DAAG)
library(car)  #to calculate VIF
library(MASS)
library(rsq)
library(kableExtra)
library(tidyverse) #for ggplot
library(sf) #for maps
library(rgdal) #for downloading shape file from web
library(cowplot) #for plotgrid
library(BAMMtools) #for jenks breaks


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
#transposing so varibles (mean and sd) are columns
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



scattergrid <- plot_grid( pBelpov, 
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
ourdata_geom$LNMEDVHAL_Jenks <- getJenksBreaks(ourdata_geom$LNMEDVHAL, 5, subset = NULL)
chloro
ggplot()+
  geom_sf(data= ourdata_geom, aes(fill = LNMEDVHAL_Jenks), color = NA)


