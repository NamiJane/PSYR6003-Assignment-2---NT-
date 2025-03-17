library(tidyverse)
library(flexplot)
library(haven)
library(apaTables)

#import datset
PSYR6003_A2 <- read_sav("PSYR6003.A2.sav")
View(PSYR6003_A2)

# Sex re coded as a factor variable, where 0 = Female, 1 = Male
A2_Data <- PSYR6003_A2 %>% mutate(sex = factor(sex, levels = c("Female", "Male"), labels = c(0, 1)))

#Check: To view if sex is coded as <fct> in consol after mutations
A2_Data

#Check: Double checking if sex was successfuly changed to a factor variable (output says TRUE, so this should indicate it was properly changed)
is.factor(A2_Data$sex)

#Remove missing values 
A2_Data <- na.omit(A2_Data)

#Reverse code tipm.CONS2.3y
A2_Data_rev <- mutate(A2_Data, tipm.CONS2.3y_rev = 8 - tipm.CONS2.3y)
view(A2_Data_rev)

#Add in new row for SSP, conscientiousness, and negative affect total (as a mean)
A2_Data_Full <- mutate(A2_Data_rev, SPP_Mean = rowMeans(select(A2_Data_rev, mps.SPP1.3y, mps.SPP2.3y, mps.SPP3.3y, mps.SPP4.3y, mps.SPP5.3y), na.rm = TRUE)) %>%
  mutate(A2_Data_rev, Con_Mean = rowMeans(select(A2_Data_rev, tipm.CONS1.3y, tipm.CONS2.3y_rev), na.rm = TRUE)) %>%
  mutate(A2_Data_rev, Neg_Mean = rowMeans(select(A2_Data_rev, guilt1.3y, guilt2.3y, guilt3.3y, dep1.3y, dep2.3y, dep3.3y, fear1.3y, fear2.3y, fear3.3y, host1.3y, host2.3y, host3.3y), na.rm = TRUE)) 

#Checking to see if new columbs were added correctly
view(A2_Data_Full)

#Univariate and model visualizations
flexplot(SPP_Mean~1, data=A2_Data_Full)
flexplot(Con_Mean~1, data=A2_Data_Full)
flexplot(Neg_Mean~1, data=A2_Data_Full) 
flexplot(sex~1, data=A2_Data_Full)

flexplot(Neg_Mean~SPP_Mean | Con_Mean + sex, data=A2_Data_Full, method = "lm")

#Assumption check for full model (both hypotheses)
model <- lm(Neg_Mean~Con_Mean + sex + SPP_Mean, data=A2_Data_Full)
visualize(model, plot="residuals")


#General Linear Model; Hypothesis 1
#STEP 1: State hypotheses
#___Hypothesis 1: Sex, conscientiousness, and SPP will all significantly predict negative affect

#STEP 2: Formulate a linear model #Question: Does the order of predictors matter in this model? 
#___Neg_Mean = b0 + b1 x conscientiousness + b2 x sex + b3 x SPP_Mean + e

#STEP 3: Identify your parameter of interest
#___b1 x conscientiousness + b2 x sex + b3 x SPP_Mean

#STEP 4: Set up a full and a reduced model and compare them
#___Full model: Neg_Mean = b0 + b1 x conscientiousness + b2 x sex + b3 x SPP_Mean + e
#___Reduced model: Neg_Mean = b0 + e

Reduced <- lm(Neg_Mean~1, data=A2_Data_Full) 
Full <- lm(Neg_Mean~Con_Mean + sex + SPP_Mean, data=A2_Data_Full)
model.comparison(Reduced, Full) #This will only show up as an output if I run the line separately for some reason. 
estimates(Full)

#General Linear Model; Hypothesis 2
#---Hypothesis 2: SPP will predict unique variance in negative affect over and above sex and conscientiousness in a meaningful way

#STEP 2: Formulate a linear model #Question: Does the order of predictors matter in this model? 
#___Neg_Mean = b0 + b1 x conscientiousness + b2 x sex + b3 x SPP_Mean + e

#STEP 3: Identify your parameter of interest
#___b1 x SPP_Mean

#STEP 4: Set up a full and a reduced model and compare them
#___Full model: Neg_Mean = b0 + b1 x conscientiousness + b2 x sex + b3 x SPP_Mean + e
#___Reduced model: Neg_Mean = b0 + b1 x conscientiousness + b2 x sex + e

Reduced <- lm(Neg_Mean~Con_Mean + sex, data=A2_Data_Full) 
Full <- lm(Neg_Mean~Con_Mean + sex + SPP_Mean, data=A2_Data_Full)
model.comparison(Reduced, Full)
estimates(Full) 

#Bivariate correlations
DATA_A <-lm(Neg_Mean ~ Con_Mean + sex, data=A2_Data_Full)

is.recursive(Full)
is.atomic(Full)

A2_Data_cor <- select(A2_Data_Full, SPP_Mean, Con_Mean, Neg_Mean)

#Saving tables in APA (Didn't keep the code running so it didnt save to markers files, the tables are in write up)
#apa.reg.table(Full, filename = "Regression_Table_A2.doc")
#apa.cor.table(A2_Data_Full, filename = "Regression_Table_A2.doc")

#Additional code that I used for my results 
table(A2_Data_Full$sex)
