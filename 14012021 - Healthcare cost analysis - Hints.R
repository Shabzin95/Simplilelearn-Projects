#######################################################################################

#======================================================================================
# How to document and submit??
#======================================================================================

# While submitting the project, one has to submit the below mentioned === MANDATORILY

# Writeup *      -- Explain the approach. Why are you doing what you are doing?
# Screenshots *  -- Share the codes
# Source Code *  -- Share the output

# The below format has all OF THESE. Work in this format, once done with the entire project,
# copy paste the entire document in a word document.

#======================================================================================
# UPLOAD THE WORD DOC IN ALL THE THREE MANDATORY FIELDS
#======================================================================================

#######################################################################################

rm(list=ls())

hops  <- read.csv('HospitalCosts.csv') 
head(hops) #This returns the first 6 records of the dataset

colSums(is.na(hops)) #Finds and adds NA values in each column

hops <- na.omit(hops) # deleting the NA values

colSums(is.na(hops))

# Attribute	      Description
# Age 	          Age of the patient discharged
# Female 	        A binary variable that indicates if the patient is female
# Los	            Length of stay in days
# Race 	          Race of the patient (specified numerically)
# Totchg	        Hospital discharge costs
# Aprdrg	        All Patient Refined Diagnosis Related Groups

str(hops) #Returns the structure of your dataset

hops$RACE <- as.factor(hops$RACE) #Changing RACE data type to a Factorial / Categorical Data type
hops$FEMALE <- as.factor(hops$FEMALE) #Changing FEMALE data type to a Factorial / Categorical Data type

str(hops)

levels(hops$RACE)
summary(hops$RACE)

# 1. To record the patient statistics, the agency wants to find the age 
# category of people who frequently visit the hospital and has the maximum expenditure.  

# a. To find the age category that has the highest frequency of hospital visit

# The as.factor() is called to make sure that the categories are not treated as numbers so
# we can look at the frequencies

summary(as.factor(hops$AGE)) # how many obserns do we have at each age?

# Output ::
#   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17 
# 307  10   1   3   2   2   2   3   2   2   4   8  15  18  25  29  29  38 

# Result: We can see that infants (AGE = 0) have the maximum 
# frequency of hospital visit, going above 300.

# b. To find the age category with the maximum expenditure

# Age TotChgAge

library(dplyr)

df <- summarise(group_by(hops, AGE), TotChgAge = sum(TOTCHG))
df

arrange(df, desc(TotChgAge))
arrange(df, desc(TotChgAge))[1,] # 1st row

# Output ::
#       AGE     TotChgAge
#      <int>        <int>
#   1     0        678118

# Result: From the result we can see that the infant category (AGE = 0) has maximum hospital costs 
# as well (in accordance with the number or frequency of visit). 

#######################################################################################
#######################################################################################

# 2. In order of severity of the diagnosis and treatments and to find out the expensive 
# treatments, the agency wants to find the diagnosis related group that has maximum 
# hospitalization and expenditure.

# Age == Aprdrg

# Similar to the previous analysis, we can find the diagnosis 
# related group with maximum hospitalization and expenditure. For this, we will use the summarise  
# and the arrange functions

df2 <- summarise(group_by(hops, AGE), AprgAge = sum(APRDRG))
df2

arrange(df2, desc(AprgAge))
arrange(df2, desc(AprgAge)) [1,]

# Output
# # A tibble: 1 x 2
# AGE AprgAge
# <int>   <int>
#   1     0  190260







#######################################################################################
#######################################################################################

# 3. To make sure that there is no malpractice, the agency needs to analyze if the 
# race of the patient is related to the hospitalization costs.

# If there is any effect of RACE on TOTCHG

# Then, to verify if the races made an impact on the costs, perform an ANOVA with the 
# following variables:  

# ANOVA variable: TOTCHG 
# Categorical/grouping variable: RACE Missing values: 1 NA value, use na.omit to remove the NA value   
# 

# Code:  

str(hops$RACE)
str(hops$TOTCHG)

model <- aov(TOTCHG ~ RACE, data = hops)  # numerical/int ~ categorical varibale
summary(model)

alpha = 0.05

pvalue = 0.943

pvalue < alpha # if this is true = whenever p_value is less than alpha; we reject the null hypothesis

# Result: We do not reject the null hypothesis. There is 
# no relation between the race of patient and the hospital cost.

#######################################################################################
#######################################################################################

# 4. To properly utilize the costs, the agency has to analyze the severity of the 
# hospital costs by age and gender for proper allocation of resources.  

#-----------------------------------------------------------------------------------------------------
# ANOVA is used to test relationship between one continuous and one or more categorical variable.
#-----------------------------------------------------------------------------------------------------

# av <- aov(dependent ~ independent1 + independent2, df)

# numerical/int ~ categorical varibale


model_2 <- aov(TOTCHG ~ RACE+FEMALE, data = hops)
summary(model_2)


alpha = 0.05
Race_pvalue = 0.943
Female_pvalue = 0.188

Race_pvalue < alpha # if this is true = whenever pvalue is less than alpha; we reject the null hypothesis
Output
#[1] FALSE

Female_pvalue < alpha # if this is true = whenever pvalue is less than alpha; we reject the null hypothesis
# Output
# [1] FALSE



#######################################################################################
#######################################################################################


# 5. Since, the length of stay is the crucial factor for inpatients, the agency 
# wants to find if the length of stay can be predicted from age, gender, and race.

str(hops)

# Since the length of stay is a qauntitative variable, we use linear regression to 
# predict the variable.  

# Dependent variable: LOS ;; Independent variables: AGE, FEMALE, RACE

# length of stay can be predicted from age, female, and race.

# Model3 <- lm(dependent ~ independent1 + independent2 + independent3, df)
# summary(Model3)

Model3 <- lm(LOS ~ AGE + FEMALE + RACE, data = hops)
summary(Model3)

# Coefficients:
#              pvalue    
# (Intercept)  <2e-16 ***
# AGE          0.0818 .  
# FEMALE1      0.2586    
# RACE2        0.7883    
# RACE3        0.8158    
# RACE4        0.7613    
# RACE5        0.6626    
# RACE6        0.7640 

# Result ::



#######################################################################################
#######################################################################################

# 6. To perform a complete analysis, the agency wants to find the variable that 
# mainly affects the hospital costs.


Model4 <- lm(TOTCHG ~ LOS + AGE + FEMALE + RACE + APRDRG, data = hops)
summary(Model4)
# Just check significance of the variables by comparing pvalue with alpha

#Result:
# Call:
#   lm(formula = TOTCHG ~ LOS + AGE + FEMALE + RACE + APRDRG, data = hops)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -6367   -691   -186    121  43412 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5024.9610   440.1366  11.417  < 2e-16 ***
#   LOS           742.9637    35.0464  21.199  < 2e-16 ***
#   AGE           133.2207    17.6662   7.541 2.29e-13 ***
#   FEMALE1      -392.5778   249.2981  -1.575    0.116    
# RACE2         458.2427  1085.2320   0.422    0.673    
# RACE3         330.5184  2629.5121   0.126    0.900    
# RACE4        -499.3818  1520.9293  -0.328    0.743    
# RACE5       -1784.5776  1532.0048  -1.165    0.245    
# RACE6        -594.2921  1859.1271  -0.320    0.749    
# APRDRG         -7.8175     0.6881 -11.361  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2622 on 489 degrees of freedom
# Multiple R-squared:  0.5544,	Adjusted R-squared:  0.5462 
# F-statistic:  67.6 on 9 and 489 DF,  p-value: < 2.2e-16

#Conclusion: 




