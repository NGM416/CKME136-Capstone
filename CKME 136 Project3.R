#--------------------------- open: activate libraries ------------------------
library("readr",lib.loc="C:/R")
library("tibble",lib.loc="C:/R")
library("Hmisc",lib.loc="C:/R")
library("psych",lib.loc="C:/R")
library("car",lib.loc="C:/R")
library("ggplot2", lib.loc="C:/R")
library("xts", lib.loc="C:/R")
library("reshape2", lib.loc="C:/R")
library("PerformanceAnalytics", lib.loc="C:/R")
library("Matrix", lib.loc="C:/R")
library("MASS", lib.loc="C:/R")
library("randomForest", lib.loc="C:/R")
library("leaps", lib.loc="C:/R")
#--------------------------- close: activate libraries ------------------------


#--------------------------- open: generage time series plots ------------------------

# load data and assign date type to date data

USData=read.csv("C:/Users/nmccl/Desktop/CKME project/USData.csv", header = TRUE, na.strings="" )
USData$Date = as.Date(USData$Date, "%m/%d/%Y")

# generate time series plots and write them to PDF

plot_list = list()
for(i in 3:116) {
GraphTitle = (colnames(USData)[i])
p = ggplot(data = USData[!is.na(USData[,i]),], aes_string(x=colnames(USData)[1], y=colnames(USData)[i])) + geom_line() + theme(axis.title.y=element_blank(), panel.border = element_rect("black", fill = NA, size =2))+ggtitle(GraphTitle)
plot_list[[i]] = p
}

pdf("C:/Users/nmccl/Desktop/CKME project/USDataTimeSeriesPlots.pdf")
for(i in 3:116) 
{
  print(plot_list[[i]])
}
dev.off()

#--------------------------- close: generage time series plots ------------------------


#--------------------------- open: generate correlation matrix ------------------------

USData_NoDates = USData[,3:length(USData)]   #to rid the data frame of non-data columns for correlation analysis
USDataCor = rcorr(as.matrix(USData_NoDates))

#function below puts all correlations into a table - too many to graph

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

USCorrOutput=flattenCorrMatrix(USDataCor$r, USDataCor$P)

# write correlation matrix values to a CSV file

write.csv(USCorrOutput,"C:/Users/nmccl/Desktop/CKME project/USDataCors.csv")

#--------------------------- close: generate correlation matrix ------------------------

#--------------------------- open: generate LINEAR regression models ------------------------

#read data
USData=read.csv("C:/Users/nmccl/Desktop/CKME project/USData.csv", header = TRUE, na.strings="" )
#format data as date
USData$Date = as.Date(USData$Date, "%m/%d/%Y")
#remove non regression columns
USData_NoDates = USData[,3:length(USData)]   #to rid the data frame of non-data columns for correlation analysis

USDataRegr = subset(USData_NoDates, select = c(US_GDP_Y______Gross_domestic_product__USD_M_,
                                               Personal_Income_Real__2012___USD_B_2012_,
                                               US_Population__000s_,
                                               USCPI_UrbanB8284__Index_8284_,
                                               Ttl_IndCapUtil__USD_M_,
                                               Total_Consumer_Revolving_Debt__USD_M_,
                                               US_HousingPrices_Base80__Index_80_,
                                               US_Employed__000s_,
                                               All_Mtgs__USD_M_,
                                               Import_Export_Price_All_Commodities__Index_2000_,
                                               US_MonetaryBase__USD_M_,
                                               SP500__Index_,
                                               LoanDel_C.I__Perc_,
                                               X13Mos_Libor_USD__Perc_,
                                               X10Year_Treasury_Rate__Perc_,
                                               Federal_funds_effective_rate__Index_8284_,
                                               X3Month_Tbill__Perc_,
                                               Prime_Rate__Perc_))

#keep only complete data for rows for regression analysis
USDataRegrClean = USDataRegr[complete.cases(USDataRegr),]
#isolate data to include non-interest values 
USDataRegrCleanNoInterest = USDataRegrClean[,1:13]
#review data without interest
summary(USDataRegrCleanNoInterest)
pairs.panels(USDataRegrCleanNoInterest, col = "red", cex.cor = 3)
#review data with interest
summary(USDataRegrClean)
pairs.panels(USDataRegrClean, col = "red", cex.cor = 2)
#divide data into training and validation
set.seed(2018)
training.size = 0.8
train.index=sample.int(nrow(USDataRegrCleanNoInterest), round(nrow(USDataRegrCleanNoInterest)* training.size))
#training data
train.USDataRegrCleanNoInterest = USDataRegrCleanNoInterest[train.index,]
View(train.USDataRegrCleanNoInterest)
#validation data
validate.USDataRegrCleanNoInterest = USDataRegrCleanNoInterest[-train.index,]
#select dependent variables for optimal model
USRegResults = regsubsets(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                            
                            US_Population__000s_ +
                            Personal_Income_Real__2012___USD_B_2012_ +
                            USCPI_UrbanB8284__Index_8284_ +
                            Ttl_IndCapUtil__USD_M_ +
                            Total_Consumer_Revolving_Debt__USD_M_ +
                            US_HousingPrices_Base80__Index_80_ +
                            US_Employed__000s_ +
                            All_Mtgs__USD_M_ +
                            Import_Export_Price_All_Commodities__Index_2000_ +
                            US_MonetaryBase__USD_M_ +
                            SP500__Index_ +
                            LoanDel_C.I__Perc_,
                          
                            data = train.USDataRegrCleanNoInterest, 
                            nbest =4)

#see summary and plot for optimal model
summary(USRegResults)
plot(USRegResults, scale="adjr2")

#--------------------------- open: generate TOP 5 LINEAR regression modelswithout interest data ------------------------
USRegrNoInt1 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                       
                       US_Population__000s_ +
                       USCPI_UrbanB8284__Index_8284_ +
                       Ttl_IndCapUtil__USD_M_ +
                       Total_Consumer_Revolving_Debt__USD_M_ +
                       US_HousingPrices_Base80__Index_80_ +
                       US_Employed__000s_ +
                       US_MonetaryBase__USD_M_ +
                       SP500__Index_,

                       data = train.USDataRegrCleanNoInterest)

train.USDataRegrCleanNoInterest$PredGDP1 = predict(USRegrNoInt1, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                    select = c(
                                                     
                                                     US_Population__000s_,
                                                     USCPI_UrbanB8284__Index_8284_,
                                                     Ttl_IndCapUtil__USD_M_,
                                                     Total_Consumer_Revolving_Debt__USD_M_,
                                                     US_HousingPrices_Base80__Index_80_,
                                                     US_Employed__000s_,
                                                     US_MonetaryBase__USD_M_,
                                                     SP500__Index_) ) )

validate.USDataRegrCleanNoInterest$PredGDP1 = predict(USRegrNoInt1, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                   select = c(
                                                     
                                                     US_Population__000s_,
                                                     USCPI_UrbanB8284__Index_8284_,
                                                     Ttl_IndCapUtil__USD_M_,
                                                     Total_Consumer_Revolving_Debt__USD_M_,
                                                     US_HousingPrices_Base80__Index_80_,
                                                     US_Employed__000s_,
                                                     US_MonetaryBase__USD_M_, 
                                                     SP500__Index_) ) )

summary(USRegrNoInt1)
train1.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDP1, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train1.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDP1 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train1.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDP1 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train1.corr^2, train1.RMSE, train1.MAE)

validate1.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDP1, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate1.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDP1 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate1.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDP1 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate1.corr^2, validate1.RMSE, validate1.MAE)
#----------------
USRegrNoInt2 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    Import_Export_Price_All_Commodities__Index_2000_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,

                    data = train.USDataRegrCleanNoInterest)

train.USDataRegrCleanNoInterest$PredGDP2 = predict(USRegrNoInt2, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    Import_Export_Price_All_Commodities__Index_2000_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_) ) )

validate.USDataRegrCleanNoInterest$PredGDP2 = predict(USRegrNoInt2, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       Import_Export_Price_All_Commodities__Index_2000_,
                                                                                       US_MonetaryBase__USD_M_,
                                                                                       SP500__Index_) ) )

summary(USRegrNoInt2)
train2.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDP2, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train2.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDP2 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train2.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDP2 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train2.corr^2, train2.RMSE, train2.MAE)

validate2.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDP2, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate2.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDP2 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate2.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDP2 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate2.corr^2, validate2.RMSE, validate2.MAE)
#----------------
USRegrNoInt3 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_ +
                    LoanDel_C.I__Perc_, 
                  
                  data = train.USDataRegrCleanNoInterest)

train.USDataRegrCleanNoInterest$PredGDP3 = predict(USRegrNoInt3, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_,
                                                                                    LoanDel_C.I__Perc_) ) )

validate.USDataRegrCleanNoInterest$PredGDP3 = predict(USRegrNoInt3, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       US_MonetaryBase__USD_M_,
                                                                                       SP500__Index_,
                                                                                       LoanDel_C.I__Perc_) ) )

summary(USRegrNoInt3)
train3.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDP3, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train3.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDP3 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train3.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDP3 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train3.corr^2, train3.RMSE, train3.MAE)

validate3.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDP3, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate3.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDP3 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate3.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDP3 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate3.corr^2, validate3.RMSE, validate3.MAE)
#----------------
USRegrNoInt4 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,

                  data = train.USDataRegrCleanNoInterest)

train.USDataRegrCleanNoInterest$PredGDP4 = predict(USRegrNoInt4, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_) ) )

validate.USDataRegrCleanNoInterest$PredGDP4 = predict(USRegrNoInt4, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       US_MonetaryBase__USD_M_, 
                                                                                       SP500__Index_) ) )

summary(USRegrNoInt4)
train4.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDP4, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train4.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDP4 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train4.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDP4 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train4.corr^2, train4.RMSE, train4.MAE)

validate4.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDP4, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate4.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDP4 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate4.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDP4 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate4.corr^2, validate4.RMSE, validate4.MAE)

#----------------
USRegrNoInt5 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    All_Mtgs__USD_M_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,

                  data = train.USDataRegrCleanNoInterest)

train.USDataRegrCleanNoInterest$PredGDP5 = predict(USRegrNoInt5, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    All_Mtgs__USD_M_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_) ) )

validate.USDataRegrCleanNoInterest$PredGDP5 = predict(USRegrNoInt5, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       All_Mtgs__USD_M_,
                                                                                       US_MonetaryBase__USD_M_,
                                                                                       SP500__Index_) ) )

summary(USRegrNoInt5)
train5.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDP5, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train5.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDP5 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train5.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDP5 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train5.corr^2, train5.RMSE, train5.MAE)

validate5.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDP5, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate5.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDP5 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate5.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDP5 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate5.corr^2, validate5.RMSE, validate5.MAE)
#--------------------------- close: generate TOP 5 LINEAR regression models without interest data ------------------------

#--------------------------- open: generate TOP 5 LINEAR regression models with interest data ------------------------

#divide data into training and validation
set.seed(2018)
training.size = 0.8
train.index=sample.int(nrow(USDataRegrClean), round(nrow(USDataRegrClean)* training.size))
#training data
train.USDataRegrClean = USDataRegrClean[train.index,]
View(train.USDataRegrClean)
#validation data
validate.USDataRegrClean = USDataRegrClean[-train.index,]
#select dependent variables for optimal model
USRegResultsInt = regsubsets(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                            
                            US_Population__000s_ +
                            Personal_Income_Real__2012___USD_B_2012_ +
                            USCPI_UrbanB8284__Index_8284_ +
                            Ttl_IndCapUtil__USD_M_ +
                            Total_Consumer_Revolving_Debt__USD_M_ +
                            US_HousingPrices_Base80__Index_80_ +
                            US_Employed__000s_ +
                            All_Mtgs__USD_M_ +
                            Import_Export_Price_All_Commodities__Index_2000_ +
                            US_MonetaryBase__USD_M_ +
                            SP500__Index_ +
                            LoanDel_C.I__Perc_ +
                            X13Mos_Libor_USD__Perc_ +
                            X10Year_Treasury_Rate__Perc_ +
                            Federal_funds_effective_rate__Index_8284_ +
                            X3Month_Tbill__Perc_ +
                            Prime_Rate__Perc_,
                          
                          data = train.USDataRegrClean, 
                          nbest =4)

#see summary and plot for optimal model
summary(USRegResultsInt)
plot(USRegResultsInt, scale="adjr2")

#run top 5 regression models
USRegrInt1 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    Ttl_IndCapUtil__USD_M_ +
                    Total_Consumer_Revolving_Debt__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_ +
                    X3Month_Tbill__Perc_,
                  
                    data = train.USDataRegrClean)

train.USDataRegrClean$PredGDP1 = predict(USRegrInt1, newdata = subset(train.USDataRegrClean, 
                                                                        select = c(
                                                                          
                                                                          US_Population__000s_,
                                                                          Ttl_IndCapUtil__USD_M_,
                                                                          Total_Consumer_Revolving_Debt__USD_M_,
                                                                          US_HousingPrices_Base80__Index_80_,
                                                                          US_Employed__000s_,
                                                                          US_MonetaryBase__USD_M_,
                                                                          SP500__Index_,
                                                                          X3Month_Tbill__Perc_) ) )

validate.USDataRegrClean$PredGDP1 = predict(USRegrInt1, newdata = subset(validate.USDataRegrClean, 
                                                                           select = c(
                                                                             
                                                                             US_Population__000s_,
                                                                             Ttl_IndCapUtil__USD_M_,
                                                                             Total_Consumer_Revolving_Debt__USD_M_,
                                                                             US_HousingPrices_Base80__Index_80_,
                                                                             US_Employed__000s_,
                                                                             US_MonetaryBase__USD_M_,
                                                                             SP500__Index_,
                                                                             X3Month_Tbill__Perc_) ) )

summary(USRegrInt1)
train1int.corr = round(cor(train.USDataRegrClean$PredGDP1, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train1int.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDP1 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train1int.MAE = round(mean(abs(train.USDataRegrClean$PredGDP1 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train1int.corr^2, train1int.RMSE, train1int.MAE)

validate1int.corr = round(cor(validate.USDataRegrClean$PredGDP1, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate1int.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDP1 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate1int.MAE = round(mean(abs(validate.USDataRegrClean$PredGDP1 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate1int.corr^2, validate1int.RMSE, validate1int.MAE)
#----------------
USRegrInt2 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    Total_Consumer_Revolving_Debt__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,
                  
                  data = train.USDataRegrClean)

train.USDataRegrClean$PredGDP2 = predict(USRegrInt2, newdata = subset(train.USDataRegrClean, 
                                                                        select = c(
                                                                          
                                                                          US_Population__000s_,
                                                                          USCPI_UrbanB8284__Index_8284_,
                                                                          Ttl_IndCapUtil__USD_M_,
                                                                          Total_Consumer_Revolving_Debt__USD_M_,
                                                                          US_HousingPrices_Base80__Index_80_,
                                                                          US_Employed__000s_,
                                                                          US_MonetaryBase__USD_M_,
                                                                          SP500__Index_) ) )

validate.USDataRegrClean$PredGDP2 = predict(USRegrInt2, newdata = subset(validate.USDataRegrClean, 
                                                                           select = c(
                                                                             
                                                                             US_Population__000s_,
                                                                             USCPI_UrbanB8284__Index_8284_,
                                                                             Ttl_IndCapUtil__USD_M_,
                                                                             Total_Consumer_Revolving_Debt__USD_M_,
                                                                             US_HousingPrices_Base80__Index_80_,
                                                                             US_Employed__000s_,
                                                                             US_MonetaryBase__USD_M_,
                                                                             SP500__Index_) ) )

summary(USRegrInt2)
train2int.corr = round(cor(train.USDataRegrClean$PredGDP2, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train2int.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDP2 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train2int.MAE = round(mean(abs(train.USDataRegrClean$PredGDP2 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train2int.corr^2, train2int.RMSE, train2int.MAE)

validate2int.corr = round(cor(validate.USDataRegrClean$PredGDP2, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate2int.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDP2 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate2int.MAE = round(mean(abs(validate.USDataRegrClean$PredGDP2 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate2int.corr^2, validate2int.RMSE, validate2int.MAE)
#----------------
USRegrInt3 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    Ttl_IndCapUtil__USD_M_ +
                    Total_Consumer_Revolving_Debt__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_ +
                    Federal_funds_effective_rate__Index_8284_, 
                  
                  data = train.USDataRegrClean)

train.USDataRegrClean$PredGDP3 = predict(USRegrInt3, newdata = subset(train.USDataRegrClean, 
                                                                        select = c(
                                                                          
                                                                          US_Population__000s_,
                                                                          Ttl_IndCapUtil__USD_M_,
                                                                          Total_Consumer_Revolving_Debt__USD_M_,
                                                                          US_HousingPrices_Base80__Index_80_,
                                                                          US_Employed__000s_,
                                                                          US_MonetaryBase__USD_M_,
                                                                          SP500__Index_,
                                                                          Federal_funds_effective_rate__Index_8284_) ) )

validate.USDataRegrClean$PredGDP3 = predict(USRegrInt3, newdata = subset(validate.USDataRegrClean, 
                                                                           select = c(
                                                                             
                                                                             US_Population__000s_,
                                                                             Ttl_IndCapUtil__USD_M_,
                                                                             Total_Consumer_Revolving_Debt__USD_M_,
                                                                             US_HousingPrices_Base80__Index_80_,
                                                                             US_Employed__000s_,
                                                                             US_MonetaryBase__USD_M_,
                                                                             SP500__Index_,
                                                                             Federal_funds_effective_rate__Index_8284_) ) )

summary(USRegrInt3)
train3int.corr = round(cor(train.USDataRegrClean$PredGDP3, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train3int.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDP3 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train3int.MAE = round(mean(abs(train.USDataRegrClean$PredGDP3 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train3int.corr^2, train3int.RMSE, train3int.MAE)

validate3int.corr = round(cor(validate.USDataRegrClean$PredGDP3, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate3int.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDP3 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate3int.MAE = round(mean(abs(validate.USDataRegrClean$PredGDP3 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate3int.corr^2, validate3int.RMSE, validate3int.MAE)
#----------------
USRegrInt4 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    Ttl_IndCapUtil__USD_M_ +
                    Total_Consumer_Revolving_Debt__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_ +
                    Prime_Rate__Perc_,
                  
                  data = train.USDataRegrClean)

train.USDataRegrClean$PredGDP4 = predict(USRegrInt4, newdata = subset(train.USDataRegrClean, 
                                                                        select = c(
                                                                          
                                                                          US_Population__000s_,
                                                                          Ttl_IndCapUtil__USD_M_,
                                                                          Total_Consumer_Revolving_Debt__USD_M_,
                                                                          US_HousingPrices_Base80__Index_80_,
                                                                          US_Employed__000s_,
                                                                          US_MonetaryBase__USD_M_,
                                                                          SP500__Index_,
                                                                          Prime_Rate__Perc_) ) )

validate.USDataRegrClean$PredGDP4 = predict(USRegrInt4, newdata = subset(validate.USDataRegrClean, 
                                                                           select = c(
                                                                             
                                                                             US_Population__000s_,
                                                                             Ttl_IndCapUtil__USD_M_,
                                                                             Total_Consumer_Revolving_Debt__USD_M_,
                                                                             US_HousingPrices_Base80__Index_80_,
                                                                             US_Employed__000s_,
                                                                             US_MonetaryBase__USD_M_,
                                                                             SP500__Index_,
                                                                             Prime_Rate__Perc_) ) )

summary(USRegrInt4)
train4int.corr = round(cor(train.USDataRegrClean$PredGDP4, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train4int.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDP4 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train4int.MAE = round(mean(abs(train.USDataRegrClean$PredGDP4 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train4int.corr^2, train4int.RMSE, train4int.MAE)

validate4int.corr = round(cor(validate.USDataRegrClean$PredGDP4, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate4int.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDP4 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate4int.MAE = round(mean(abs(validate.USDataRegrClean$PredGDP4 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate4int.corr^2, validate4int.RMSE, validate4int.MAE)

#----------------
USRegrInt5 = lm(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    All_Mtgs__USD_M_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,
                  
                  data = train.USDataRegrClean)

train.USDataRegrClean$PredGDP5 = predict(USRegrInt5, newdata = subset(train.USDataRegrClean, 
                                                                        select = c(
                                                                          
                                                                          US_Population__000s_,
                                                                          USCPI_UrbanB8284__Index_8284_,
                                                                          Ttl_IndCapUtil__USD_M_,
                                                                          US_HousingPrices_Base80__Index_80_,
                                                                          US_Employed__000s_,
                                                                          All_Mtgs__USD_M_,
                                                                          US_MonetaryBase__USD_M_,
                                                                          SP500__Index_) ) )

validate.USDataRegrClean$PredGDP5 = predict(USRegrInt5, newdata = subset(validate.USDataRegrClean, 
                                                                           select = c(
                                                                             
                                                                             US_Population__000s_,
                                                                             USCPI_UrbanB8284__Index_8284_,
                                                                             Ttl_IndCapUtil__USD_M_,
                                                                             US_HousingPrices_Base80__Index_80_,
                                                                             US_Employed__000s_,
                                                                             All_Mtgs__USD_M_,
                                                                             US_MonetaryBase__USD_M_,
                                                                             SP500__Index_) ) )

summary(USRegrInt5)
train5int.corr = round(cor(train.USDataRegrClean$PredGDP5, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train5int.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDP5 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train5int.MAE = round(mean(abs(train.USDataRegrClean$PredGDP5 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train5int.corr^2, train5int.RMSE, train5int.MAE)

validate5int.corr = round(cor(validate.USDataRegrClean$PredGDP5, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate5int.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDP5 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate5int.MAE = round(mean(abs(validate.USDataRegrClean$PredGDP5 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate5int.corr^2, validate5int.RMSE, validate5int.MAE)
#--------------------------- close: generate TOP 5 LINEAR regression models with interest data ------------------------


#--------------------------- open: generate TOP 5 RANDOM FOREST regression models without interest data ------------------------

USRFNoInt1 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    Total_Consumer_Revolving_Debt__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,
                  
                    data = train.USDataRegrCleanNoInterest,
                    type = regression)

train.USDataRegrCleanNoInterest$PredGDPRF1 = predict(USRFNoInt1, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    Total_Consumer_Revolving_Debt__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_) ) )

validate.USDataRegrCleanNoInterest$PredGDPRF1 = predict(USRFNoInt1, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       Total_Consumer_Revolving_Debt__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       US_MonetaryBase__USD_M_, 
                                                                                       SP500__Index_) ) )

summary(USRFNoInt1)
train1RF.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDPRF1, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train1RF.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDPRF1 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train1RF.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDPRF1 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train1RF.corr^2, train1RF.RMSE, train1RF.MAE)

validate1RF.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDPRF1, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate1RF.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDPRF1 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate1RF.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDPRF1 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate1RF.corr^2, validate1RF.RMSE, validate1RF.MAE)

#----------------
USRFNoInt2 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    Import_Export_Price_All_Commodities__Index_2000_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,
                  
                    data = train.USDataRegrCleanNoInterest,
                    type = regression)

train.USDataRegrCleanNoInterest$PredGDPRF2 = predict(USRFNoInt2, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    Import_Export_Price_All_Commodities__Index_2000_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_) ) )

validate.USDataRegrCleanNoInterest$PredGDPRF2 = predict(USRFNoInt2, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       Import_Export_Price_All_Commodities__Index_2000_,
                                                                                       US_MonetaryBase__USD_M_,
                                                                                       SP500__Index_) ) )

summary(USRFNoInt2)
train2RF.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDPRF2, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train2RF.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDPRF2 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train2RF.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDPRF2 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train2RF.corr^2, train2RF.RMSE, train2RF.MAE)

validate2RF.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDPRF2, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate2RF.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDPRF2 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate2RF.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDPRF2 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate2RF.corr^2, validate2RF.RMSE, validate2RF.MAE)
#----------------
USRFNoInt3 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_ +
                    LoanDel_C.I__Perc_, 
                  
                    data = train.USDataRegrCleanNoInterest,
                    type = regression)

train.USDataRegrCleanNoInterest$PredGDPRF3 = predict(USRFNoInt3, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_,
                                                                                    LoanDel_C.I__Perc_) ) )

validate.USDataRegrCleanNoInterest$PredGDPRF3 = predict(USRFNoInt3, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       US_MonetaryBase__USD_M_,
                                                                                       SP500__Index_,
                                                                                       LoanDel_C.I__Perc_) ) )

summary(USRFNoInt3)
train3RF.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDPRF3, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train3RF.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDPRF3 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train3RF.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDPRF3 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train3RF.corr^2, train3RF.RMSE, train3RF.MAE)

validate3RF.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDPRF3, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate3RF.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDPRF3 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate3RF.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDPRF3 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate3RF.corr^2, validate3RF.RMSE, validate3RF.MAE)
#----------------
USRFNoInt4 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,
                  
                    data = train.USDataRegrCleanNoInterest,
                    type = regression)

train.USDataRegrCleanNoInterest$PredGDPRF4 = predict(USRFNoInt4, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_) ) )

validate.USDataRegrCleanNoInterest$PredGDPRF4 = predict(USRFNoInt4, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       US_MonetaryBase__USD_M_, 
                                                                                       SP500__Index_) ) )

summary(USRFNoInt4)
train4RF.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDPRF4, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train4RF.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDPRF4 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train4RF.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDPRF4 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train4RF.corr^2, train4RF.RMSE, train4RF.MAE)

validate4RF.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDPRF4, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate4RF.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDPRF4 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate4RF.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDPRF4 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate4RF.corr^2, validate4RF.RMSE, validate4RF.MAE)

#----------------
USRFNoInt5 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                    
                    US_Population__000s_ +
                    USCPI_UrbanB8284__Index_8284_ +
                    Ttl_IndCapUtil__USD_M_ +
                    US_HousingPrices_Base80__Index_80_ +
                    US_Employed__000s_ +
                    All_Mtgs__USD_M_ +
                    US_MonetaryBase__USD_M_ +
                    SP500__Index_,
                  
                    data = train.USDataRegrCleanNoInterest,
                    type = regression)

train.USDataRegrCleanNoInterest$PredGDPRF5 = predict(USRFNoInt5, newdata = subset(train.USDataRegrCleanNoInterest, 
                                                                                  select = c(
                                                                                    
                                                                                    US_Population__000s_,
                                                                                    USCPI_UrbanB8284__Index_8284_,
                                                                                    Ttl_IndCapUtil__USD_M_,
                                                                                    US_HousingPrices_Base80__Index_80_,
                                                                                    US_Employed__000s_,
                                                                                    All_Mtgs__USD_M_,
                                                                                    US_MonetaryBase__USD_M_,
                                                                                    SP500__Index_) ) )

validate.USDataRegrCleanNoInterest$PredGDPRF5 = predict(USRFNoInt5, newdata = subset(validate.USDataRegrCleanNoInterest, 
                                                                                     select = c(
                                                                                       
                                                                                       US_Population__000s_,
                                                                                       USCPI_UrbanB8284__Index_8284_,
                                                                                       Ttl_IndCapUtil__USD_M_,
                                                                                       US_HousingPrices_Base80__Index_80_,
                                                                                       US_Employed__000s_,
                                                                                       All_Mtgs__USD_M_,
                                                                                       US_MonetaryBase__USD_M_,
                                                                                       SP500__Index_) ) )

summary(USRFNoInt5)
train5RF.corr = round(cor(train.USDataRegrCleanNoInterest$PredGDPRF5, train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train5RF.RMSE = round(sqrt(mean((train.USDataRegrCleanNoInterest$PredGDPRF5 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train5RF.MAE = round(mean(abs(train.USDataRegrCleanNoInterest$PredGDPRF5 - train.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train5RF.corr^2, train5RF.RMSE, train5RF.MAE)

validate5RF.corr = round(cor(validate.USDataRegrCleanNoInterest$PredGDPRF5, validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate5RF.RMSE = round(sqrt(mean((validate.USDataRegrCleanNoInterest$PredGDPRF5 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate5RF.MAE = round(mean(abs(validate.USDataRegrCleanNoInterest$PredGDPRF5 - validate.USDataRegrCleanNoInterest$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate5RF.corr^2, validate5RF.RMSE, validate5RF.MAE)

#--------------------------- close: generate TOP 5 RANDOM FOREST regression models without interest data ------------------------

#--------------------------- open: generate TOP 5 RANDOM FOREST regression models with interest data ------------------------

#run top 5 regression models
USRFInt1 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                  
                  US_Population__000s_ +
                  Ttl_IndCapUtil__USD_M_ +
                  Total_Consumer_Revolving_Debt__USD_M_ +
                  US_HousingPrices_Base80__Index_80_ +
                  US_Employed__000s_ +
                  US_MonetaryBase__USD_M_ +
                  SP500__Index_ +
                  X3Month_Tbill__Perc_,
                
                  data = train.USDataRegrClean,
                  type = regression)

train.USDataRegrClean$PredGDPRF1 = predict(USRFInt1, newdata = subset(train.USDataRegrClean, 
                                                                      select = c(
                                                                        
                                                                        US_Population__000s_,
                                                                        Ttl_IndCapUtil__USD_M_,
                                                                        Total_Consumer_Revolving_Debt__USD_M_,
                                                                        US_HousingPrices_Base80__Index_80_,
                                                                        US_Employed__000s_,
                                                                        US_MonetaryBase__USD_M_,
                                                                        SP500__Index_,
                                                                        X3Month_Tbill__Perc_) ) )

validate.USDataRegrClean$PredGDPRF1 = predict(USRFInt1, newdata = subset(validate.USDataRegrClean, 
                                                                         select = c(
                                                                           
                                                                           US_Population__000s_,
                                                                           Ttl_IndCapUtil__USD_M_,
                                                                           Total_Consumer_Revolving_Debt__USD_M_,
                                                                           US_HousingPrices_Base80__Index_80_,
                                                                           US_Employed__000s_,
                                                                           US_MonetaryBase__USD_M_,
                                                                           SP500__Index_,
                                                                           X3Month_Tbill__Perc_) ) )

summary(USRFInt1)
train1RFint.corr = round(cor(train.USDataRegrClean$PredGDPRF1, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train1RFint.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDPRF1 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train1RFint.MAE = round(mean(abs(train.USDataRegrClean$PredGDPRF1 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train1RFint.corr^2, train1RFint.RMSE, train1RFint.MAE)

validate1int.corr = round(cor(validate.USDataRegrClean$PredGDP1, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate1int.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDP1 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate1int.MAE = round(mean(abs(validate.USDataRegrClean$PredGDP1 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate1int.corr^2, validate1int.RMSE, validate1int.MAE)
#----------------
USRFInt2 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                  
                  US_Population__000s_ +
                  USCPI_UrbanB8284__Index_8284_ +
                  Ttl_IndCapUtil__USD_M_ +
                  Total_Consumer_Revolving_Debt__USD_M_ +
                  US_HousingPrices_Base80__Index_80_ +
                  US_Employed__000s_ +
                  US_MonetaryBase__USD_M_ +
                  SP500__Index_,
                
                  data = train.USDataRegrClean,
                  type = regression)

train.USDataRegrClean$PredGDPRF2 = predict(USRFInt2, newdata = subset(train.USDataRegrClean, 
                                                                      select = c(
                                                                        
                                                                        US_Population__000s_,
                                                                        USCPI_UrbanB8284__Index_8284_,
                                                                        Ttl_IndCapUtil__USD_M_,
                                                                        Total_Consumer_Revolving_Debt__USD_M_,
                                                                        US_HousingPrices_Base80__Index_80_,
                                                                        US_Employed__000s_,
                                                                        US_MonetaryBase__USD_M_,
                                                                        SP500__Index_) ) )

validate.USDataRegrClean$PredGDPRF2 = predict(USRFInt2, newdata = subset(validate.USDataRegrClean, 
                                                                         select = c(
                                                                           
                                                                           US_Population__000s_,
                                                                           USCPI_UrbanB8284__Index_8284_,
                                                                           Ttl_IndCapUtil__USD_M_,
                                                                           Total_Consumer_Revolving_Debt__USD_M_,
                                                                           US_HousingPrices_Base80__Index_80_,
                                                                           US_Employed__000s_,
                                                                           US_MonetaryBase__USD_M_,
                                                                           SP500__Index_) ) )

summary(USRFInt2)
train2RFint.corr = round(cor(train.USDataRegrClean$PredGDPRF2, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train2RFint.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDPRF2 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train2RFint.MAE = round(mean(abs(train.USDataRegrClean$PredGDPRF2 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train2RFint.corr^2, train2RFint.RMSE, train2RFint.MAE)

validate2RFint.corr = round(cor(validate.USDataRegrClean$PredGDPRF2, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate2RFint.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDPRF2 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate2RFint.MAE = round(mean(abs(validate.USDataRegrClean$PredGDPRF2 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate2RFint.corr^2, validate2RFint.RMSE, validate2RFint.MAE)
#----------------
USRFInt3 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                  
                  US_Population__000s_ +
                  Ttl_IndCapUtil__USD_M_ +
                  Total_Consumer_Revolving_Debt__USD_M_ +
                  US_HousingPrices_Base80__Index_80_ +
                  US_Employed__000s_ +
                  US_MonetaryBase__USD_M_ +
                  SP500__Index_ +
                  Federal_funds_effective_rate__Index_8284_, 
                
                  data = train.USDataRegrClean,
                  type = regression)

train.USDataRegrClean$PredGDPRF3 = predict(USRFInt3, newdata = subset(train.USDataRegrClean, 
                                                                      select = c(
                                                                        
                                                                        US_Population__000s_,
                                                                        Ttl_IndCapUtil__USD_M_,
                                                                        Total_Consumer_Revolving_Debt__USD_M_,
                                                                        US_HousingPrices_Base80__Index_80_,
                                                                        US_Employed__000s_,
                                                                        US_MonetaryBase__USD_M_,
                                                                        SP500__Index_,
                                                                        Federal_funds_effective_rate__Index_8284_) ) )

validate.USDataRegrClean$PredGDPRF3 = predict(USRFInt3, newdata = subset(validate.USDataRegrClean, 
                                                                         select = c(
                                                                           
                                                                           US_Population__000s_,
                                                                           Ttl_IndCapUtil__USD_M_,
                                                                           Total_Consumer_Revolving_Debt__USD_M_,
                                                                           US_HousingPrices_Base80__Index_80_,
                                                                           US_Employed__000s_,
                                                                           US_MonetaryBase__USD_M_,
                                                                           SP500__Index_,
                                                                           Federal_funds_effective_rate__Index_8284_) ) )

summary(USRFInt3)
train3RFint.corr = round(cor(train.USDataRegrClean$PredGDPRF3, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train3RFint.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDPRF3 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train3RFint.MAE = round(mean(abs(train.USDataRegrClean$PredGDPRF3 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train3RFint.corr^2, train3RFint.RMSE, train3RFint.MAE)

validate3RFint.corr = round(cor(validate.USDataRegrClean$PredGDPRF3, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate3RFint.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDPRF3 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate3RFint.MAE = round(mean(abs(validate.USDataRegrClean$PredGDPRF3 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate3RFint.corr^2, validate3RFint.RMSE, validate3RFint.MAE)
#----------------
USRFInt4 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                  
                  US_Population__000s_ +
                  Ttl_IndCapUtil__USD_M_ +
                  Total_Consumer_Revolving_Debt__USD_M_ +
                  US_HousingPrices_Base80__Index_80_ +
                  US_Employed__000s_ +
                  US_MonetaryBase__USD_M_ +
                  SP500__Index_ +
                  Prime_Rate__Perc_,
                
                  data = train.USDataRegrClean,
                  type = regression)

train.USDataRegrClean$PredGDPRF4 = predict(USRFInt4, newdata = subset(train.USDataRegrClean, 
                                                                      select = c(
                                                                        
                                                                        US_Population__000s_,
                                                                        Ttl_IndCapUtil__USD_M_,
                                                                        Total_Consumer_Revolving_Debt__USD_M_,
                                                                        US_HousingPrices_Base80__Index_80_,
                                                                        US_Employed__000s_,
                                                                        US_MonetaryBase__USD_M_,
                                                                        SP500__Index_,
                                                                        Prime_Rate__Perc_) ) )

validate.USDataRegrClean$PredGDPRF4 = predict(USRFInt4, newdata = subset(validate.USDataRegrClean, 
                                                                         select = c(
                                                                           
                                                                           US_Population__000s_,
                                                                           Ttl_IndCapUtil__USD_M_,
                                                                           Total_Consumer_Revolving_Debt__USD_M_,
                                                                           US_HousingPrices_Base80__Index_80_,
                                                                           US_Employed__000s_,
                                                                           US_MonetaryBase__USD_M_,
                                                                           SP500__Index_,
                                                                           Prime_Rate__Perc_) ) )

summary(USRFInt4)
train4RFint.corr = round(cor(train.USDataRegrClean$PredGDPRF4, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train4RFint.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDPRF4 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train4RFint.MAE = round(mean(abs(train.USDataRegrClean$PredGDPRF4 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train4RFint.corr^2, train4RFint.RMSE, train4RFint.MAE)

validate4RFint.corr = round(cor(validate.USDataRegrClean$PredGDPRF4, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate4RFint.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDPRF4 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate4RFint.MAE = round(mean(abs(validate.USDataRegrClean$PredGDPRF4 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate4RFint.corr^2, validate4RFint.RMSE, validate4RFint.MAE)

#----------------
USRFInt5 = randomForest(US_GDP_Y______Gross_domestic_product__USD_M_ ~ 
                  
                  US_Population__000s_ +
                  USCPI_UrbanB8284__Index_8284_ +
                  Ttl_IndCapUtil__USD_M_ +
                  US_HousingPrices_Base80__Index_80_ +
                  US_Employed__000s_ +
                  All_Mtgs__USD_M_ +
                  US_MonetaryBase__USD_M_ +
                  SP500__Index_,
                
                  data = train.USDataRegrClean,
                  type = regression)

train.USDataRegrClean$PredGDPRF5 = predict(USRFInt5, newdata = subset(train.USDataRegrClean, 
                                                                      select = c(
                                                                        
                                                                        US_Population__000s_,
                                                                        USCPI_UrbanB8284__Index_8284_,
                                                                        Ttl_IndCapUtil__USD_M_,
                                                                        US_HousingPrices_Base80__Index_80_,
                                                                        US_Employed__000s_,
                                                                        All_Mtgs__USD_M_,
                                                                        US_MonetaryBase__USD_M_,
                                                                        SP500__Index_) ) )

validate.USDataRegrClean$PredGDPRF5 = predict(USRFInt5, newdata = subset(validate.USDataRegrClean, 
                                                                         select = c(
                                                                           
                                                                           US_Population__000s_,
                                                                           USCPI_UrbanB8284__Index_8284_,
                                                                           Ttl_IndCapUtil__USD_M_,
                                                                           US_HousingPrices_Base80__Index_80_,
                                                                           US_Employed__000s_,
                                                                           All_Mtgs__USD_M_,
                                                                           US_MonetaryBase__USD_M_,
                                                                           SP500__Index_) ) )

summary(USRFInt5)
train5RFint.corr = round(cor(train.USDataRegrClean$PredGDPRF5, train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
train5RFint.RMSE = round(sqrt(mean((train.USDataRegrClean$PredGDPRF5 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
train5RFint.MAE = round(mean(abs(train.USDataRegrClean$PredGDPRF5 - train.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(train5RFint.corr^2, train5RFint.RMSE, train5RFint.MAE)

validate5RFint.corr = round(cor(validate.USDataRegrClean$PredGDPRF5, validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_),2)
validate5RFint.RMSE = round(sqrt(mean((validate.USDataRegrClean$PredGDPRF5 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)^2)))
validate5RFint.MAE = round(mean(abs(validate.USDataRegrClean$PredGDPRF5 - validate.USDataRegrClean$US_GDP_Y______Gross_domestic_product__USD_M_)))
c(validate5RFint.corr^2, validate5RFint.RMSE, validate5RFint.MAE)

#--------------------------- close: generate TOP 5 RANDOM FOREST regression models with interest data ------------------------



