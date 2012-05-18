###########################################
#function to calculate the model error
###########################################
calc_error <- function(act,pred)
{
  aact <- as.matrix(act)
  ppred <- as.matrix(pred)
  
  if(nrow(aact) == nrow(ppred)){
    return (sqrt(colSums((log(ppred+1) - log(aact+1)) ^ 2) / nrow(aact)))
  } else {
    return (-99)
  }
  
}
#### EOF to calcualte model error ####


y3.actual <- read.csv('DaysInHospital_Y3.csv')
y3.prediction <- read.csv('GBM_demo1.csv')
names(y3.prediction)[2] = 'DaysInHospital.Prediction'

y3.merge <- merge (y3.actual, y3.prediction, by='MemberID')
calc_error(y3.merge$DaysInHospital, y3.merge$DaysInHospital.Prediction)
