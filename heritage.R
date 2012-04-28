########################################
# Example GBM model for HHP
# scores ~ 0.4635 on leaderboard
# which would be 55th position of 510
# as at 9th Sept 2011
#
# Requires the data having been prepared
# using the SQL supplied
#
########################################

starttime <- proc.time()

########################################
#load the data
########################################

alldata <- read.csv("modeling_set1.csv")


########################################
# arrange the data
########################################
#work around a weird behavior, MemberID_t becomes X.MemberID_t after loading somehow
colnames(alldata)[1]='MemberID_t'

#identify train and leaderboard data
trainrows <- which(alldata$trainset == 1)
scorerows <- which(alldata$trainset == 0)

#sanity check the size of each set
length(trainrows)
length(scorerows)

#display the column names
colnames(alldata)

#memberid is required as key for submission set
memberid <- alldata[scorerows,'MemberID_t']

#remove redundant fields
alldata$MemberID_t <- NULL
alldata$YEAR_t <- NULL
alldata$trainset <- NULL

#target - what we are predicting
theTarget <- 'DaysInHospital'

#put the target on the log scale
alldata[trainrows,theTarget] <- log1p(alldata[trainrows,theTarget]) 

#find the position of the target
targindex <-  which(names(alldata)==theTarget)


########################################
# build the model
########################################

#GBM model settings, these can be varied
GBM_NTREES = 500
GBM_SHRINKAGE = 0.05
GBM_DEPTH = 4
GBM_MINOBS = 50

#build the GBM model
library(gbm)
GBM_model <- gbm.fit(
             x = alldata[trainrows,-targindex]
            ,y = alldata[trainrows,targindex]
            ,distribution = "gaussian"
            ,n.trees = GBM_NTREES
            ,shrinkage = GBM_SHRINKAGE
            ,interaction.depth = GBM_DEPTH
            ,n.minobsinnode = GBM_MINOBS
            ,verbose = TRUE) 

#list variable importance
summary(GBM_model,GBM_NTREES)

#predict for the leaderboard data
prediction <- predict.gbm(object = GBM_model
              ,newdata = alldata[scorerows,-targindex]
              ,GBM_NTREES)

#put on correct scale and cap
prediction <- expm1(prediction)
prediction <- pmin(15,prediction)
prediction <- pmax(0,prediction)

#plot the submission distribution
hist(prediction, breaks=500)


########################################
#write the submission to file
########################################
submission <- cbind(memberid,prediction)
colnames(submission) <- c("MemberID","DaysInHospital")
fnname <- "GBM_demo1.csv"
write.csv(submission, file=fnname, row.names = FALSE)

elapsedtime <- proc.time() - starttime
cat("\nFinished\n",elapsedtime)
