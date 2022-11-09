#read in plot data and metrics
metrics_all.m<-read.csv("output/metrics_all_m.csv")

#We have a few options for building an AGB model with lidar.
# 1) linear regression (lm)
# 2) multivariate linear regression (lm)
# 3) non-linear regression (nls)
# 4) machine learning (randomForest)


# 1) Linear Regression 

#build a linear regression
lr.m<-lm(AGB~zmax, metrics_all.m)

#check RMSE?
sqrt(mean((predict(lr.m)-metrics_all.m$AGB)^2))/mean(metrics_all.m$AGB)
sqrt(mean((predict(lr.m)-metrics_all.m$AGB)^2))

#how does the model fit?
plot(predict(lr.m), metrics_all.m$AGB)
abline(0,1)

# 2) Multi-Linear Regression - what variables are important?

library(leaps)
AGBreg<-regsubsets(AGB~., nbest=3, nvmax=5, data=metrics_all.m[,-c(1,2,8,59,61:62)], really.big = T)
summary(AGBreg)
plot(AGBreg)

#build a multi-linear regression
mlr.m<-lm(AGB~zmax+zsd, metrics_all.m)
summary(mlr.m)

#check RMSE?
sqrt(mean((predict(mlr.m)-metrics_all.m$AGB)^2))/mean(metrics_all.m$AGB)
sqrt(mean((predict(mlr.m)-metrics_all.m$AGB)^2))

#how does the model fit?
plot(predict(mlr.m), metrics_all.m$AGB)
abline(0,1)


# 3) non-linear regression (nls)

#does a height metric look non-linear?
plot(metrics_all.m$zmax, metrics_all.m$AGB)

#fit a non-linear model
nls.m<-nls(AGB~a*zmax^b, start=list(a=0.1,b=1),metrics_all.m)

#look at the model
lines(0:30,predict(nls.m, newdata=data.frame(zmax=0:30)))

#check error
sqrt(mean((predict(nls.m)-metrics_all.m$AGB)^2))/mean(metrics_all.m$AGB)
sqrt(mean((predict(nls.m)-metrics_all.m$AGB)^2))

library(randomForest)

#we can also use random forest - not always ideal
rf.m<-randomForest(AGB~., do.trace=TRUE,data=metrics_all.m[,-c(1,7, 59:60)],
                   ntree=1000)
plot(rf.m)
names(importance(rf.m)[rev(order(importance(rf.m))),])[1:10]
randomForest::varImpPlot(rf.m)

rf.m<-randomForest(AGB~zq90+zmax+zq55, do.trace=TRUE,data=metrics_all.m)

library(caret)
sqrt(mean((predict(rf.m)-metrics_all.m$AGB)^2))/mean(metrics_all.m$AGB)
sqrt(mean((predict(rf.m)-metrics_all.m$AGB)^2))

plot(predict(rf.m), metrics_all.m$AGB)
abline(0,1)

