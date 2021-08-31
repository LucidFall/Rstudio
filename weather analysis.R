library(lme4)
library(dplyr)
library(rms)
library(pROC)
library(BMA)
weather = read.csv("D:\\Users\\xiaol\\Documents\\weather.csv")
str(weather)

#section below is trying to deal with missing values

#here I removed all rows with NA on either raintoday or raintomorrow because not only there two are binary categorical variables, there 
#two variables I think are very important in our model, thus I think delete rows with these two missing are reasonable
weather = weather[!(is.na(weather$RainToday)|is.na(weather$RainTomorrow)),]
colSums(is.na(weather))

#for missing values that are non-catagorical, I decided to fill it with the mean of 
#that data of the same month at the same location because I believe variables like min/max temp and wind level are seasonal and regional

unique_location = unique(weather$Location)
unique_month = c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
#missing values filled here: MinTemp, MaxTemp, Rainfall, WindGustSpeed, WindSpeed9am, WindSpeed3pm, Humidity9am, Humidity3pm, 
#Pressure9am, Pressure3pm, Cloud9am, Cloud3pm, Temp9am, Temp3pm

Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}
#mode function modified from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

copy = weather[0,]
for (location in unique_location){
  for (month in unique_month){
    mod_index = c(3, 4, 5, 9, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21) # this all numerical
    location_month = weather[format(as.Date(weather$Date),'%m') == month & weather$Location == location, ]
    for (i in mod_index){
      location_month[is.na(location_month[,i]), i] = mean(location_month[, i], na.rm = TRUE)
    }
    mod_index_categorical = c(8, 10, 11) #remaining categorical variables to be dealt
    for (i in mod_index_categorical){
      location_month[is.na(location_month[,i]), i] = Mode(location_month[, i])
    }
    
    copy = rbind(copy, location_month)
  }
}
#these variables are removed due to the reason stated in the report
weather = copy[, !(names(copy) %in% c('Evaporation', 'Sunshine', 'Cloud9am', 'Cloud3pm'))]

#this is to fill the remaining missing values with the mean of the whole data.
weather[is.na(weather[, "WindGustDir"]), 'WindGustDir'] = Mode(weather[, "WindGustDir"])
mod_index = c(7, 14, 15)
for (i in mod_index){
  weather[is.na(weather[,i]),i] = mean(weather[,i],na.rm = TRUE)
}
colSums(is.na(weather)) #all 0


#Below is trying to analyze the data
weather_train = weather[!format(as.Date(weather$Date), format = '%Y')== 2017,]
weather_test = weather[format(as.Date(weather$Date), format = '%Y')== 2017,]

#trying to fit glm for this dataset below for variable selection. 
logit.glm = glm(RainTomorrow ~ . - Date, data=weather_train, family=binomial(logit))
summary(logit.glm)


backwards_aic = step(logit.glm, direction = 'backward')
forwardglm = glm(RainTomorrow ~ 1, family = binomial,  data = weather_train)
forward_aic = step(forwardglm, scope = list(lower = formula(forwardglm), upper = formula(logit.glm)), direction = 'forward')
both_aic = step(forwardglm, scope = list(lower = formula(forwardglm), upper = formula(logit.glm)), direction = 'both')

summary(backwards_aic)
summary(forward_aic)
summary(both_aic)
#forward, backward, and doing it on both directions yield the same model with the same AIC score(93884)
#proceed to build glmm with location as mixed effect. 


#BIC selection method below
bic_data = bic.glm(mod_glm$formula, data = weather_train, glm.family = 'binomial')

par(mfrow = c(1,2))
qqnorm(residuals(mod_glm), main = '')
plot(fitted(mod_glm), residuals(mod_glm), xlab = 'Fitted', ylab = 'Residuals')
abline(h=0, lty=2)


weather_train = weather %>% mutate(RainTomorrow = ifelse(RainTomorrow == "No",0,1))
mod_glm = glm(RainTomorrow ~ Humidity3pm + Pressure3pm + WindGustSpeed +
                WindDir3pm + RainToday + Pressure9am + WindSpeed3pm + 
                WindDir9am + MinTemp + MaxTemp + Rainfall + WindGustDir + 
                WindSpeed9am + Humidity9am + Temp9am + Location, family = binomial,
              data = weather_train)

pred_prob = predict(mod_glm, type = 'response')
roc_logit = roc(weather_train[,19]~pred_prob)
## The True Positive Rate ##
TPR <- roc_logit$sensitivities
## The False Positive Rate ##
FPR <- 1 - roc_logit$specificities
plot(FPR, TPR, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, lwd = 2,col = 'red')
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit),2)))
#this has an AUC score of 0.87

#cross-validation
mod = lrm(RainTomorrow ~ Humidity3pm + Pressure3pm + WindGustSpeed +
            WindDir3pm + RainToday + Pressure9am + WindSpeed3pm + 
            WindDir9am + MinTemp + MaxTemp + Rainfall + WindGustDir + 
            WindSpeed9am + Humidity9am + Temp9am + Location, data = weather_train, x = TRUE, y = TRUE, model = T)
cross.calib <- calibrate(mod, method="crossvalidation", B=10) # model calibration
plot(cross.calib, las=1, xlab = "Predicted Probability")

#validating on the test data
pred.prob <- predict(mod_glm, newdata = weather_train, type = "response")
weather_test$pred.prob <- predict(mod_glm, newdata = weather_test, type = "response")
deciles <- quantile(weather_test$pred.prob, probs = seq(0,1, by =0.1))
weather_test$decile <- findInterval(weather_test$pred.prob, deciles, rightmost.closed = T)
pred.prob <- tapply(weather_test$pred.prob, weather_test$decile, mean)
obs.prob <- tapply(weather_test$RainTomorrow, weather_test$decile, mean)

## The plot ##
par(family = 'serif')
plot(pred.prob, obs.prob, type = "l", ylab = "Observed",
     xlab = "Predicted", xlim = c(0,1), ylim = c(0,1))
abline(a=0, b=1)

