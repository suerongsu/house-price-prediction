# house-price-prediction
> house<-read.table("D:/price_prediction/house_price.csv",sep=",",header=TRUE)
> head(house)
> summary(house)
      var1             var2              var3           var4       house_price    
 Min.   :  0.00   Min.   : 0.3831   Min.   : 852   Min.   :1.00   Min.   :169900  
 1st Qu.: 60.94   1st Qu.: 6.0607   1st Qu.:1432   1st Qu.:3.00   1st Qu.:249900  
 Median :145.51   Median : 9.9424   Median :1888   Median :3.00   Median :299900  
 Mean   :223.93   Mean   :11.4649   Mean   :2001   Mean   :3.17   Mean   :340413  
 3rd Qu.:379.99   3rd Qu.:19.1358   3rd Qu.:2269   3rd Qu.:4.00   3rd Qu.:384450  
 Max.   :708.36   Max.   :22.7913   Max.   :4478   Max.   :5.00   Max.   :699900  
> plot(house)
 

#Use Multiple linear regression to plot data and predict house price#
> re=lm(house_price~var1+var2+var3+factor(var4),data=house)
> summary(re)

Call:
lm(formula = house_price ~ var1 + var2 + var3 + factor(var4), 
    data = house)

Residuals:
    Min      1Q  Median      3Q     Max 
-135108  -47061   -6806   37394  192208 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)    25986.90   77675.47   0.335    0.740    
var1              15.70      49.90   0.315    0.755    
var2             123.82    1461.62   0.085    0.933    
var3             138.91      17.16   8.093 7.11e-10 ***
factor(var4)2  42410.48   75513.39   0.562    0.578    
factor(var4)3  39820.46   72306.22   0.551    0.585    
factor(var4)4  13579.33   76271.65   0.178    0.860    
factor(var4)5  44391.12  116708.58   0.380    0.706    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 68880 on 39 degrees of freedom
Multiple R-squared:  0.7428,    Adjusted R-squared:  0.6966 
F-statistic: 16.09 on 7 and 39 DF,  p-value: 9.849e-10
#predited value with multiple linear regression#
> predict(re,data.frame(var1=27.5,var2=34,var3=1650,var4=3))
       1 
299642.7 
> plot(resid(re))
 
> qqnorm(resid(re))
> qqline(resid(re))
 
> house$pre<-predict(re,house)
> house$abserr<-abs(house$house_price-house$pre)/house$house_price
> rmse<-function(error){sqrt(mean(error^2))}
> error<-re$residuals
> predictionRMSE<-rmse(error)
#MSE Check for multiple linear regression#
> predictionRMSE
[1] 62740.34

#Use SVM to predict house price #

> library(e1071)
> svmre<-svm(house_price~var1+var2+var3+var4,house)
> summary(svmre)
Call:
svm(formula = house_price ~ var1 + var2 + var3 + var4, data = house)
Parameters:
   SVM-Type:  eps-regression 
 SVM-Kernel:  radial 
       cost:  1 
      gamma:  0.25 
    epsilon:  0.1 
Number of Support Vectors:  42
> predhousepri<-predict(svmre,data.frame(var1=27.5,var2=34,var3=1650,var4=3))
#Predicted value with SVM#
> predhousepri
     1 
321956 
> predictedRes<-predict(svmre,house)
> house$predSVM<-predictedRes
> error<-house$house_price-house$predSVM
> svrpredictionRMSE<-rmse(error)
#MSE check for SVM#
> svrpredictionRMSE
[1] 64155.31
> house$abserrsvm<-abs(house$house_price-house$predSVM)/house$house_price

Non-linear Regression (earth method) to predict 
> library(earth)
> nonlreg<-earth(house_price~var1+var2+var3+var4,house)
> summary(nonlreg)
Call: earth(formula=house_price~var1+var2+var3+var4, data=house)

             coefficients
(Intercept)     323402.64
h(1940-var3)      -111.63
h(var3-1940)       143.98

Selected 3 of 14 terms, and 1 of 4 predictors
Termination condition: Reached nk 21
Importance: var3, var1-unused, var2-unused, var4-unused
Number of terms at each degree of interaction: 1 2 (additive model)
GCV 5093186876    RSS 191157056379    GRSq 0.681175    RSq 0.7342121
> evimp(nonlreg)
     nsubsets   gcv    rss
var3        2 100.0  100.0
> house$prednonlreg1<-predict(nonlreg,house)
> error<-house$house_price-house$prednonlreg1
> nonlregpredictionRMSE<-rmse(error)
#MSE Check for non linear regression#
> nonlregpredictionRMSE
[1] 63774.38
> house$abserrnonlreg1<-abs(house$house_price-house$prednonlreg1)/house$house_price
> write.table(house,"D:/price_prediction/house_reg.csv",sep=",")
> predictnonleg<-predict(nonlreg,data.frame(var1=27.5,var2=34,var3=1650,var4=3))
#Predicted value with non linear regression#
> predictnonleg
     house_price
[1,]    291029.2

