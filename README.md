# Effectiveness of Marketing Techniques In-Store to increase Sales

# Abstract
In the world of marketing, there are many levers that marketers can use that communicate with the consumers. The consumers may get a chance to interact with the brand through different mediums like TV, radio, social, online or instore. Within each of these mediums, there are several options from which marketers' plan to optimize their budget effectively. This paper aims to understand the effectiveness of the marketing done instore where the purchase decisions are made by the consumer. While most of the levers in marketing are run by the marketing team, in-store marketing has some levers that are run by the sales team. Hence the effectiveness of in-store marketing is also dependent on the dynamics cross of functional teams

## Introduction
In-store marketing is usually the most important touch point for most brands. This is the point of sale and hence there are a couple of factors that affect the consumer's decision, one can be the loyalty of the consumers whereas the other can be the environment in the store. Consumer loyalty is built over a period of time and may vary at different points in time. In some cases, the consumer may be a loyalist or prefer a particular brand and would not purchase any other brand. In other cases, the consumer may prefer to try different products and would like to experiment within the category. The consumer might also get influenced by the environment in the store. Factors like discount offers or a communication on the packaging or on the shelf play an important role in the decision making of the consumer in-store.

trend <- read.csv("sabraunit.csv")
summary(trend)

#eda
trend$Time <- as.Date(trend$Time, "%m/%d/%Y")
plot(trend$Dollar.Sales ~ trend$Time, trend, xaxt = "n", type = "l")
axis(1, trend$Time, format(trend$Time, "%b %Y"), cex.axis = .5)
library(ggplot2)
ggplot(trend, aes(x = BOGOs, y = Dollar.Sales)) + geom_boxplot()
ggplot(trend, aes(x = Product.Demo, y = Dollar.Sales)) + geom_boxplot()
ggplot(trend, aes(x = Digital.Coupon, y = Dollar.Sales)) + geom_boxplot()
ggplot(trend, aes(x = Shelf.talk.Banner, y = Dollar.Sales)) + geom_boxplot()
ggplot(trend, aes(x = Price.offs, y = Dollar.Sales)) + geom_boxplot()
hist(trend$Dollar.Sales)

#Linear regression
ggplot(trend, aes(x=trend$Time, y=trend$Dollar.Sales)) + geom_point() + geom_smooth(method = lm) + labs(title="Dollars Sales every year", x="Time", y="Dollar Sales")

#Multi linear regression
mlr<-lm(Dollar.Sales~Product.Demo + Digital.Coupon + Shelf.talk.Banner + Price.offs + BOGOs, data=trend)
summary(mlr)

#Outlier Analysis
coefficients(mlr) # model coefficients
confint(mlr, level=0.95) # CIs for model parameters 
fitted(mlr) # predicted values
residuals(mlr) # residuals
anova(mlr) # anova table 
vcov(mlr) # covariance matrix for model parameters 
influence(mlr) # regression diagnostics

layout(matrix(c(1,2,3,4),2,2))
plot(mlr)

step<-stepAIC(mlr, direction="both")
step$anova



# All Subsets Regression
library(leaps)
attach(mydata)
leaps<- regsubsets(Dollar.Sales~Product.Demo + Digital.Coupon + Shelf.talk.Banner + Price.offs + BOGOs, data=trend, nbest = 10)
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 

boot <- boot.relimp(mlr, b = 1000, type = c("lmg","last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot)
plot(booteval.relimp(boot,sort=TRUE))

outlier_tf = outlier(trend$Dollar.Sales,logical=TRUE)
find_outlier= which(outlier_tf==TRUE,arr.ind = TRUE)
find_outlier
data_new=trend[-find_outlier,]
data_new

newmlr<-lm(Dollar.Sales~Product.Demo + Digital.Coupon + Shelf.talk.Banner + Price.offs + BOGOs, data=data_new)
summary(newmlr)

smmlr<- lm(Dollar.Sales~Product.Demo + Digital.Coupon + Shelf.talk.Banner, data=data_new)
summary(smmlr)

tmmlr<- lm(Dollar.Sales~ Price.offs + BOGOs, data=data_new)
summary(tmmlr)

#Anova
fit<-aov(data_new$Dollar.Sales~data_new$Product.Demo+data_new$Shelf.talk.Banner+data_new$Digital.Coupon+data_new$Price.offs+data_new$BOGOs)
summary(fit)
#Anova for shopper marketing techniques
fit2<-aov(data_new$Dollar.Sales~data_new$Product.Demo+data_new$Shelf.talk.Banner+data_new$Digital.Coupon)
summary(fit2)

#Anova for trade marketing techniques

fit1<- aov(data_new$Dollar.Sales~data_new$Price.offs+data_new$BOGOs)
summary(fit1)


#Decision Tree
DTsabdata <- read.csv("mod_sabraunit.csv")
newtrend <- DTsabdata[c(-1,-2)]
row <- nrow(newtrend)
rsample <- sample(row, row * .70)
train_data <- newtrend[rsample,]
test_data <- newtrend[-rsample,]
library("rpart")
tree <- rpart(Dollar.Sales ~ Product.Demo + Digital.Coupon + Shelf.talk.Banner+ Price.offs + BOGOs, data = train_data, method = "class")
library("rpart.plot")
rpart.plot(tree)
