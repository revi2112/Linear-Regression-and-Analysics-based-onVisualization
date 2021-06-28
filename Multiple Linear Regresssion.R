install.packages("psych")
library(Hmisc) 
library(psych)
library(car)

auto <- read.csv("Automobile_data.csv",na.strings="?")
head(auto)
str(auto)
sum(is.na(auto))
sapply(auto,function(x) sum(is.na(x)))
dim(auto)
lapply(auto, function(x){
  if(!(is.numeric(x) | is.integer(x)))
    unique(x)
})

auto$num.of.doors <- as.factor(auto$num.of.doors)
levels(auto$num.of.doors)
auto$num.of.doors<-ifelse(auto$num.of.doors=="two",2,4)
head(auto$num.of.doors)
str(auto)

auto$fuel.type <- as.factor(auto$fuel.type)
auto$fuel.type <- ifelse(auto$fuel.type == "gas",1,0)

auto$drive.wheels <-as.factor(auto$drive.wheels)
auto$drive.wheels <- ifelse(auto$drive.wheels == "rwd",0,1)





str(auto)

### Eliminate missing values
auto$price <- as.numeric(impute(auto$price, mean))
auto$normalized.losses <- as.numeric(impute(auto$normalized.losses, mean))
auto$num.of.doors <- as.numeric(impute(auto$num.of.doors, median))
auto$horsepower <- as.numeric(impute(auto$horsepower, mean))
auto$peak.rpm <- as.numeric(impute(auto$peak.rpm, mean))
auto$bore <- as.numeric(impute(auto$bore, mean))
auto$stroke <- as.numeric(impute(auto$stroke, mean))
sum(is.na(auto))
head(auto)



### Select variables for regression modelling
auto.sel <- subset(auto,select =c("price", "wheel.base", "length", "width","height", "normalized.losses", "curb.weight","engine.size", "bore", "stroke", "compression.ratio", "horsepower",
  "peak.rpm", "city.mpg", "highway.mpg","symboling","num.of.doors","drive.wheels","fuel.type"))
str(auto.sel)
pairs.panels(auto.sel,col="red")
#length width are highly corelated
#highway.mpg and city.mpg are highly colrelaetd highly  corelated
#compression ratio and num.of.doors , symbolling,drive,wheels,fuel.type are not contributing
#city.mpg and curb.weight  negituvely corelated
#curbweight engine size highly corelated
#wheel base,length to crubweight

auto.sel <- subset(auto, select = c("price","height", "normalized.losses", "curb.weight", "bore", "stroke", "horsepower",
                                    "peak.rpm", "city.mpg"))
str(auto.sel)
pairs.panels(auto.sel,col="red")

### Transform non-linear vars
##right skew
auto.sel$price <- log10(auto.sel$price)
auto.sel$horsepower <- log10(auto.sel$horsepower)
auto.sel$normalized.losses <- log10(auto.sel$normalized.losses)
pairs.panels(auto.sel,col="red")


### Split data into training and validation samples
set.seed(333)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$price), round(length(auto.sel$price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]

fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+height+bore, data=train.sample)
summary(fit) #81.1
#pvalue for height(0.84919),bore(0.7754) is very high so we will remove it 

fit <- lm(price ~ horsepower+curb.weight+city.mpg+peak.rpm+normalized.losses+stroke, data=train.sample)
summary(fit) #81.3

#pvalue for normalized.loses(0.82) and city.mpg(0.993) is very high so we will remove it

fit <- lm(price ~ horsepower+curb.weight+peak.rpm+stroke, data=train.sample)
summary(fit) #81.3

plot(fit)
#we can see the curve is fit to a point and there are some outliers from the 4 plots
 #45,129,130,13
crPlots(fit)


### Eliminate observations known to be extreme for the final linear model

cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff) #128
train.sample <- train.sample[-which(rownames(train.sample)
                                    %in% c("45","30","128","129","130")),]  

### Create a model which avoids multi-collinearity and checks the p-values
fit <- lm(price ~ curb.weight+peak.rpm+stroke, data=train.sample)

plot(fit) #13,127,156,67

summary(fit) #82.9
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <-train.sample[-which(rownames(train.sample) 
                                   %in% c("13","67","127","156")),]

fit <- lm(price ~ curb.weight+peak.rpm+stroke, data=train.sample)
plot(fit) #15,17,110,114,75
summary(fit) #86.8
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <-train.sample[-which(rownames(train.sample) 
                                   %in% c("15","17","110","75","114")),]

fit <- lm(price ~ curb.weight+peak.rpm+stroke, data=train.sample)
summary(fit) #88.2
plot(fit)#66,126,103,111,139,2
cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)


train.sample <-train.sample[-which(rownames(train.sample) 
                                   %in% c("111","139","2","66","103","126")),]
fit <- lm(price ~ curb.weight+peak.rpm+stroke, data=train.sample)
plot(fit)
summary(fit) # R2=90.7%
vif(fit)
plot(fit)


#############predictions on valid and rmse and mae

train.sample$pred.price <- predict(fit, newdata = subset(train.sample, select=c(price, peak.rpm, curb.weight,stroke)))
valid.sample$pred.price <- predict(fit,  newdata = subset(valid.sample, select=c(price, peak.rpm, curb.weight,stroke)))


train.corr <- round(cor(train.sample$pred.price, train.sample$price), 2)
train.RMSE <- round(sqrt(mean((10 ^ train.sample$pred.price - 10 ^ train.sample$price)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$pred.price - 10 ^ train.sample$price)))
c(train.corr^2, train.RMSE, train.MAE)

valid.corr <- round(cor(valid.sample$pred.price, valid.sample$price), 2)
valid.RMSE <- round(sqrt(mean((10 ^ valid.sample$pred.price - 10 ^ valid.sample$price)^2)))
valid.MAE <- round(mean(abs(10 ^ valid.sample$pred.price - 10 ^ valid.sample$price)))
c(valid.corr^2, valid.RMSE, valid.MAE)


