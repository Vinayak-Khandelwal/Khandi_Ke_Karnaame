## Logit model

probs <- seq(0,1, by = 0.1)
odds <- probs/(1-probs)
logodds <- log(odds)

#table drawing in R
tab1 <- cbind(probs,odds)
tab2 <- cbind(odds,logodds)
tab3 <- cbind(logodds,probs)

#plots for tables 
probsp <- seq(0,1, length=1000)
oddsp <- probsp/(1-probsp)
logoddsp <- log(oddsp)

plot(probsp,oddsp,type='l')
plot(oddsp,logoddsp,type='l')
plot(logoddsp,probsp,type='l')

#example-1

discount <- c(0,0,0,0,0,0,50,50,50,50,50,100,100,100,100,150,150,150,200,200)
buy <- c(0,0,0,1,0,0,0,1,0,0,1,1,1,0,1,1,0,1,1,1)
tab <- cbind(discount,buy)

ex1 <- glm(buy~discount , family= binomial)
summary(ex1)
# the estimates that come in the summary are beta0 and beta1

#example-2

data <- read.csv("CAFE.csv",header = TRUE,sep = ",")
#now we transform the contribution variable
Amt1 <- log(data$Contribution+1)
head(Amt1,n=5)

REP <- !(data$Dem)

logit2 <- glm(data$Vote ~ REP + Amt1,family = binomial)
summary(logit2)

#estimates give values for beta0, beta1 and beta2