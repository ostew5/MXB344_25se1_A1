# The data set laptops.csv is available on Canvas. 
# It contains extensive information on 363 different laptop saels listings. 
# We will be interested primarily in release price, and which of the other 
# variables we can use to predict the release price of a new laptop.

library(GGally)
library(ggpubr)
library(tidyverse)
data <- read.csv("pracs/laptops-1.csv",header=TRUE)

# (a) Which variables do you think will be useful in predicting release price? 
# Are any of these variables related to each other? (For example, CPU Frequency and release year, or cores and threads.)

names(data)

# (b) Construct scatterplots to examine the relationships between any of these "predictor" variables. 
# If the relationships are strong, you may be able to remove them from the list of possible predictors of release price.

p <- ggpairs(data)
ggsave("prac/ggpairs_data.png", plot = p, dpi = 300, width = 20, height = 20)
ggpairs(data,columns=c(
  "Release.Price....",
  "RAM..GB.",
  "SSD..GB.",
  "Cores")
        )

# (c) Once you have settled on a list of predictors of release price, write down the algebraic formulation of the model.

# Y = beta_0 + beta_1 * X_1 + beta_2 * X_2 + beta_3 * X_3 + eps
# or
# Y = X*beta + eps, X = [1,X_1,X_2,X_3], beta = [beta_0,beta_1,beta_2,beta_3]'
# eps ~ N(0.0, sigma^2)

# (d) Use R to estimate the values of each of the slopes and the intercept for your model. Interpret the output.

laptops <- lm(Release.Price.... ~ RAM..GB. + SSD..GB. + Cores, data = data)
summary(laptops)

# (e) Create and examine the residual plots in R. Is there anything you wish to investigate further?

residual.df <- fortify(laptops)

names(residual.df)
a<- ggplot(data=residual.df,aes(x=.fitted,y=.resid)) + 
  geom_point() +
  labs(x="Fitted values",y="Residuals", title="Residuals vs Fitted Values") +   
  geom_hline(yintercept=0)
b<- ggplot(data=residual.df,aes(x=1:363,y=.resid)) + 
  geom_line() + 
  geom_hline(yintercept=0) + 
  labs(x="Index",y="Residuals",title="Independence Plot")
c<- ggplot(data=residual.df,aes(sample=.stdresid)) +
  stat_qq() + stat_qq_line() +
  labs(title="Normal Q-Q Plot")
d<- ggplot(data=residual.df,aes(x=.resid)) +
  geom_histogram(binwidth=50) + 
  labs(x="Residuals",y="Frequency",title="Histogram of Residuals")
ggarrange(a,b,c,d)

e<-ggplot(data=residual.df,aes(x=RAM..GB.,y=.resid)) + 
  geom_point()+
  labs(x="RAM (GB)",y="Residuals")
f<-ggplot(data=residual.df,aes(x=SSD..GB.,y=.resid))+ 
  geom_point()+
  labs(x="SSD (GB)",y="Residuals")
g<-ggplot(data=residual.df,aes(x=Cores,y=.resid))+ 
  geom_point()+
  labs(x="# Cores",y="Residuals")
ggarrange(e,f,g,ncol=3)

# (f) Use stepwise regression to determine the most appropriate model. 
# Is this model more appropriate than the one you estimated in part (d)? Why/why not?

laptops_full <- lm(data=data,Release.Price.... ~  .)

laptops_fit <- step(laptops_full,scope=~.,direction="both",trace=F)

extractAIC(laptops)
extractAIC(laptops_fit)

# (g) Given the model in part (f), what proportion of variability in the data does the model explain?

summary(laptops_fit)
# Look at R^2 value

# (h) Given the model in part (f), what is the distribution of the residuals? 
# Include estimates of any parameters, as required. Provide some interpretation of this distribution.

laptops_fit.df <- fortify(laptops_fit)
sigma <- sd(laptops_fit.df$.resid)

ci95 <- c(-1,1) * qnorm(0.975) * sigma
ci95

names(laptops_fit.df)
a<- ggplot(data=laptops_fit.df,aes(x=.fitted,y=.resid)) + 
    geom_point() +
    labs(x="Fitted values",y="Residuals", title="Residuals vs Fitted Values") +   
    geom_hline(yintercept=0)
b<- ggplot(data=laptops_fit.df,aes(x=1:363,y=.resid)) + 
    geom_line() + 
    geom_hline(yintercept=0) + 
    labs(x="Index",y="Residuals",title="Independence Plot")
c<- ggplot(data=laptops_fit.df,aes(sample=.stdresid)) +
    stat_qq() + stat_qq_line() +
    labs(title="Normal Q-Q Plot")
d<- ggplot(data=laptops_fit.df,aes(x=.resid)) +
    geom_histogram(binwidth=50) + 
    labs(x="Residuals",y="Frequency",title="Histogram of Residuals")
ggarrange(a,b,c,d)

e<-ggplot(data=laptops_fit.df,aes(x=Current.Price....,y=.resid)) + 
    geom_point()+
    labs(x="Current Price",y="Residuals")
f<-ggplot(data=laptops_fit.df,aes(x=Discount....,y=.resid))+ 
    geom_point()+
    labs(x="Discount (%0",y="Residuals")
g<-ggplot(data=laptops_fit.df,aes(x=Threads,y=.resid))+ 
    geom_point()+
    labs(x="# Threads",y="Residuals")
ggarrange(e,f,g,ncol=3)

