#### My NR function (binomial)
NRfit <- function(y,X,B=c(0,0,0),n.iter=100,tol=1e-4) {
  for (i in 2:n.iter) {
    lp <- as.numeric(X%*%B)
    p <- 1/(1+exp(-lp))
    w <- p*(1-p)
    W <- diag(w)
    ifish <- solve(t(X)%*%W%*%X)
    z <- lp + (y-p)/w
    B_old <- B
    B <- ifish%*%t(X)%*%W%*%z
    
    if (all(abs(B-B_old) < tol)) return(list(B=B,se=sqrt(diag(ifish))))
  }
  #print(se=diag(sqrt(ifish)))
  out <- list(B=B,se=sqrt(diag(ifish)))
  out
}

#### Testing function
n.obs <- 100
beta <- c(-2,1,-0.5)
x1 <- runif(n.obs,-1,1)
x2 <- runif(n.obs,-1,1)
lp <- beta[1] + beta[2]*x1 + beta[3]*x2 
p <- 1/(1+exp(-lp))
y <- as.numeric(p>runif(n.obs))
X <- cbind(rep(1,n.obs),x1,x2)

#n <- rep(1,n.obs)
#yn <- cbind(y,1-y)
fit <- glm(y~x1+x2,family=binomial(link="logit"))
coefficients(fit)
NRfit(y,X)


#### Seeds NR example
data <- read.csv("seeds_NR.csv",header=TRUE)
attach(data)
bean <- as.numeric(plant=="bean")
yn <- cbind(germinated,total-germinated)
fit <- glm(yn~bean,family=binomial(link="logit")); coefficients(fit)
y <- germinated
X <- cbind(1,bean)
NRfit(y,X,B=c(0,0))

# generate Poisson data
n.obs <- 100
beta <- c(3,1,-0.5)
x1 <- runif(n.obs,-1,1)
x2 <- runif(n.obs,-1,1)
lp <- beta[1] + beta[2]*x1 + beta[3]*x2
lambda <- exp(lp)
y <- rpois(100,lambda)
X <- cbind(rep(1,n.obs),x1,x2)



#### My NR function (poisson)
NRfit <- function(y,X,B=c(0,0,0),n.iter=100,tol=1e-4) {
  for (i in 2:n.iter) {
    lp <- as.numeric(X%*%B)
    lambda <- exp(lp)
    w <- lambda
    W <- diag(w)
    ifish <- solve(t(X)%*%W%*%X)
    z <- lp + (y-lambda)/w
    B_old <- B
    B <- ifish%*%t(X)%*%W%*%z
    
    if (all(abs(B-B_old) < tol)) return(list(B=B,se=sqrt(diag(ifish))))
  }
 out<-list(B=B,se=sqrt(diag(ifish)))
 out
}
 

NRfit(y,X)
pois_fit<- glm(y~x1+x2, family=poisson(link="log"))
summary(pois_fit)

data <- read.csv("infections_NR.csv",header=TRUE)
attach(data)
head(data)
fit <- glm(infections~ward,family=poisson); coefficients(fit)
y <- infections
X <- cbind(1,ward)
NRfit(y,X,B=c(0,0))
 