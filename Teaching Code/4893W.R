## This example is to help demonstrate how Python and R can be very similar
## We are going to create a random data set and fit a linear regression

## First we set up some initial conditions
## It should be noted that <- and = do the same thing in R
## Normally I like to use = but for comparison this script will use <-

n <- 100    # Number of subjects
p <- 5      # Number of predictors (includes an intercept)

mu <- 0     # For realizing normal random variables
sigma <- .5

## Creating a design matrix
## Each row will represent 1 subject
## Each entry will be iid N(mu, sigma)
set.seed(4893)

X <- matrix(nrow=n, ncol=p-1, rnorm(n = n*(p-1), mean = mu, sd = sigma))
X.int <- rep(1, n)
X <- cbind(X.int, X)

## The response Y will be given by X %*% beta.star + e
## X is the design matrix defined above
## beta.star is the true vector of p coefficients
## e is the error, it is N(0,1)

beta.star <- c(3,1,0,-5,2)
Y <- X %*% beta.star + rnorm(n=n, mean=0, sd=1)

## Here we will calculate beta.hat by hand
## Later we will compare to the results produced my lm()
## Recall that beta.hat = (XtX)^-1 %*% XtY

XtXinv <- qr.solve(t(X)%*%X) 
XtY <- crossprod(X, Y)

beta.hat = XtXinv %*% XtY

c(beta.hat, beta.star)# print() also works in R

## Now with lm()
## First we convert the matrix to a data frame
df <-  cbind(X[-1], Y)
df <- as.data.frame(df)
names(df) <- c('x1', 'x2', 'x3', 'x4', 'y')

## This should look familiar
m1 <- lm(y ~ ., data = df)
c(m1$coefficients, beta.hat, beta.star)

## Now lets do an example with a simulation study
## I want to take a discrete uniform distribution u(1,9) and 
## add continuous uniform noise U(0,1) to see if the resulting 
## rv is continuous uniform U(0,10)

## First, we will generate 10000 realizations each of u(1,9) and
## U(-1,1). Then we will add the two together, and make a QQ plot 
## comparing against U(0,10).

n <- 1e4 # scientific notation

set.seed(4893)
u <- sample(1:9, n, replace=T) # realizations of u(1,9)
U <- runif(n, min=-1, max=1) # realizations of U(-1,1)

X <- U+u 

## Quick diagnostics
range(X)
hist(X)

## Making a QQ plot
## First we have to create an actual U(0,10)
U = seq(0, 10, length.out = 1e4)
probs=seq(from=0.01, to=0.99, by=0.001)
plot(quantile(U,probs), 
     quantile(X, probs), 
     xlab = 'Theoretical Percentiles',
     ylab = 'Sample Percentiles',
     main = 'QQ-Plot')
abline(0,1)
