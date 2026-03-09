## This is the Python version of the R code example
## Note that Python has much less functionality without packages
## Numpy is going to be critical for you to know

import numpy as np # This shorthand is pretty much universal

n = 100 # Number of Subjects
p = 5   # Number of predictors (includes an intercept)

mu = 0
sigma = 0.5

## Creating a design matrix
## Each row will represent 1 subject
## Each entry will be iid N(mu, sigma)

## The 
## X = np.random.randn(n*(p-1)).reshape(n,(p-1)) # This
np.random.seed(4893) # Setting the same seed does not produce 
                     # the same results as in R

X = np.random.normal(loc = mu, scale = sigma, size = (n,p-1))
X_1 = np.array([1]*n).reshape(n,)
X = np.insert(X , 0, X_1, axis=1)

## The response Y will be given by X %*% beta.star + e
## X is the design matrix defined above
## beta.star is the true vector of p coefficients
## e is the error, it is N(0,1)

beta_star = [3,1,0,-5,2]
Y = X.dot(beta_star) + np.random.randn(n)

## Here we will calculate beta.hat by hand
## Later we will compare to the results produced my lm()
## Recall that beta.hat = (XtX)^-1 %*% XtY

XtXinv = np.linalg.inv(np.transpose(X).dot(X))
XtY = np.transpose(X).dot(Y)

beta_hat = XtXinv.dot(XtY)

print(beta_star, '\n', beta_hat)

## Here we will calculate beta.hat by hand
## Pandas is used for data manipulation
import pandas as pd
import statsmodels.api as sm

## This step is completly unnecessary. It is shown
## to offer a more direct comparison from R

X = np.delete(X, obj = 0, axis = 1) # The index begins at 0
df = np.insert(X, 4, Y, axis=1)
df = pd.DataFrame(df, columns = ['x1', 'x2', 'x3', 'x4', 'y'])

## To use statsmodels, we must seperate our response and predictors
X = df[['x1', 'x2', 'x3', 'x4']]
X = sm.add_constant(X) # A different way to add a constant term
y = df['y']

m1 = sm.OLS(y, X).fit()

print(beta_star, '\n', beta_hat, '\n', m1.params)
## Recall that beta.hat = (XtX)^-1 %*% XtY

XtXinv = np.linalg.inv(np.transpose(X).dot(X))
XtY = np.transpose(X).dot(Y)

beta_hat = XtXinv.dot(XtY)

print(beta_star, '\n', beta_hat)

## Now lets do an example with a simulation study
## I want to take a discrete uniform distribution u(1,9) and 
## add continuous uniform noise U(0,1) to see if the resulting 
## rv is continuous uniform U(0,10)

## First, we will generate 10000 realizations each of u(1,9) and
## U(-1,1). Then we will add the two together, and make a QQ plot 
## comparing against U(0,10).
import matplotlib.pyplot as plt # standard graphics package

## This notation does work BUT it makes 'n' a float
## Later functions require 'n' to be an integer
## n = 1E4 
n = 10000

np.random.seed(4893)
u = np.random.randint(1,10, size=n) 
U = np.random.uniform(-1, 1, size=n)

X = U+u

## Quick diagnostics
print(min(X), max(X))
plt.hist(X)

## Making a QQ plot
## First we have to create an actual U(0,10)
U = np.arange(0,10, step=0.001)
probs = np.arange(0.01, 0.99, step=0.001)

plt.scatter(np.quantile(U, probs), 
         np.quantile(X, probs))
plt.title('QQ-Plot')
plt.xlabel('Theoretical Percentiles')
plt.ylabel('Sample Percentiles')


