##############################
#    Distributions           #
##############################

#Bernoulli distribution
x<-function(x,p) (p^x*(1-p)^(1-x))
x(0,0.5)

#Binomial distribution
#to take value 4 using sample size 10 with prob=0.3
#we get 0.2
dbinom(4,10,0.3) 

fx<-function(x,n,p)(p^x*(1-p)^(n-x))
fx(0,18,0.6)

#binomial variance n*p(1-p)
Bin_variance(10,0.5)
#binomial expected n*p
Bin_expected(10,0.5)
sqrt(Bin_variance(10,0.5))
dbinom(7,10,0.5)

#Buttered bread falling down
Bin_expected<-function(n,p) n*p
Bin_variance<-function(n,p) n*p*(1-p)
print(Bin_expected(10,0.5))
print(Bin_variance(10,0.5))
stddev=sqrt(Bin_variance(10,0.5))
print(stddev)
dbinom(7,10,0.5)


curve(
  dbinom(x, size=10, prob=0.3),
  from = 0,
  to = 10,
  type = 'p', # It looks better using points rather than the default lines
)

curve(
  dbinom(x,size=12,prob=0.2),
  from = 0,
  to = 10,
  type = 'p', # It looks better using points rather than the default lines
)


#Poisson distribution
#If, on average, 5 servers go offline during the day, what is the chance that no more than 1 will go offline? (Assume independence of servers going offline).
ppois(1,lambda = 5)
#Poisson expected = lambda and variance = lambda


#exponential distribution
exp <- function(x,beta) (1/beta)*exp(-x/beta)
exp_expected<-function(x,beta) beta
exp_variance<-function(x,beta) beta*beta


#Uniform distribution
uniform <-function(x,a,b) 1/(b-a)
uniform_expected <-function(x,a,b) (a+b)/2
uniform_variance <-function(x,a,b) (b-a)/12
#test
uniform(x,1,3)

#Normal distribution (continuous random variables)
std_density = function(x) (1/sqrt(2*pi))*(exp((-x^2)/2))
integrate(std_density,lower=-Inf,upper=0.47 )
pnorm(0.47) #same as the above

pnorm(12,mean=10,sd=9)

#height greater than 182.9 cms
1-pnorm(182.9,mean=174,sd=6.4)

#What's the chance that a randomly selected young man is 170-something cm tall?
round(pnorm(180,mean=174,sd=6.4)-pnorm(170,mean=174,sd=6.4),4)

#Find a range of heights that contains 95% of young men.
x<-qnorm(0.95,mean=174,sd=6.4)
print(x)

#Chi squared distribution
#Chi-squared distribution with 6 degrees of freedom
pchisq(2,df=6)

#Conversely, to find the value x such that P(X <= x) = p 
#for p known, we use the command line:
#Chi-squared distribution with 6 degrees of freedom
qchisq(0.080314,df=6)

#Student t-distribution
df<-3
pt(-1,df=df);
#Reverse P(X<=x)=p
qt(pt(-1,df),df)

#F-distribution
# Example: F distribution with 3 and 2 degrees of freedom
df1<-3
df2<-2
pf(1.3, df1=df1,df2=df2)
#Converse
qf(0.5374263,df1=df1,df2=df2)
# x here is the pth quantile of X
