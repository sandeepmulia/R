pnorm(-1, mean=-2, sd=3);
1-pnorm(-3.5,mean=-2,sd=3);

pnorm(6,mean=-2,s=3) - pnorm(-7,mean=-2,s=3);

#integrate e^x with upper and lower limit
f<-function(x) exp(-x)
integrate(f,lower=2,upper=3)

g<-function(p) (-log(1-p))
g(0.5)
g(0.75)


h<-function(x) exp(-x)
integrate(h,lower = 0, upper=+Inf)

i<-function(x) x
integrate(i,lower=0,upper=1)


#expected current
cx <-function(x) (x*.5)
integrate(cx,lower=1,upper=3)


#power calculation P=3I^2
#E(I) = integral(x*dx)
#problem statement says 1/2 hence the below formula
cx <-function(x) (3*0.5*(x^2))
integrate(cx,lower=1,upper=3)

#lifetime of a bulb
ax <-function(x) ((x^2)*exp(-x))
integrate(ax,lower=0,upper=Inf)

#variance calculation - slide 84 / Lesson 1.4 SD example 2
x<-c(1,2,3,4,5)
px<-c(0.15,0.25,0.2,0.25,0.15)
prod=x*px
print(prod)
#Calculate mean
ex=sum(x*px)

#calculate variance
varianceA = sum((x - sum(x*px))^2 * px)
#calculate std deviation
stddev = sqrt(variance)
print(stddev)
print(varianceA)

x<-c(1,2,3,4,5)
px<-c(0.1,0.1,0.6,0.1,0.1)
varianceB = sum((x - sum(x*px))^2 * px)
print(varianceB)


