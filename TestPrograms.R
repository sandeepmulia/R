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

#Mean
x<-c(500,100,20,0)
px<-c(1/1000,2/1000,10/1000,987/1000)
#Calculate mean
meanval=sum(x*px)
print(meanval)

x<-c(2,3,4,5,6,7,8,9,10,11,12)
px<-c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
meanval=sum(x*px)
print(meanval)
print(sum(px[6],px[10]))


#Differentiation
f=expression(x^(1/3))
D(f,'x')

