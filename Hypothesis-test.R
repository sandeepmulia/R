#Activity 1 (Confidence intervals)
nx <-21
ny <-21
xbar <-17.5
ybar <-16.9
sx <-0.55
sy <-0.49

sp <- sqrt(((nx-1)*(sx^2) + (ny-1)*(sy^2))/(nx+ny-2))
lower_ci <-xbar - ybar - qt(0.975, nx + ny -2)*sp*sqrt(1/nx + 1/ny)
upper_ci <-xbar - ybar + qt(0.975, nx + ny -2)*sp*sqrt(1/nx + 1/ny)

print(sp)
print(lower_ci)
print(upper_ci)


#Activity 2 (Hypothesis test)
qf(0.05,24,20)

#Inference on variance example 
#The speeds (km/h) of 23 cars travelling along Anzac Parade are recorded, leading to the following set of data:  
#  57, 65, 65, 55, 58, 57, 71, 60, 57, 65, 62, 60, 54, 70, 57, 52, 49, 65, 68, 60, 65, 68, 44
#57,65,65,55,58,57,71,60,57,65,62,60,54,70,57,52,49,65,68,60,65,68,44
#The sample mean and standard deviation are, respectively, 60.174 km/h and 6.807 km/h. Graphical inspection of the data shows that the normality assumption can be reasonably assumed.
x<-c(57,65,65,55,58,57,71,60,57,65,62,60,54,70,57,52,49,65,68,60,65,68,44)
n<-length(x)
mean_observed=60.174
sdev=6.807
confidence_intvl=90
alpha=(100-confidence_intvl)*0.01
df = n-1
print(alpha)
mean_observed + qt((1-(alpha/2)),df=df)*(sdev/sqrt(n))
mean_observed - qt((1-(alpha/2)),df=df)*(sdev/sqrt(n))

#95% confidence interval for observed standard deviation
x<-c(57,65,65,55,58,57,71,60,57,65,62,60,54,70,57,52,49,65,68,60,65,68,44)
n<-length(x)
print(paste("Sample size:", n))
mean_observed=60.174
sdev=6.807
confidence_intvl=95
alpha=(100-confidence_intvl)*0.01
df = n-1
print(paste("Alpha , 1-alpha/2, alpha/2",alpha, (1-alpha/2), alpha/2))
qchisq((1-(alpha/2)),df=df)

sdev*sqrt((n-1)/(qchisq((1-alpha/2),df=df)))
sdev*sqrt((n-1)/(qchisq((alpha/2),df=df)))

#
#Pooled variance
n1<-68
n2<-74
s1<-4.89
s2<-6.43
xbar<-26.99
ybar<-35.76

sp_squared = ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
print(sp_squared)
sp = sqrt(sp_squared)
test_stat = (xbar-ybar)/(sp*(sqrt((1/n1) + (1/n2))))
print(paste("Pooled Variance:",sp))
print (paste("Test stat:",test_stat))
qt(0.05,(n1+n2-2))


#F-distribution
df1<-27
df2<-25
qf(0.05, df1=df1,df2=df2)
