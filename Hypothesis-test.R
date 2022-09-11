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

#Assessment Q3
nx <-78
ny <-56
xbar <-71.1466
ybar <-79.0491
sx <-120.9421
sy <-74.5316

sp <- sqrt((((nx-1)*(sx^2)) + ((ny-1)*(sy^2))) /(nx+ny-2))
lower_ci <-xbar - ybar - qt(0.975, nx + ny -2)*sp*sqrt(1/nx + 1/ny)
upper_ci <-xbar - ybar + qt(0.975, nx + ny -2)*sp*sqrt(1/nx + 1/ny)

print(sp)
print(sp^2)
print(lower_ci)
print(upper_ci)

2*pnorm(-1.96)

pnorm(-5.29,lower.tail = TRUE) + pnorm(-1.37,lower.tail = TRUE)

