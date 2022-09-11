#Assessment Q3
nx <-78
ny <-56
xbar <-71.1466
ybar <-79.0491
sx_sq <-120.9421
sy_sq <-74.5316

#Sp^2
sp_squared = ((nx-1)*sx_sq + (ny-1)*sy_sq)/(nx+ny-2)
print(sp_squared)

#Confidence intervals Lower and Upper
sp <- sqrt(sp_squared)
lower_ci <-xbar - ybar - qt(0.975, nx + ny -2)*sp*sqrt(1/nx + 1/ny)
upper_ci <-xbar - ybar + qt(0.975, nx + ny -2)*sp*sqrt(1/nx + 1/ny)

print(lower_ci)
print(upper_ci)

#test stat
test_stat = (xbar-ybar)/(sp*(sqrt((1/nx) + (1/ny))))
test_stat_alt = (xbar-ybar)/sqrt(sp_squared*(1/nx + 1/ny))

print(test_stat)
print(test_stat_alt)