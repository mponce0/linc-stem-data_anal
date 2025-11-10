# use only last row, i.e. latest time
control <- c(1,5,0,4,5)
exp <- c(2,2,0,7,4)

# visual inspection
plot(control, col='blue', pch=20)
lines(control, col='blue')
points(exp, col='red', pch=19)
lines(exp, col='red', pch=19)

# Run chi-squared test
print( chisq.test(exp, control) )
