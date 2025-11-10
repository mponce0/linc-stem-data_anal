# use only last row, i.e. latest time
control <- c(1,5,0,4,5)	# expected
exp <- c(2,2,0,7,4)	# observed
locn <- c("central","#1","#2","#3","#4")

# visual inspection
plot(control, col='blue', pch=20, ylim=c(0,max(control,exp)+.3))
lines(control, col='blue')
points(exp, col='red', pch=19)
lines(exp, col='red', pch=19)

# Run chi-squared test
print( chisq.test(exp, control) )

# plot differences
dev.new()
barplot(control-exp)
points(control-exp, col='red', pty=19)
lines(control-exp, col='red', lwd=1, lty=2)

