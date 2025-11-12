# data declaration
rounds <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
predators <- c(5, 8, 16, 24, 24, 16, 10, 8, 4, 10)
prays <- c(100, 100, 100, 100, 84, 80, 96, 100, 100, 100)

# plots
plot(rounds, prays, col='blue', type='b', ylim=c(0,max(prays,predators)))
points(rounds,predators, col='red', type='b')

dev.new()
plot(prays,predators, type='b', lwd=2, col='orange')
