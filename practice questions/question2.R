x``
#Draw an ordered stem and leaf plot for the data and describe the shape of the plot.
a <- c(4.1,1.5,10.4,5.7,3,5.9,3.4,6.1,1.6,3.7,3.1,4.8,2,4.2,11.1,14.8,5.4,4.1,3.9,3.5,4.1,4.1,8.8,
3.3,6.2,5.6,4.3,10.3,7.1,7.6,10.8,2.8,9.5,0.7,4.4,12.9,12.1,9.2,4,5.7,7.2,6.1,5.7,3.9,6.1,5.9,4.7,
3.1,3.7,3.1)

sorted_a <- sort(a)

stem_leaf_a <- stem(a,2)

###

# Find the mean, median, mode, standard deviation and coefficient of variation for the data.
mean_a <- mean(a)

median_a <- median(a)

cv_a <- sd(a) / mean(a)

getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode_a <- getmode(a)

####

#Assuming that the data was drawn from a bell-shaped curve, within what limits would you expect:
    # 67% of the values to fall?
    # 95% of the values, and,
    # 99.9% of the data to fall?

lower_67 <- mean_a - sd(a)
upper_67 <- mean_a + sd(a)

lower_95 <- mean_a - 2*sd(a)
upper_95 <- mean_a + 2*sd(a)

lower_99 <- mean_a - 3*sd(a)
upper_99 <- mean_a + 3*sd(a)

####

#Compute the actual proportion of the sample that fall within the limits stipulated by the empirical rule. 
#is there reason to believe that the data came from a bell-shaped curve?

actual_1sd <- length(a[a > lower_67 & a < upper_67])  / length(a)
actual_2sd <- length(a[a > lower_95 & a < upper_95])  / length(a)
actual_3sd <- length(a[a > lower_99 & a < upper_99])  / length(a)

g <- a

h <- hist(g, breaks = 15, density = 15,
          col = "lightgray", xlab = "Accuracy", main = "Overall") 
xfit <- seq(min(g), max(g), length = 40)        #Trying to fit normal curve on hist of data
yfit <- dnorm(xfit, mean = mean(g), sd = sd(g)) #Histogram & actuals indicate data is not distrubed normally!!!
yfit <- yfit * diff(h$mids[1:2]) * length(g) 

lines(xfit, yfit, col = "black", lwd = 2)
