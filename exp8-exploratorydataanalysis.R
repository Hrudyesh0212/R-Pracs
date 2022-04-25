dat <- iris
head(dat, 4)
#Minimum
min(dat$Sepal.Length)
#Maximun
max(dat$Sepal.Length)
#Range Function
rng <- range(dat$Sepal.Length)
rng[1]
rng[2]
#Range Formula
max(dat$Sepal.Length) - min(dat$Sepal.Length)
#Range Through Function
range2 <- function(x) {
  range <- max(x) - min(x)
  return(range)
}
range2(dat$Sepal.Length)
#Mean
mean(dat$Sepal.Length)
#Median
median(dat$Sepal.Length)
#Median through quantile
quantile(dat$Sepal.Length, 0.5)
#First quartile
quantile(dat$Sepal.Length, 0.25)
#Third quartile
quantile(dat$Sepal.Length, 0.75)
# 4th decile
quantile(dat$Sepal.Length, 0.4)
# 98th percentile
quantile(dat$Sepal.Length, 0.98)
#Interquartile range
IQR(dat$Sepal.Length)
#IQR Through Formula
quantile(dat$Sepal.Length, 0.75) - quantile(dat$Sepal.Length,
                                            0.25)
#Standard deviation
sd(dat$Sepal.Length)
#Variance
var(dat$Sepal.Length)
#standard deviation (or variance) of multiple variables
lapply(dat[, 1:4], sd)
#Summary
summary(dat)
#By Function
by(dat, dat$Species, summary)
#Coefficient of variation
sd(dat$Sepal.Length) / mean(dat$Sepal.Length)
#Mode
tab <- table(dat$Sepal.Length)
sort(tab, decreasing = TRUE)
#Table Function
sort(table(dat$Species), decreasing = TRUE)
summary(dat$Species)
#Contingency table
dat$size <- ifelse(dat$Sepal.Length <
                     median(dat$Sepal.Length),
                   "small", "big"
)
table(dat$size)
table(dat$Species, dat$size)
#xtabs
xtabs(~ dat$Species + dat$size)
#prop table function
prop.table(table(dat$Species, dat$size))
round(prop.table(table(dat$Species, dat$size), 1), 2)