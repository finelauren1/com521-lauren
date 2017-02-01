#PC0. Load Data. Take mean of x
library(readr)
com521_population <- read.delim("~/Desktop/OneDrive/Winter 2017/Statistics/uwcom521-assignments/week_05/com521_population.tsv")

View(com521_population)
head(com521_population)

mean(com521_population$x)
#41.878

#PC1. Go back to the dataset I distributed for the week 3 problem set. 
#Compute the 95% confidence interval for the variable x in two ways:

#(a) By hand using the normal formula for standard error
setwd("/users/laurenfine/desktop/onedrive/winter 2017/statistics/com521-lauren")
library(readr)
week3_dataset_lauren <- read_csv("/users/laurenfine/desktop/onedrive/winter 2017/statistics/com521-lauren/week3_dataset-lauren.csv")

mean(week3_dataset_lauren$x)
#33.888

mean(week3_dataset_lauren$x) + (1.96*sd(week3_dataset_lauren$x)/sqrt(length(week3_dataset_lauren$x)))*c(-1,1)
#(26.992, 40.784)

#Using the appropriate built-in R function
t.test(week3_dataset_lauren$x)
#(26.907, 40.869)
#the t.test function doesn't assume normal distribution, while the formula does.

#c)compare the true mean to my sample mean.
# The true mean is not inside the confidence interval for my sample mean.


#PC2. Compare the distribution from your sample of x to the true population.
#Draw histograms and compute other descriptive and summary statistics. Discuss.

hist(week3_dataset_lauren$x)
#Pretty normally distributed betewen -50 and 100

hist(com521_population$x)
#very normally distributed between -100 and 200

summary(week3_dataset_lauren$x)
summary(com521_population$x)
#entire population has larger distribution than my sample. 
#Median and Mean are closer on the population

t.test(week3_dataset_lauren$x)
t.test(com521_population$x)
#the population's t-test shows a smaller confidence interval

#PC3. Compute mean of y from true population. Compute mean and confidence interval my sample. In/out?

mean(com521_population$y)
#241.5074

t.test(week3_dataset_lauren$y)
#mean of x: 191.5833 
#Confidence interval: (153.1027, 230.0639)

#Once again, the true mean is significantly higher than my mean
#and my confidence interval does not contain the true mean.


#PC4. Run a simulation.
#(a) Create a vector of 10,000 randomly generated numbers that are uniformly distributed between 0 and 9.
random.vector<-runif(n=10000, min=0, max=9)

#(b) Take the mean of that vector. Draw a histogram.
mean(random.vector)
hist(random.vector)
sd(random.vector)

#Create 100 random samples of 2 items each from your randomly generated data
#and take the mean of each sample. Create a new vector that contains those means. 
#Describe/display the distribution of those means.

mean.of.two<-function(i){
  my.sample<-sample(random.vector, 2)
  mean(my.sample)
}

sapply(rep(1,100), mean.of.two)
hist(sapply(rep(1,100), mean.of.two))
#OK, so that creates a function that allows us to do that repeatedly. 
#If I want to save a set of those, I need to create a new vector

means.from.random.vector<-sapply(rep(1,100), mean.of.two)

summary(means.from.random.vector)
hist(means.from.random.vector)

#(d) Do (c) except make the items 10 items in each sample instead of 2. 
#Then do (c) again except with 100 items. Be ready to describe how the 
#histogram changes as the sample size increases.

mean.of.ten<-function(i){
  my.sample<-sample(random.vector, 10)
  mean(my.sample)
}

hist(sapply(rep(1,100), mean.of.ten))

mean.of.hundred<-function(i){
  my.sample<-sample(random.vector, 100)
  mean(my.sample)
}

hist(sapply(rep(1,100), mean.of.hundred))

#Well, I created some functions. Not sure how much they will contribute to mako's happiness.
##How do I create a function where I can tell R how many numbers to create a mean from?
#So I don't have to create 3 separate functions? 

mean.of.x<-function(sample.size) {
  my.sample<-sample(random.vector, sample.size)
  mean(my.sample)
}

hist(replicate(100, {mean.of.x(sample.size=2)}))
hist(replicate(100, {mean.of.x(sample.size=10)}))
hist(replicate(100, {mean.of.x(sample.size=100)}))
##How do I know if this is doing what I want it to be doing, taking the mean of 2, 10, or 100 observations?



#PC5. same as PC4 but with normally distributed vector.
#a.
normal.distribution.vector<-rnorm(n=1000, mean=42, sd=42)

#b.
mean(normal.distribution.vector)
#43.99265
mean(random.vector)
#4.511

hist(normal.distribution.vector)
hist(random.vector)


#c.
mean.of.ndv<-function(sample.size) {
  my.sample<-sample(normal.distribution.vector, sample.size)
  mean(my.sample)
}

hist(replicate(100, {mean.of.ndv(sample.size=2)}))


#d.

hist(replicate(100, {mean.of.ndv(sample.size=10)}))
hist(replicate(100, {mean.of.ndv(sample.size=100)}))

#or
hist(sapply(rep(1,100), function(x) {mean(sample(normal.distribution.vector,2))}))
hist(sapply(rep(1,100), function(x) {mean(sample(normal.distribution.vector,10))}))
hist(sapply(rep(1,100), function(x) {mean(sample(normal.distribution.vector,100))}))

#if you look at the variance, it gets smaller ever time (even though normal distribution looks similar)