####################################################
#               Common Code
####################################################

total_students <- 316

videodata <- read.table("videodata.txt", header = T)

get_good_responses <- function(x) {
        videodata[which(x != 99), ]
}

bootstrap <- function(x, n) {
        bootstrap_population <- rep(x, each = round(total_students/(length(x))))
        replicate(n, sample(bootstrap_population, length(x), replace = TRUE))
}

skewness <- function(x) {
        sum <- 0
        for (value in x) {
                sum <- sum + (((value-mean(x))/sd(x))^3)
        }
        sum/length(x)
}

kurtosis <- function(x) {
        sum <- 0
        for (value in x) {
                sum <- sum + (((value-mean(x))/sd(x))^4)
        }
        sum/length(x)
}


####################################################
#               Scenario 1
#  - How many students (out of a total of 314)
#    probably played video games in the week before
#    the survey?
####################################################

s1_clean <- get_good_responses(videodata$time)

samples <- bootstrap(s1_clean$time, 10000)

played <- apply(samples, 2, function(x) {sum(x > 0)})

pcts <- played / length(s1_clean$time)

mean_pct <- mean(pcts)
sd_pct <- sd(pcts)

pct_skew <- skewness(pcts)
pct_kurtosis <- kurtosis(pcts)

stderr_pct <- sd_pct / sqrt(length(s1_clean$time))
z <- qnorm(.975)

point_estimate_mean <- mean_pct
lower_interval_95 <- (mean_pct - (z * stderr_pct))
upper_interval_95 <- (mean_pct + (z * stderr_pct))

hist(pcts, main="Bootstrapped Percentage of Students Who \nPlayed Video Games in the Previous Week", 
                xlab="Percentage",
                col="deepskyblue4",
                freq=TRUE
)

####################################################
#               Scenario 2
#  - How does the amount of time spent playing 
#    videogames in the week prior to the survey
#    compare to the reported frequency of play?
####################################################

s2_time_clean <- get_good_responses(videodata$time)
s2_freq_clean <- get_good_responses(videodata$freq)

s2_time_mean <- mean(s2_time_clean$time)

time_counts <- as.data.frame(table(s2_time_clean$time))

barplot_time <- barplot(time_counts$Freq,
                        names.arg=c(0, 0.1, 0.5, 1, 1.5, 2, 3, 4, 5, 14, 30),
                        col="deepskyblue4",
                        main="Reported Hours of Play",
                        ylim=c(0, 60),
                        lab="Hours",
                        ylab="Frequency")

freq_counts <- as.data.frame(table(s2_freq_clean$freq))

barplot_freq <- barplot(freq_counts$Freq,
                        names.arg=c("Daily", "Weekly", "Montly", "Semesterly"),
                        col="deepskyblue4", main="Reported Frequency of Play", 
                        ylim=c(0, 30), 
                        ylab="Frequency")
text(barplot_freq, freq_counts$Var1 , paste(freq_counts$Freq), cex=1)

####################################################
#               Scenario 3
#  - How much time did Stats 2 students spend 
#    playing videogames?
####################################################

s3_time_clean <- get_good_responses(videodata$time)

obs_mean <- mean(s3_time_clean$time)

simulations <- bootstrap(s3_time_clean$time, 10000)

sample_means <- apply(simulations, 2, mean)

mean_means <- mean(sample_means)
sd_means <- sd(sample_means)
skew_means <- skewness(sample_means)
kurtosis_means <- kurtosis(sample_means)

n <- 91
margin_of_error <- qnorm(0.975) * sd_means / sqrt(n)
left <- mean_means - margin_of_error
right <- mean_means + margin_of_error

hist(sample_means, 
     main="Bootstrapped Average Amount of Time Played",
     xlab="Averages",
     col="deepskyblue4",
     freq=FALSE)
curve(dnorm(x, mean=mean(sample_means), sd=sd(sample_means)), add=TRUE, col="red", lwd=2)
points(x=obs_mean, y=0, pch=1, cex=2, col="red", lwd=2)
points(x=left, y=0, pch=4, cex=2, col="red", lwd=2)
points(x=right, y=0, pch=5, cex=2, col="red", lwd=2)
legend(2.5, 0.8, legend=c("Observed Mean", "Left", "Right"), pch=c(1, 4, 5), cex=1.5, col="red")


####################################################
#               Scenario 4
####################################################





####################################################
#               Scenario 5
####################################################

