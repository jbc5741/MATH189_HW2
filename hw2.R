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
####################################################





####################################################
#               Scenario 3
####################################################





####################################################
#               Scenario 4
####################################################





####################################################
#               Scenario 5
####################################################

