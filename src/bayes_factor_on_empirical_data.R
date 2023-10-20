#testing the ttestBF in r
library(BayesFactor)

# read table from panel_data_classification.csv
df <- read.csv("panel_data_classification.csv", header = TRUE, sep = ",")

# check the data frame
summary(df)

# keep only the columns we need that are Participant.id, Round, participant.in_deception, Decision
df <- df[, c("Participant.id", "Round", "participant.in_deception", "Decision")]
# sort the data frame by Participant.id and Round
df <- df[order(df$Participant.id, df$Round), ]
head(df)
tail(df)

# convert the participant.in_deception column to a factor
df$participant.in_deception <- as.factor(df$participant.in_deception)

# make ttestBF for the first round between participant.in_deception being 0 or 1
t1 <- ttestBF(x = df[df$Round == 1 & df$participant.in_deception == 0, ]$Decision,
        y = df[df$Round == 1 & df$participant.in_deception == 1, ]$Decision,
        nullInterval = c(0, 0), rscale = sqrt(2)/2, iterations = 100000, progress = FALSE)

# make ttestBF for the first 5 rounds between participant.in_deception being 0 or 1
t2 <- ttestBF(x = df[df$Round <= 5 & df$participant.in_deception == 0,]$Decision,
        y = df[df$Round <= 5 & df$participant.in_deception == 1,]$Decision, 
        nullInterval = c(0, 0), rscale = sqrt(2)/2, iterations = 100000, progress = FALSE)

# make ttestBF for the last 5 rounds between participant.in_deception being 0 or 1
t3 <- ttestBF(x = df[df$Round > 5 & df$participant.in_deception == 0,]$Decision,
        y = df[df$Round > 5 & df$participant.in_deception == 1,]$Decision, 
        nullInterval = c(0, 0), rscale = sqrt(2)/2, iterations = 100000, progress = FALSE)
bf_list <- list("BF1" =as.vector(t1),"BF1" =as.vector(t1))
as.vector(t1)
as.vector(t1)[2][[1]]
# take the shape of a vector
t1 <- as.vector(t1)

# take the second element of the vector
t1 <- t1[2]

print(t1)
print(t2)
print(t3)
# all the test are smaller than 0
# this implies that the null hypothesis is true
# that means, we cannot distinguish whether the two samples are from two different distributions
# this is correct as we sampled the data from the same distribution

# loop through all the rounds and make ttestBF between participant.in_deception being 0 or 1
# and save the results in a list
t_list <- list()
for (i in 1:11) {
  temp <- ttestBF(x = df[df$Round == i & df$participant.in_deception == 0, ]$Decision,
                         y = df[df$Round == i & df$participant.in_deception == 1, ]$Decision,
                         nullInterval = c(0, 0), rscale = sqrt(2)/2, iterations = 100000, progress = FALSE)
  temp <- as.vector(temp)
  temp <- temp[2][[1]] #we only need the Bayes factor
  t_list[[i]] <-temp #we store the Bayes factor in the list
}

# make a plot of the Bayes factor for each round
# use ggplot2 that makes nice graphs
library(ggplot2)
# convert the list to a data frame
t_df <- data.frame(Round = 1:11, BF = unlist(lapply(t_list, function(x) x[1])))
# make the plot
ggplot(t_df, aes(x = Round, y = BF)) + geom_point() + geom_line() + geom_hline(yintercept = 1, linetype = "dashed") + theme_bw()
# add one horizontal line at y = 1
# make the y range between 0.01 and 5
# add a vertical line at round 6
# make the ticks and ticks label larger
# make a tight layout
# chan
ggplot(t_df, aes(x = Round, y = BF)) + 
        geom_point() + geom_line() + 
        geom_hline(yintercept = 1, color="red") + 
        theme_bw()  + 
        geom_vline(xintercept = 6, linetype = "dashed") + 
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) + 
        coord_cartesian(ylim = c(0, 1.5)) +
        labs(y = "Bayes factor") + 
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

bf_before <- as.vector(t2)[2][[1]]
bf_after <- as.vector(t3)[2][[1]]
# add two diamond points to the plot
# one for the Bayes factor before at round 3
# and one for the  Bayes factor after at round 9
# make the diamonds grey and larger
# add a legend
ggplot(t_df, aes(x = Round, y = BF)) + 
        geom_point(color = "grey") + geom_line() + 
        geom_hline(yintercept = 1, color="red") + 
        theme_bw()  + 
        geom_vline(xintercept = 6, linetype = "dashed") + 
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 16)) + 
        coord_cartesian(ylim = c(0, 1.5)) +
        labs(y = "Bayes factor") + 
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
        geom_point(aes(x = 3, y = bf_before), shape = 18, size = 3, color = "black") +
        geom_point(aes(x = 9, y = bf_after), shape = 18, size = 3, color = "black") +
        labs(color = "Bayes factor") +
        theme(legend.position = "bottom")
# save the plot as a pdf
ggsave("bayes_factor.pdf", width = 5, height = 5)

