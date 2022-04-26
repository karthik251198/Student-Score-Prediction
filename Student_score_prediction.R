library(dplyr)
library(caTools)
library(ggplot2)
library(Amelia)

df <- read.csv('StudentsPerformance.csv')
list_na <- colnames (df)[ apply(df, 2, anyNA) ]
df <-df %>%
  + na.omit()
print(head(df))

average_missing <- apply(df[,colnames(df) %in% list_na],
                         + 2,
                         + mean,
                         + na.rm = TRUE)
print(average_missing)

df_replace <- df %>%
  + mutate(replace_mean_math=ifelse(is.na(math.score), average_missing[1], math.score),
           + replace_mean_reading = ifelse(is.na(reading.score), average_missing[2], reading.score),replace_mean_writing = ifelse(is.na(writing.score), average_missing[3], writing.score))

sum(is.na(df_replace$replace_mean_math))
sum(is.na(df_replace$replace_mean_reading))
sum(is.na(df_replace$replace_mean_writing))

ggplot(aes(df_replace$replace_mean_math)) + geom_histogram(aes(factor=df$gender)
ggplot(aes(df_replace$replace_mean_reading)) + geom_histogram(aes(factor=df$gender)
ggplot(aes(df_replace$replace_mean_writing)) + geom_histogram(aes(factor=df$gender)

df <- select(df_replace,-'math.score',-'reading.score',-'writing.score')
sample <- sample.split(df$replace_mean_math, SplitRatio = 0.70)
train = subset (df, sample == TRUE)
test = subset (df, sample == FALSE)
model <- lm(replace_mean_math ~ .,train)
plot(model)
 
math.predictions <- predict(model,test)
results <- cbind(math.predictions,test$replace_mean_math)
colnames(results) <- c('pred','real')

results <- as.data.frame(results)
print(head(results))

SSE = sum((results$pred - results$real)^2)
SST = sum((mean(df$replace_mean_math) - results$real)^2)
R2 = (1 - SSE/SST) 
print(R2)
