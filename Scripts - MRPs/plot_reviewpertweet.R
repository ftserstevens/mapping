library(progress)
library(ggplot2)

ntweets = 2000:8000
nlucid = 2000
minreview = 5
nreview = 20
ntwreviews = nreview * nlucid

avg = ntwreviews/ntweets
p = 1/ntweets
effreviews = as.vector(NA)

pb <- progress_bar$new(total = length(ntweets))

for( i in seq_along(ntweets)) {
  chosen_t = replicate(nlucid, 
                       sample.int(ntweets[i], size = nreview, replace = TRUE)
                       ) 
  
  
  effreviews[i] = mean(table(chosen_t) >= minreview) * ntweets[i]
  pb$tick()
  
}
pb$terminate()

df = as.data.frame(cbind(ntweets, effreviews, avg))
names(df) = c("ntweets","effreviews", "average")

plot(ntweets, effreviews, xlab = "Number of Tweets", ylab = "Effective Reviews")

ggplot(data = df,
       map = aes(y= (effreviews), x = average)) +
  geom_smooth() + theme_minimal() + xlab("Average Reviews per Tweet") +
  ylab("Effective Reviews") +
  geom_vline(xintercept = 5.8) + geom_vline(xintercept = 6.8)

