library(purrr)


df <- read.csv("mls-salaries-2017.csv")
# remove second listed position
df$position <- as.vector(map(strsplit(as.character(df$position), "-"), 1))
df$position <- as.vector(map(strsplit(as.character(df$position), "/"), 1))
# remove outliers (9 highest earners)
df <- df[-tail(order(df$guaranteed_compensation), 9),]



x <- df[df$position == "F", "base_salary"]
y <- df[df$position == "D", "base_salary"]

n_x <- length(x)
n_y <- length(y)
m <- 1000000

t <- abs(mean(x) - mean(y))

z <- c(x, y)
permutation_samples <- matrix(ncol = n_x+n_y, nrow = m)
for (i in 1:m) {
  permutation_samples[i,] <- sample(z, (n_x+n_y), replace = FALSE)
}
permutation_x_samples <- permutation_samples[,1:n_x]
permutation_y_samples <- permutation_samples[,(n_x+1):(n_x+n_y)]

t_hats <- abs(apply(permutation_x_samples, 1, mean) - apply(permutation_y_samples, 1, mean))
k <- sum(t_hats > t)
p_value_est <- (k+1)/(m+1)


### 



