
set.seed(8)

y <- rnorm(50)
x1 <- rnorm(50)
x2 <- x1 + runif(50)

# run multiple regression

model <- lm(y ~ x1 +x2)

#get loadings
sum_x1 <- summary(model); beta_x1

beta_x1 <- summary(model)$coefficients[2, 1]

sum_x2 <- summary(model); sum_x2

beta_x2 <- summary(model)$coefficients[3, 1]

#compute factor
composite <- beta_x1 * x1 + beta_x2 * x2

summary(lm(y ~ composite))
