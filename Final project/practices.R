# some practice to prepare for the final project

reps <- 1000
design <- expand.grid(A=c("a", "b"), B=c("c","d"), reps=1:10)
pvals <- vector("numeric", length=reps)
## simulate data under the null hypothesis.
for (i in 1:reps) {
  design$response <- rnorm(40) # random data with zero mean
  ## fit the model
  fit <- aov(response~A*B, data=design)
  ## record the p-value for the interaction term.
  ## You could record other values.
  ## Save the p value
  pvals[i] <- summary(fit)[[1]][[5]][3]
}
Type1 <- length(which(pvals < 0.05))/reps
print(Type1)


ssize <- c(5, 10, 20)
pvals <- matrix(NA, nrow=reps, ncol=3)
## First set up the design
for (j in 1:3) {
  reps <- 1000
  design <- expand.grid(reps=1:ssize[j], A=c("a", "b"), B=c("c","d"))
  ## simulate data under the null hypothesis.
  for (i in 1:reps) {
    design$response <- c(rnorm(3*ssize[j]), rnorm(ssize[j], mean=2))
    ## fit the model
    fit <- aov(response~A*B, data=design)
    ## record the p-value for the interaction term.
    ## You could record other values.
    ## Save the p value
    pvals[i,j] <- summary(fit)[[1]][[5]][3]
  }
}
Power <- apply(pvals, 2, function (x) length(which(x < 0.05))/reps)
names(Power) <- as.character(ssize)
print(Power)

# Construct a graph of the relationship between power and sample size for a
#multiple regression model with 3 predictor variables, over a range of 1 to
#10 for each predictor. For the effect size, let the residual error have σ = 5,
#and β1 = 0.1, β2 = 1 and β3 = 5. Try varying the effect size to examine
#its effects on power in this case.



