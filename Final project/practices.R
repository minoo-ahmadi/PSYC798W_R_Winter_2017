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

n.sim <- 100
sample.ns <- 1:100
betas <- c( 0.87, 0.78, 0.41)
RE <- 5

power.analysis.results <- data.frame(sample.size = sample.ns, power = NA)
results <- data.frame(iteration = 1:n.sim, p = NA)

# pvals <- matrix(NA, nrow=n.sim, ncol=3)
## First set up the design
for (j in sample.ns) {
  n.sim <- 100
  design <- expand.grid(n.sim= sample.ns[j], BFNE= NA, SPS= NA, SIAS= NA,)
  ## simulate data under the null hypothesis.
  for (i in 1:n.sim) {
    design$response <- c(rnorm(3*sample.ns[j]), rnorm(sample.ns[j], mean=2))
    ## fit the model
    fit <- aov(response~A*B, data=design)
    ## record the p-value for the interaction term.
    ## You could record other values.
    ## Save the p value
    pvals[i,j] <- summary(fit)[[1]][[5]][3]
  }
}
Power <- apply(pvals, 2, function (x) length(which(x < 0.05))/n.sim)
names(Power) <- as.character(sample.ns)
print(Power)




x = 1:20
a = 2
b = 1 
y_det = a + b * x
y = y_det+rnorm(20,sd=2) # or more general: y = rnorm(20, mean = y_det, sd = 2)
df <- data.frame(x, y)
ggplot(df, aes(x, y))+ geom_point()+ geom_smooth(method = "lm")

y_det = a + b * x 
y = rnorm(N, mean = y_det, sd = sd)
m = lm(y ~ x) 
coef(summary(m))["x", "Pr(>|t|)"] 

nsim = 400 
pval = numeric(nsim) 
for (i in 1:nsim) { 
y_det = a + b * x
y = rnorm(N, mean = y_det, sd = sd)
m = lm(y ~ x)
pval[i] = coef(summary(m))["x", "Pr(>|t|)"]
} 
sum(pval < 0.05)/nsim



nsim = 400 
N = 100
sd = 2
x = 1:20
a = 2
bvec = seq(-2, 2, by = 0.1) 
pval = numeric(nsim)
power.b = numeric(length(bvec)) 
for (j in 1:length(bvec)) { 
  b = bvec[j] 
  for (i in 1:nsim) { 
    y_det = a + b * x 
    y = rnorm(N, mean = y_det, sd = sd) 
    m = lm(y ~ x) 
    pval[i] = coef(summary(m))["x", "Pr(>|t|)"] 
    } 
  power.b[j] = sum(pval < 0.05)/nsim 
} 


nsim = 400 
N = 27
sd = 2
x = 1:20
a = 2
bvec = seq(-2, 2, by = 0.1) 
pval = numeric(nsim)
power.b = numeric(length(bvec)) 
for (j in 1:length(bvec)) { 
  b = bvec[j] 
  for (i in 1:nsim) { 
    y_det = a + b * x 
    y = rnorm(N, mean = y_det, sd = sd) 
    m = lm(y ~ x) 
    pval[i] = coef(summary(m))["x", "Pr(>|t|)"] 
  } 
  power.b[j] = sum(pval < 0.05)/nsim 
} 


set.seed(1)
repetitions = 100
N = 10000
n = N/8
var1  = c(   .03,    .03,    .03,    .03,    .06,    .06,    .09,   .09)
var2  = c(     0,      0,      0,      1,      0,      1,      0,     1)
rates = c(0.0025, 0.0025, 0.0025, 0.00395, 0.003, 0.0042, 0.0035, 0.002)
var1    = rep(var1, times=n)
var2    = rep(var2, times=n)
var12   = var1**2
var1x2  = var1 *var2
var12x2 = var12*var2
significant = matrix(nrow=repetitions, ncol=7)
startT = proc.time()[3]
for(i in 1:repetitions){
  responses          = rbinom(n=N, size=1, prob=rates)
  model              = glm(responses~var1+var2+var12+var1x2+var12x2, 
                           family=binomial(link="logit"))
  significant[i,1:5] = (summary(model)$coefficients[2:6,4]<.05)
  significant[i,6]   = sum(significant[i,1:5])
  modelDev           = model$null.deviance-model$deviance
  significant[i,7]   = (1-pchisq(modelDev, 5))<.05
}
endT = proc.time()[3]
endT-startT
sum(significant[,1])/repetitions      # pre-specified effect power for var1
sum(significant[,2])/repetitions      # pre-specified effect power for var2
sum(significant[,3])/repetitions      # pre-specified effect power for var12
sum(significant[,4])/repetitions      # pre-specified effect power for var1X2
sum(significant[,5])/repetitions      # pre-specified effect power for var12X2
sum(significant[,7])/repetitions      # power for likelihood ratio test of model
sum(significant[,6]==5)/repetitions   # all effects power
sum(significant[,6]>0)/repetitions    # any effect power
sum(significant[,4]&significant[,5])/repetitions   # power for interaction terms
