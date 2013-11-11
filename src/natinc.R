## Relation between income and nationalism in Catalonia
## @griverorz
## Sat Oct 19 12:44:08 PDT 2013

library(foreign)
library(car)
library(arm)
library(nnet)
library(MASS)
library(rjags)
library(reshape2)
theme_set(theme_bw())

setwd("~/Documents/datablog/natinc")
set.seed(20131019)

#################### load data ####################
load("./dta/dfcat.RData")

dfcat <- dfcat[, c("org", "pkincome", "age", "educ",
                   "sizetown", "nat", "gender")]
dfcat <- na.omit(dfcat)

## Prepare dataset for estimation
dfcat$orgrec <- recode(dfcat$org, "1:2 = 1; 3 = 2; 4 = 3; 5 = 4")
dfcat$pkincomerec <- recode(dfcat$pkincome, "1 = 1; 2 = 2; 3:4 = 3")

## Intercept
intercept <- polr(factor(dfcat$orgrec) ~ 1)

## Reference
reference <- polr(factor(dfcat$orgrec) ~ factor(dfcat$pkincomerec))

## Full model
fullmodel <- polr(factor(org) ~ factor(pkincome) + 
                  I(gender - 1) + I(age - 18) + 
                  educ + factor(sizetown) + factor(nat),
                  data = dfcat, Hess = TRUE)

fullmodel <- polr(factor(orgrec) ~ factor(pkincomerec) + 
                  I(gender - 1) + I(age - 18) + 
                  educ + factor(sizetown) + factor(nat),
                  data = dfcat, Hess = TRUE)

#################### simulations from coefficients ####################
coefs <- coef(summary(fullmodel))[, "Value"]
vcovs <- vcov(fullmodel)
simcoefs <- mvrnorm(10000, coefs, vcovs)
jmodel <- apply(simcoefs, 2, function(x) quantile(x, c(0.025, 0.25,
                                                       0.5, 
                                                       0.75, 0.975)))

#################### plot coefficients ####################
coeffs <- jmodel[, grep("pkincome", names(as.data.frame(jmodel)))]
coeffs <- melt(coeffs)

names(coeffs) <- c("idx", "parameter", "value")
levels(coeffs$parameter) <- c("Income:2", "Income:3")
coeffs <- dcast(coeffs, parameter ~ idx)
names(coeffs) <- c("parameter", "ymin", "yminq", "ymedian", "ymaxq", "ymax")

p <- ggplot(data = coeffs, aes(x = parameter,
                ymin = ymin,
                y = ymedian,
                ymax = ymax))
pq <- p + geom_pointrange() + 
    geom_linerange(aes(ymin = yminq, ymax = ymaxq), size = 1.25) +
    xlab("Parameter") + ylab("Value") +
    scale_color_discrete(name = "Parameter")
ggsave("./img/controls.png", pq)

#################### prediction ####################
newdata <- data.frame(c(0, 0, 0, 1, 1^2, 0, 0, 0, 1, 0, 0, 0, 0, 1), 
                      c(1, 0, 0, 1, 1^2, 0, 0, 0, 1, 0, 0, 0, 0, 1), 
                      c(0, 1, 0, 1, 1^2, 0, 0, 0, 1, 0, 0, 0, 0, 1))

Tau <- simcoefs[,grep("\\|", names(as.data.frame(simcoefs)))]
Beta <- simcoefs[,-grep("\\|", names(as.data.frame(simcoefs)))]

antilogit <- function(x) exp(x)/(1+exp(x))
predict_ologit <- function(tau, beta, newdata) {
    linpred <- apply(beta, 1, function(x) newdata %*% x)
    linpred <- tau - linpred
    
    cumprob <- antilogit(linpred)
    predprob <- matrix(NA, ncol = ncol(tau) + 1, nrow = nrow(beta))
    
    predprob[,1] <- cumprob[,1]
    predprob[,2] <- cumprob[,2] - cumprob[,1]
    predprob[,3] <- cumprob[,3] - cumprob[,2]    
    predprob[,4] <- 1 - cumprob[,3]
    return(predprob)    
}

predsim <- vector("list", ncol(newdata))
for (k in 1:ncol(newdata)) {
    predsim[[k]] <- as.data.frame(predict_ologit(Tau, Beta, newdata[,k]))
    predsim[[k]]$group <- c("poor", "middle", "rich")[k]
}
predsim <- do.call("rbind", predsim)
names(predsim) <- c("first", "second", "third", "fourth", "income")

## plot predicted probability
predsim <- melt(predsim)

p <- ggplot(predsim, aes(x = value, group = income)) 
p + geom_density(aes(colour = income)) + 
    xlab("Group") + ylab("Predicted probability") +
    facet_grid(~ variable)

    scale_color_discrete()

ggsave("./img/predprob.png", pq)

#################### missing data ####################
load("./dta/dfcat.RData")

dfcat <- dfcat[, c("org", "age", "educ", "pkincome",
                   "sizetown", "nat", "gender")]

dfcat$miss <- is.na(dfcat$pkincome)

summary(glm(miss ~ I(gender - 1) + I(age - 18) + 
            educ + factor(sizetown) + factor(nat),
            data = dfcat, family = "binomial"))
