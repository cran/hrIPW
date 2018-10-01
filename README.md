<!-- README.md is generated from README.Rmd. Please edit that file -->
hrIPW
=====

This function allows estimating the log hazard ratio associated with a
binary exposure using a Cox PH model weighted by the propensity score.
Propensity model is estimated using a simple logistic regression.
Variance estimation takes into account the propensity score estimation
step with the method proposed by Hajage *et al.* (2018)
\doi{10.1002/bimj.201700330}. Both the average treatment effect on the
overall (ATE) or the treated (ATT) population can be estimated. For ATE,
both unstabilized and stabilized weights can be used. Ties are handled
throught the Breslow approximation.

Example
-------

``` r
library(hrIPW)

## Using a simulated cohort
data(hrData, package = "hrIPW")
hrIPW(hrData, time = "time", status = "status", exposure = "Trt",
      variables = paste("X", 1:9, sep = ""), wtype = "ATE-stab")
#> $coefficient
#> [1] -0.01717167
#> 
#> $std
#> [1] 0.03282054
#> 
#> $ciinf
#> [1] -0.08149875
#> 
#> $cisup
#> [1] 0.04715541
#> 
#> $p.value
#> [1] 0.6008359

# Standard error could be compared with the (robust) Lin's standard error
# which does not take into account the propensity score estimation step:
modT <- glm(Trt ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = hrData, family = "binomial")
probT <- predict(modT, type = "response")
hrData$w <- 1/ifelse(hrData$Trt == 1, probT, 1 - probT)
library(survival)
coxph(Surv(time, status) ~ Trt + cluster(id), data = hrData, method = "breslow", weights = w)
#> Call:
#> coxph(formula = Surv(time, status) ~ Trt + cluster(id), data = hrData, 
#>     weights = w, method = "breslow")
#> 
#>        coef exp(coef) se(coef) robust se     z    p
#> Trt -0.0174    0.9827   0.0142    0.0354 -0.49 0.62
#> 
#> Likelihood ratio test=1.52  on 1 df, p=0.2
#> n= 10000, number of events= 10000

# or with the bootstrap-based standard-error (see Austin 2016):
f.boot <- function(data, i, wtype) {
    df <- data[i, ]
    modT <- glm(Trt ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = df, family = "binomial")
    probT <- predict(modT, type = "response")
    df$w <- 1/ifelse(df$Trt == 1, probT, 1 - probT)
    
    return(coxph(Surv(time, status) ~ Trt, data = df, weights = w)$coef)
}

library(boot); set.seed(1234)
#> 
#> Attaching package: 'boot'
#> The following object is masked from 'package:survival':
#> 
#>     aml
rcoefs <- boot(data = hrData, statistic = f.boot, R = 500)$t
sd(rcoefs)
#> [1] 0.03283661

## Using the DIVAT data base (package IPWsurvival, to be installed)
data(DIVAT, package = "IPWsurvival")
hrIPW(data = DIVAT, time = "times", status = "failures", exposure = "ecd",
      variables = c("age", "hla", "retransplant"), wtype = "ATE-unstab")
#> $coefficient
#> [1] 0.4661664
#> 
#> $std
#> [1] 0.2564084
#> 
#> $ciinf
#> [1] -0.03638477
#> 
#> $cisup
#> [1] 0.9687177
#> 
#> $p.value
#> [1] 0.0690546

# Standard error could be compared with the (robust) Lin's standard error
# which does not take into account the propensity score estimation step:
modT <- glm(ecd ~ age + hla + retransplant, data = DIVAT, family = "binomial")
probT <- predict(modT, type = "response")
DIVAT$w <- 1/ifelse(DIVAT$ecd == 1, probT, 1 - probT)
DIVAT$id <- 1:nrow(DIVAT)
coxph(Surv(times, failures) ~ ecd + cluster(id), data = DIVAT, method = "breslow", weights = w)
#> Call:
#> coxph(formula = Surv(times, failures) ~ ecd + cluster(id), data = DIVAT, 
#>     weights = w, method = "breslow")
#> 
#>       coef exp(coef) se(coef) robust se   z     p
#> ecd 0.4650    1.5920   0.0517    0.2579 1.8 0.071
#> 
#> Likelihood ratio test=83.28  on 1 df, p=<2e-16
#> n= 1837, number of events= 582

# or with the bootstrap-based estimation (see Austin 2016):
f.boot2 <- function(data, i, wtype) {
    df <- data[i, ]
    modT <- glm(ecd ~ age + hla + retransplant, data = df, family = "binomial")
    probT <- predict(modT, type = "response")
    df$w <- 1/ifelse(df$ecd == 1, probT, 1 - probT)
    
    return(coxph(Surv(times, failures) ~ ecd, data = df, weights = w)$coef)
}

set.seed(1234)
rcoefs2 <- boot(data = DIVAT, statistic = f.boot2, R = 500)$t
sd(rcoefs2)
#> [1] 0.2885173
```
