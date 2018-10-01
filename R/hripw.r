#' Hazard ratio estimation using Cox model weighted by the propensity score
#'
#' This function allows estimating the log hazard ratio associated with a binary exposure using a Cox PH model weighted by the propensity score. Propensity model is estimated using a simple logistic regression. Variance estimation takes into account the propensity score estimation step with the method proposed by Hajage *et al.* (2018) \doi{10.1002/bimj.201700330}. Both the average treatment effect on the overall (ATE) or the treated (ATT) population can be estimated. For ATE, both unstabilized and stabilized weights can be used. Ties are handled throught the Breslow approximation.
#'
#' @param data The data.frame to be analyzed.
#' @param time A character string indicating the name of the follow up times column.
#' @param status A character string indicating the name of the failure column (0=right censored, 1=event).
#' @param exposure A character string indicating the name of the exposure column (0=unexposed, 1=exposed).
#' @param variables A character vector indicating the names of the confounders to be accounted for in the propensity model.
#'   Only numeric variables could be handled, so factors, characters and other eligible classes should be expanded into dummy/indicator variables.
#' @param wtype A character string indicating the type of weights that should be used
#'   ('ATE-unstab' for unstabilized ATE weights, 'ATE-stab' for stabilized ATE weights, or 'ATT' for ATT weights)
#' @param alpha A numeric value indicating the (bilateral) alpha level used for the computation of the confidence interval
#'
#' @return A list with the following elements:
#' * coefficient: the log-HR associated with the exposure
#' * std: the standard error
#' * ciinf and cisup: lower and upper limits of the (1-alpha) condidence interval
#' * p.value: the p-value
#' @md
#'
#' @importFrom stats as.formula complete.cases glm pnorm predict qnorm var
#' @importFrom survival coxph Surv
#' @export
#' @references
#’   Hajage D, Chauvet G, Belin L, Lafourcade A, Tubach F, De Rycke Y
#'   Closed-form variance estimator for weighted propensity score estimators with survival outcome.
#'   Submitted to Statistics in Medicine (2017).
#'
#'   Austin PC. Variance estimation when using inverse probability of treatment weighting (IPTW) with survival analysis.
#'   Statistics in medicine, 30;35(30):5642-5655, 2016. <doi: 10.1002/sim.7084>
#'
#'   Lin DY, Wei LJ. The Robust Inference for the Cox Proportional Hazards Model.
#'   Journal of the American Statistical Association 84(408):1074-1078, 1989. <doi: 10.1080/01621459.1989.10478874>
#' @examples
#' ## Using a simulated cohort
#' data(hrData, package = "hrIPW")
#' hrIPW(hrData, time = "time", status = "status", exposure = "Trt",
#'       variables = paste("X", 1:9, sep = ""), wtype = "ATE-stab")
#'
#' # Standard error could be compared with the (robust) Lin's standard error
#' # which does not take into account the propensity score estimation step:
#' modT <- glm(Trt ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = hrData, family = "binomial")
#' probT <- predict(modT, type = "response")
#' hrData$w <- 1/ifelse(hrData$Trt == 1, probT, 1 - probT)
#' library(survival)
#' coxph(Surv(time, status) ~ Trt + cluster(id), data = hrData, method = "breslow", weights = w)
#'
#' # or with the bootstrap-based standard-error (see Austin 2016):
#' \dontrun{
#' f.boot <- function(data, i, wtype) {
#'     df <- data[i, ]
#'     modT <- glm(Trt ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data = df, family = "binomial")
#'     probT <- predict(modT, type = "response")
#'     df$w <- 1/ifelse(df$Trt == 1, probT, 1 - probT)
#'
#'     return(coxph(Surv(time, status) ~ Trt, data = df, weights = w)$coef)
#' }
#'
#' library(boot); set.seed(1234)
#' rcoefs <- boot(data = hrData, statistic = f.boot, R = 500)$t
#' sd(rcoefs)
#' }
#'
#' ## Using the DIVAT data base (package IPWsurvival, to be installed)
#' data(DIVAT, package = "IPWsurvival")
#' hrIPW(data = DIVAT, time = "times", status = "failures", exposure = "ecd",
#'       variables = c("age", "hla", "retransplant"), wtype = "ATE-unstab")
#'
#' # Standard error could be compared with the (robust) Lin's standard error
#' # which does not take into account the propensity score estimation step:
#' modT <- glm(ecd ~ age + hla + retransplant, data = DIVAT, family = "binomial")
#' probT <- predict(modT, type = "response")
#' DIVAT$w <- 1/ifelse(DIVAT$ecd == 1, probT, 1 - probT)
#' DIVAT$id <- 1:nrow(DIVAT)
#' coxph(Surv(times, failures) ~ ecd + cluster(id), data = DIVAT, method = "breslow", weights = w)
#'
#' # or with the bootstrap-based estimation (see Austin 2016):
#' \dontrun{
#' f.boot2 <- function(data, i, wtype) {
#'     df <- data[i, ]
#'     modT <- glm(ecd ~ age + hla + retransplant, data = df, family = "binomial")
#'     probT <- predict(modT, type = "response")
#'     df$w <- 1/ifelse(df$ecd == 1, probT, 1 - probT)
#'
#'     return(coxph(Surv(times, failures) ~ ecd, data = df, weights = w)$coef)
#' }
#'
#' set.seed(1234)
#' rcoefs2 <- boot(data = DIVAT, statistic = f.boot2, R = 500)$t
#' sd(rcoefs2)
#' }
hrIPW <- function(data, time, status, exposure, variables, wtype, alpha = 0.05) {

    data <- data[, c(time, status, exposure, variables)]
    na <- !complete.cases(data)
    if (any(na)) {
        warning(paste(sum(na), "observation(s) dropped."))
        data <- data[!na, ]
    }

    formT <- as.formula(paste(exposure, "~", paste(variables, collapse = " + ")))
    modT <- glm(formT, data = data, family = "binomial")
    data$pT <- predict(modT, type = "response")
    if (wtype[1] == "ATE-unstab") {
        data$w <- 1/ifelse(data[, exposure] == 1, data$pT, 1 - data$pT)
    } else if (wtype[1] == "ATE-stab") {
        data$w <- ifelse(data[, exposure] == 1, mean(data[, exposure]), 1 - mean(data[, exposure]))/ifelse(data[, exposure] == 1, data$pT, 1 - data$pT)
    } else if (wtype[1] == "ATT") {
        data$w <- ifelse(data[, exposure] == 1, 1, data$pT/(1 - data$pT))
    }

    formSurv <- as.formula(paste("Surv(", time, ", ", status, ") ~", exposure))
    coefficient <- unname(coxph(formSurv, data = data, weights = data$w)$coefficients)

    fct <- function(x) t(rbind(x)) %*% rbind(x)
    data <- data[order(data[, time]),]
    n <- nrow(data)
    X <- as.matrix(cbind(1, data[, variables]))

    wegT <- data$w*exp(coefficient*data[, exposure])
    TwegT <- data[, exposure]*wegT
    s0 <- (sum(wegT) - cumsum(wegT) + wegT)/n
    s1 <- (sum(TwegT) - cumsum(TwegT) + TwegT)/n
    R <- s1/s0

    R2 <- sapply(seq_along(data[, time]), function(i) mean((data$w*data[, status]*as.numeric(data[, time][i] >= data[, time])/s0)*(data[, exposure][i] - R)))

    XX <- t(apply(X, 1, fct))
    pTXX <- sweep(XX, 1, data$pT*(1 - data$pT), '*')
    A <- array(colSums(pTXX), dim = c(length(variables) + 1, length(variables) + 1))/n
    Ainv <- solve(A)

    U0 <- data[, status]*(data[, exposure] - R) - exp(coefficient*data[, exposure])*R2

    if (wtype[1] == "ATE-unstab") {
        c <- 0
        bint <- (-data[, exposure]*(1 - data$pT)/data$pT + (1 - data[, exposure])*data$pT/(1 - data$pT))*U0
    } else if (wtype[1] == "ATE-stab") {
        c <- mean((data[, exposure]/data$pT - (1 - data[, exposure])/(1 - data$pT))*U0)
        prev <- mean(data[, exposure])
        bint <- (-data[, exposure]*prev*(1 - data$pT)/data$pT + (1 - data[, exposure])*data$pT*(1 - prev)/(1 - data$pT))*U0
    } else if (wtype[1] == "ATT") {
        c <- 0
        bint <- ((1 - data[, exposure])*data$pT/(1 - data$pT))*U0
    }
    bint <- sweep(X, 1, bint, '*')
    bint <- colSums(bint)/n
    b <- Ainv %*% bint
    B <- mean(data$w*data[, status]*R*(1 - R))
    U <- (data$w*U0 + c*(data[, exposure] - data$pT) + sweep(X, 1, data[, exposure] - data$pT, '*')%*%b)/B
    U <- U[, 1]

    std <- sqrt(var(U)/n)
    ci <- coefficient + qnorm(c(alpha/2, 1-alpha/2))*c(std)
    p.value <- 2*(1-pnorm(abs(coefficient/std)))

    return(list(coefficient = coefficient, std = std, ciinf = ci[1], cisup = ci[2], p.value = p.value))
}

