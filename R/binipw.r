# binIPW <- function(data, outcome, exposure, variables, wtype, estimate, alpha = 0.05) {
#
#     n <- nrow(data)
#     X <- as.matrix(cbind(1, data[, variables]))
#     fct <- function(x) t(rbind(x)) %*% rbind(x)
#
#     formT <- as.formula(paste(exposure, "~", paste(variables, collapse = " + ")))
#     modT <- glm(formT, data = data, family = "binomial")
#     data$pT <- predict(modT, type = "response")
#     if (wtype[1] == "ATE") {
#         data$w <- 1/ifelse(data[, exposure] == 1, data$pT, 1 - data$pT)
#     } else if (wtype[1] == "ATT") {
#         data$w <- ifelse(data[, exposure] == 1, 1, data$pT/(1 - data$pT))
#     }
#
#     A1 <- sum(data$w*data[, exposure]*data[, outcome])
#     B1 <- sum(data$w*data[, exposure])
#     P1 <- A1/B1
#     A0 <- sum(data$w*(1-data[, exposure])*data[, outcome])
#     B0 <- sum(data$w*(1-data[, exposure]))
#     P0 <- A0/B0
#
#     x <- t(apply(X, 1, fct))
#     pTx <- sweep(x, 1, data$pT*(1-data$pT), "*")
#     mpTx <- array(colSums(pTx), dim=c(length(vars)+1, length(vars)+1))
#     mpTxinv <- solve(mpTx)
#
#     if (wtype[1] == "ATE") {
#         y1 <- sweep(X, 1, data[, exposure]*(1/data$pT-1), "*")
#         y1 <- sweep(y1, 1, data[, outcome] - P1, "*")
#         my1 <- colSums(y1)
#         gamma11 <- mpTxinv %*% my1
#         xgamma11 <- X%*%gamma11
#         xgamma11 <- sweep(xgamma11, 1, data[, exposure]-data$pT, "*")
#         E1 <- ((data[, outcome] - P1)*data[, exposure]/data$pT - xgamma11)
#
#         y0 <- sweep(X, 1, (1-data[, exposure])*(data$pT/(1-data$pT)), "*")
#         y0 <- sweep(y0, 1, data[, outcome] - P0, "*")
#         my0 <- colSums(y0)
#         gamma10 <- mpTxinv %*% my0
#         xgamma10 <- X%*%gamma10
#         xgamma10 <- sweep(xgamma10, 1, data[, exposure]-data$pT, "*")
#         E0 <- ((data[, outcome] - P0)*(1-data[, exposure])/(1-data$pT) + xgamma10)
#     } else if (wtype[1] == "ATT") {
#         E1 <- data[, exposure]*(data[, outcome]-P1)/(mean(data[, exposure]))
#
#         nt <- sum((1-data[, exposure])*data$pT/(1-data$pT))/n
#         y0 <- sweep(X, 1, (1-data[, exposure])*(data$pT/(1-data$pT)), "*")
#         y0 <- sweep(y0, 1, data[, outcome] - P0, "*")
#         my0 <- colSums(y0)
#         gamma20 <- mpTxinv %*% my0
#         xgamma20 <- X%*%gamma20
#         xgamma20 <- sweep(xgamma20, 1, data[, exposure]-data$pT, "*")
#         E0 <- ((data[, outcome] - P0)*(1-data[, exposure])*data$pT/(1-data$pT) + xgamma20)/(nt)
#     }
#
#     if (estimate[1] == "log(OR)") {
#         coefficient <- logit(P1) - logit(P0)
#         E0.or <- E0/(P0*(1-P0))
#         E1.or <- E1/(P1*(1-P1))
#         U.or <- E1.or - E0.or
#         std <- sqrt(var(U.or)/n)
#     } else if (estimate[1] == "log(RR)") {
#         coefficient <- log(P1) - log(P0)
#         E0.rr <- E0/(P0)
#         E1.rr <- E1/(P1)
#         U.rr <- E1.rr - E0.rr
#         std <- sqrt(var(U.rr)/n)
#     } else if (estimate[1] == "RD") {
#         coefficient <- P1 - P0
#         U.rd <- E1 - E0
#         std <- sqrt(var(U.rd)/n)
#     }
#
#     ci <- coefficient + qnorm(c(alpha/2, 1-alpha/2))*c(std)
#     p.value <- 2*(1-pnorm(abs(coefficient/std)))
#
#     return(list(coefficient = coefficient, std = std, ciinf = ci[1], cisup = ci[2], p.value = p.value))
# }
#
# contIPW <- function(data, outcome, exposure, variables, wtype, alpha = 0.05) {
#     binIPW(data, outcome, exposure, variables, wtype, estimate = "RD", alpha = alpha)
# }
