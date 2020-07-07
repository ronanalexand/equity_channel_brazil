#### Modelo Bivariado Credito e Return ####
p1ct_cred.12 =  VAR(base_credito, p = 12, type = "none")
summary(p1ct_cred.12,equation = "dsaldo_credPJPF_M")
plot(p1ct_cred.12, names = "dsaldo_credPJPF_M")

serial.test(p1ct_cred.12, lags.pt = 12)
# null hypothesis is well specified,
# the alternative hypothesis is more loosely specified
# reject null hypothesis

normality.test(p1ct_cred.12)
# Null Hypothesis: Residuals are multivariate normal
# reject null hypothesis

arch.test(p1ct_cred.12, lags.multi = 12) 
# H_0 := B_1 = B_2 = … = B_q = 0
# not reject null hypothesis

plot(stability(p1ct_cred.12), names = "dsaldo_credPJPF_M")
# Stable coefficients

var.irf <- vars::irf(p1ct_cred.12, response = "dsaldo_credPJPF_M",impulse = "return_M",
                     n.ahead = 12, boot = TRUE,cumulative = T)
plot(var.irf)

FEVD_cred <- vars::fevd(p1ct_cred.12, n.ahead = 12,impulse = "return")
FEVD_cred

##### Modelo Bivariado  IBC-Br e return ####
p1ct_ibc_br.4 =  VAR(base_ibc_br, p = 1, type = "none")
summary(p1ct_ibc_br.4,equation = "dibc_br_M")
serial.test(p1ct_ibc_br.4, lags.pt = 1, type = "PT.asymptotic")
#  null hypothesis is well specified,
#  the alternative hypothesis is more loosely specified
# reject null hypothesis

normality.test(p1ct_ibc_br.4)
# Null Hypothesis: Residuals are multivariate normal
# reject null hypothesis

arch.test(p1ct_ibc_br.4, lags.multi = 1) 
# H_0 := B_1 = B_2 = … = B_q = 0
# reject null hypothesis

plot(stability(p1ct_ibc_br.4), names = "dibc_br_M")
# Stable coefficients

var.irf.2 <- vars::irf(p1ct_ibc_br.4, response = "dibc_br_M", impulse ="return_M",
                       n.ahead = 12, boot = TRUE,cumulative = T)
plot(var.irf.2)

FEVD_ibc <- vars::fevd(p1ct_ibc_br.4, n.ahead = 12, impulse ="return_M")
FEVD_ibc

##### Modelo Bivariado  Investimento e return ####
p1ct_invest.1 =  VAR(base_invest, p = 1, type = "none")
summary(p1ct_invest.1,equation = "invest")
serial.test(p1ct_invest.1, lags.pt = 1, type = "PT.asymptotic")
#  null hypothesis is well specified,
#  the alternative hypothesis is more loosely specified
# reject null hypothesis

normality.test(p1ct_invest.1)
# Null Hypothesis: Residuals are multivariate normal
# not reject null hypothesis

arch.test(p1ct_invest.1, lags.multi = 1) 
# H_0 := B_1 = B_2 = … = B_q = 0
# reject null hypothesis

plot(stability(p1ct_invest.1), names = "invest")
# not stable coefficients

var.irf.3 <- vars::irf(p1ct_invest.1, response = "invest", impulse ="return_T",
                       n.ahead = 4, boot = TRUE,cumulative = T)
plot(var.irf.3)

FEVD_invest <- vars::fevd(p1ct_invest.1, n.ahead = 4, impulse ="return_T")
FEVD_invest

#####  VAR do modelo Mensal Variações IBC-Br, Saldo Crédito PJ, r_M, Return_M ####
var_mensal =  VAR(base_M, p = 2, type = "none")
summary(var_mensal,equation = "dibc_br_M")
plot(var_mensal, names = "dibc_br_M")
serial.test(var_mensal,lags.pt = 2)
#  null hypothesis is well specified,
#  the alternative hypothesis is more loosely specified
# reject null hypothesis

normality.test(var_mensal)
# Null Hypothesis: Residuals are multivariate normal
# reject null hypothesis

arch.test(var_mensal,lags.multi = 2) 
# H_0 := B_1 = B_2 = … = B_q = 0
# reject null hypothesis

plot(stability(var_mensal), names = "dibc_br_M")
# coeficiente estável

var.irf_M.1 <- vars::irf(var_mensal, impulse = "return_M", n.ahead = 12, boot = TRUE)
plot(var.irf_M.1)

FEVD_ISCRR <- vars::fevd(var_mensal, n.ahead = 12, impulse = "return_M")
FEVD_ISCRR$dibc_br_M

# SVAR para o modelo mensal
amat = diag(4)
amat[2, 1] <- NA
amat[3, 1:2] <- NA
amat[4, 1:3] <- NA

bmat = diag(4)

svar_mensal = vars::SVAR(var_mensal,estmethod = "direct",Amat = amat,Bmat = bmat,
                         hessian = TRUE, method = "BFGS")
svar_mensal.irf.1 <- irf(svar_mensal, impulse = "return_M",
                         n.ahead = 60, boot = TRUE,cumulative = T)
plot(svar_mensal.irf.1)

S_FEVD_ISRR <- vars::fevd(svar_mensal, n.ahead = 60)
S_FEVD_ISRR$dibc_br_M

##### VAR do modelo trimestral PIB, Saldo Crédito PJ&PF, r_T, Return_T ####
var_trimestral =  VAR(base_T, p = 2, type = "none")
summary(var_trimestral,equation = "PIB_br")
plot(var_trimestral, names = "PIB_br")
serial.test(var_trimestral,lags.pt = 2)
#  null hypothesis is well specified,
#  the alternative hypothesis is more loosely specified
# reject null hypothesis

normality.test(var_trimestral)
# Null Hypothesis: Residuals are multivariate normal
# reject null hypothesis

arch.test(var_trimestral,lags.multi = 2) 
# H_0 := B_1 = B_2 = … = B_q = 0
# not reject null hypothesis

plot(stability(var_trimestral), names = "PIB_br")
# Stable coefficients

var.irf_T.1 <- vars::irf(var_trimestral, impulse = "return_T", n.ahead = 12, boot = TRUE)
plot(var.irf_T.1)

FEVD_PSRR <- vars::fevd(var_trimestral, n.ahead = 12, impulse = "return_T")
FEVD_PSRR$PIB_br

# SVAR para o modelo trimestral
amat = diag(4)
amat[2, 1] <- NA
amat[3, 1:2] <- NA
amat[4, 1:3] <- NA

bmat = diag(4)

svar_trimestral = vars::SVAR(var_trimestral,estmethod = "direct",
                             Amat = amat,Bmat = bmat,
                             hessian = TRUE, method = "BFGS")
svar_trimestral.irf.1 <- irf(svar_trimestral, 
                             impulse = "return_T", n.ahead = 20, 
                             boot = TRUE,cumulative = T,runs = 100)
plot(svar_trimestral.irf.1)

S_FEVD_PSRR_T <- vars::fevd(svar_trimestral, n.ahead = 20, impulse = "return_T")
S_FEVD_PSRR_T$PIB_br

##### VAR do modelo trimestral Investimento, Saldo Crédito PJ&PF, r_T, Return_T ####
var_trimestral_invest =  VAR(base_invest_T, p = 2, type = "none")
summary(var_trimestral_invest,equation = "invest")
plot(var_trimestral_invest, names = "invest")
serial.test(var_trimestral_invest,lags.pt = 2)
#  null hypothesis is well specified,
#  the alternative hypothesis is more loosely specified
# reject null hypothesis

normality.test(var_trimestral_invest)
# Null Hypothesis: Residuals are multivariate normal
# reject null hypothesis

arch.test(var_trimestral_invest,lags.multi = 2) 
# H_0 := B_1 = B_2 = … = B_q = 0
# not reject null hypothesis

plot(stability(var_trimestral_invest), names = "invest")
# Stable coefficients

var.irf_T.2 <- vars::irf(var_trimestral_invest, 
                         impulse = "return_T", n.ahead = 12, boot = TRUE)
plot(var.irf_T.2)

FEVD_INSRR <- vars::fevd(var_trimestral_invest, n.ahead = 12, impulse = "return_T")
FEVD_INSRR$invest

# SVAR para o modelo trimestral
amat = diag(4)
amat[2, 1] <- NA
amat[3, 1:2] <- NA
amat[4, 1:3] <- NA

bmat = diag(4)

svar_trimestral = vars::SVAR(var_trimestral_invest,estmethod = "direct",
                             Amat = amat,Bmat = bmat,
                             hessian = TRUE, method = "BFGS")
svar_trimestral.irf.2 <- irf(svar_trimestral, 
                             impulse = "return_T", n.ahead = 20, 
                             boot = TRUE,cumulative = T,runs = 100)
plot(svar_trimestral.irf.2)

S_FEVD_INSRR <- vars::fevd(svar_trimestral, n.ahead = 20, impulse = "return_T")
S_FEVD_INSRR$invest

##### VAR trimestral PIB, Investimento, IPCA, Saldo Crédito PJ&PF, Swap-DI360, Retorno####
var_PIISCR =  VAR(base_PIISCR, p = 3, type = "none")
summary(var_PIISCR,equation = "PIB_br")
summary(var_PIISCR,equation = "invest")
plot(var_PIISCR, names = "PIB_br")
plot(var_PIISCR, names = "invest")
plot(var_PIISCR, names = "return")

serial.test(var_PIISCR,lags.pt = 3)
#  null hypothesis is well specified,
#  the alternative hypothesis is more loosely specified
# reject null hypothesis

normality.test(var_PIISCR)
# Null Hypothesis: Residuals are multivariate normal
# reject null hypothesis

arch.test(var_PIISCR,lags.multi = 3) 
# H_0 := B_1 = B_2 = … = B_q = 0
# not reject null hypothesis

plot(stability(var_PIISCR), names = "PIB_br")
plot(stability(var_PIISCR), names = "invest")
# coeficiente  estável

var.irf_T.3 <- vars::irf(var_PIISCR, 
                         impulse = "return", n.ahead = 12, boot = TRUE)
plot(var.irf_T.3)

FEVD_PIISSR <- vars::fevd(var_PIISCR, n.ahead = 12, impulse = "return")
FEVD_PIISSR

# SVAR para o modelo trimestral
amat = diag(6)
amat[2, 1] <- NA
amat[3, 1:2] <- NA
amat[4, 1:3] <- NA
amat[5, 1:4] <- NA
amat[6, 1:5] <- NA

bmat = diag(6)

svar_PIISSR = vars::SVAR(var_PIISCR,estmethod = "direct",
                         Amat = amat,Bmat = bmat,
                         hessian = TRUE, method = "BFGS")
svar_PIISSR.irf <- irf(svar_PIISSR, 
                       impulse = "return", n.ahead = 20, 
                       boot = TRUE,cumulative = T,runs = 100)
plot(svar_PIISSR.irf)

S_FEVD_PIISSR <- vars::fevd(svar_PIISSR, n.ahead = 20, impulse = "return_T")
S_FEVD_PIISSR[1:2]

