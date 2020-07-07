require(urca)

#### PIB, Crédito, r, IBOV ####

PIB_br
saldo_credPJPF_def_T
r_T
bvsp_def

dados_PCRB = na.omit(xts::merge.xts(log(PIB_br),
                                    saldo_credPJPF_def_T,
                                    r_T,
                                    bvsp_def))
colnames(dados_PCRB) = c("PIB_br","saldo_credPJPF","r_T","bvsp_def")
#### exportando os dados ####
require(writexl)
write.zoo(dados_PCRB,file=paste0(mydir,"/Mestrado/artigo/","dados_PCRB.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
##### modelo VAR ####
autoplot(dados_PCRB)
VARselect(dados_PCRB,"both",lag.max = 4)

grangertest(PIB_br ~ saldo_credPJPF, order = 3, data = dados_PCRB) 
# crédito causa PIB
grangertest(PIB_br ~ r_T, order = 3, data = dados_PCRB) 
# juros real causa PIB
grangertest(PIB_br ~ bvsp_def, order = 3, data = dados_PCRB) 
# IBOV causa PIB

grangertest(saldo_credPJPF ~ PIB_br, order = 3, data = dados_PCRB) 
grangertest(saldo_credPJPF ~ r_T, order = 3, data = dados_PCRB) 
grangertest(saldo_credPJPF ~ bvsp_def, order = 3, data = dados_PCRB) 
# PIB, r e IBOV causam crédito

grangertest(r_T ~ PIB_br, order = 3, data = dados_PCRB) 
grangertest(r_T ~ saldo_credPJPF, order = 3, data = dados_PCRB) 
grangertest(r_T ~ bvsp_def, order = 3, data = dados_PCRB) 
# PIB não causa r / crédito e IBOV causam r 

grangertest(bvsp_def ~ PIB_br, order = 3, data = dados_PCRB) 
grangertest(bvsp_def ~ saldo_credPJPF, order = 3, data = dados_PCRB) 
grangertest(bvsp_def ~ r_T, order = 3, data = dados_PCRB) 
# PIB, crédito e r não causa IBOV

var_aic <- vars::VAR(dados_PCRB, type = "both", p = 3)

var_aic$p

## testing serial correlation
args(serial.test)
## Portmanteau−Test
var2c.serial = serial.test(var_aic, lags.pt = 16,
                            type = "PT.asymptotic")
var2c.serial
plot(var2c.serial , names = "PIB_br")
plot(var2c.serial , names = "saldo_credPJPF")
plot(var2c.serial , names = "r_T")
plot(var2c.serial , names = "bvsp_def")

## testing heteroscedasticity 9
args(arch.test)
var2c.arch = arch.test(var_aic , lags.multi = 5,
                        multivariate.only = TRUE)
var2c.arch
## testing for normality
args(normality.test) 
var2c.norm = normality.test(var_aic ,
                             multivariate.only = TRUE)
var2c.norm 

plot(stability(var_aic), names = "PIB_br")
plot(stability(var_aic), names = "saldo_credPJPF")
plot(stability(var_aic), names = "r_T")
plot(stability(var_aic), names = "bvsp_def")
# Coeficientes estáveis

vec.n <- ca.jo(dados_PCRB, ecdet = "none", type = "trace",
               K = 3, spec = "transitory")
summary(vec.n)
lttest(vec.n, r=1)
lttest(vec.n, r=2)
lttest(vec.n, r=3)

vec.c <- ca.jo(dados_PCRB, ecdet = "const", type = "trace",
               K = 3, spec = "transitory")
summary(vec.c)
lttest(vec.c, r=1)
lttest(vec.c, r=2)
lttest(vec.c, r=3)

vec.t <- ca.jo(dados_PCRB, ecdet = "trend", type = "trace",
               K = 3, spec = "transitory")
summary(vec.t)
lttest(vec.t, r=1)
lttest(vec.t, r=2)
lttest(vec.t, r=3)

##### modelo VECM #### 
vecm.r1 <- cajorls(vec.c, r=1)
summary(vecm.r1$rlm,equation = "PIB_br")
alpha = coef(vecm.r1$rlm)[1, ]
beta = vecm.r1$beta
resids = resid(vecm.r1$rlm)
N = nrow(resids)
sigma = crossprod ( resids )/N
## t−stats for alpha
alpha.se = sqrt(solve(crossprod(
  cbind(vec.c@ZK %*% beta,vec.c@Z1)))
  [1,1]*diag(sigma))
alpha.t = alpha/alpha.se
## t−stats for beta
beta.se = sqrt(diag(kronecker(solve(
  crossprod(vec.c@RK[ ,-1])),
  solve(t(alpha)%*%solve(sigma)
        %*% alpha))))
beta.t = c(NA,beta[-1]/beta.se)

var_level = vec2var(vec.c,r = 1)
irf.vec2vac = vars::irf(var_level,impulse = "bvsp_def",
                        cumulative = T,boot = T,runs = 1000,n.ahead = 20)
irf.vec2vac

fevd.vec2vac = vars::fevd(var_level, n.ahead = 20)
fevd.vec2vac$PIB_br

# número de restrições independente  lineares 
length(dados_PCRB[1,])
K = length(dados_PCRB[1,])
1/2*K*(K-1)
# 6 restrições independente lineares

#vindo da decomposição de Beveridge-Nelson =
#choques permanentes
r = 1
k = r*(K-r)
k
# 3 choques permanentes
# 1 choque temporário dado r = 1

# Porque esta matriz é de classificação reduzida
k*r
# 3 restrições independentes lineares são impostas

# Portanto, é necessário definir elementos adicionais para zero
1/2*k*(k-1)
# 3 elementos adicionais para zero

LR <- matrix(NA, nrow = 4, ncol = 4)
dimnames(LR) <- list(dimnames(dados_PCRB)[[2]]
                     , dimnames(dados_PCRB)[[2]]) # Rename matrix dimensions
LR[1:4,3] = 0
LR[3,2:4] = 0
LR

SR <- matrix(NA, nrow = 4, ncol = 4)
dimnames(SR) <- list(dimnames(dados_PCRB)[[2]]
                     , dimnames(dados_PCRB)[[2]]) # Rename matrix dimensions
SR[3,2] = 0
SR

#### modelo com constante ####
svec = SVEC(vec.c, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE, runs = 100)
summary(svec)

# verificação dos choques de LR
# sera que IBOV tem efeito de LP no PIB?
LR[1, 4] <- 0
svec.ip <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.ip$LRover
# null hypothesis:
# shocks to the IBOVESPA do not exert a long-run effect on PIB_br
# hasn't be rejected for a significance level of 5%

# será que crédito tem efeito de LP no PIB?
LR <- matrix(NA, nrow = 4, ncol = 4)
dimnames(LR) <- list(dimnames(dados_PCRB)[[2]]
                     , dimnames(dados_PCRB)[[2]]) # Rename matrix dimensions
LR[1:4,3] = 0
LR[3,2:4] = 0

LR[1, 2] <- 0
svec.cp <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.cp$LRover
# null hypothesis:
# shocks to the crédito do not exert a long-run effect on PIB_br
# has to be rejected for a significance level of 5%

svec.irf <- vars::irf(svec.ip, response = "PIB_br", n.ahead = 20,
                      boot = TRUE,cumulative = T)
plot(svec.irf)

fevd.PIB_br <- vars::fevd(svec,n.ahead= 20)$PIB_br
fevd.PIB_br

#####  modelo com tendência ####
# número de restrições independente  lineares 
length(dados_PCRB[1,])
K = length(dados_PCRB[1,])
1/2*K*(K-1)
# 6 restrições independente lineares

#vindo da decomposição de Beveridge-Nelson =
#choques permanentes
r = 1
k = r*(K-r)
k
# 3 choques permanentes
# 1 choque temporário dado r = 1

# Porque esta matriz é de classificação reduzida
k*r
# 3 restrições independentes lineares são impostas

# Portanto, é necessário definir elementos adicionais para zero
1/2*k*(k-1)
# 3 elementos adicionais para zero

LR <- matrix(NA, nrow = 4, ncol = 4)
dimnames(LR) <- list(dimnames(dados_PCRB)[[2]]
                     , dimnames(dados_PCRB)[[2]]) # Rename matrix dimensions
LR[1:4,3] = 0
LR[3,2:4] = 0
LR

SR <- matrix(NA, nrow = 4, ncol = 4)
dimnames(SR) <- list(dimnames(dados_PCRB)[[2]]
                     , dimnames(dados_PCRB)[[2]]) # Rename matrix dimensions
SR[3,2] = 0
SR

svec = SVEC(vec.t, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE, runs = 100)
summary(svec)

# verificação dos choques de LR
# sera que IBOV tem efeito de LP no PIB?
LR[1, 4] <- 0
svec.ip <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.ip$LRover
# null hypothesis:
# shocks to the IBOVESPA do not exert a long-run effect on PIB_br
# has to be rejected for a significance level of 5%

# será que crédito tem efeito de LP no PIB?
LR <- matrix(NA, nrow = 4, ncol = 4)
dimnames(LR) <- list(dimnames(dados_PCRB)[[2]]
                     , dimnames(dados_PCRB)[[2]]) # Rename matrix dimensions
LR[1:4,3] = 0
LR[3,2:4] = 0

LR[1, 2] <- 0
svec.cp <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.cp$LRover
# null hypothesis:
# shocks to the crédito do not exert a long-run effect on PIB_br
# has to be rejected for a significance level of 5%

svec.irf <- vars::irf(svec, response = "PIB_br", n.ahead = 20,
                      boot = TRUE,cumulative = T)
plot(svec.irf)

fevd.PIB_br <- vars::fevd(svec,n.ahead= 20)$PIB_br
fevd.PIB_br

##### PIB, Investimento, IPCA, Saldo Crédito PJ&PF, Swap-DI360 e IBOV####
dados_PIISCR = na.omit(xts::merge.xts(log(PIB_br),log(invest),
                                      ipca_T,log(saldo_credPJPF_T),swap_360_T,
                                      log(bvsp_T)))

autoplot(dados_PIISCR)
# exportando os dados
write.zoo(dados_PIISCR,file=paste0(mydir,"/Mestrado/artigo/","dados_PIISCR.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")