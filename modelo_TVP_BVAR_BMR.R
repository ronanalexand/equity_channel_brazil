library(BMR)

#
bvar_data <- data.matrix(as.data.frame(base_M))

#

tau <- 30

XiBeta <- 4
XiQ <- 0.005
gammaQ <- tau
XiSigma <- 1
gammaS = 4

which_irfs = c(90,136,200)

bvar_obj <- new(bvartvp)

#

bvar_obj$build(bvar_data,TRUE,2)
bvar_obj$prior(tau,XiBeta,XiQ,gammaQ,XiSigma,gammaS)
bvar_obj$gibbs(100000,50000)

BMR::IRF.Rcpp_bvartvp(bvar_obj,20,which_irfs,
                      which_shock = 4,var_names=colnames(bvar_data),save=FALSE)
plot(bvar_obj,var_names=colnames(bvar_data),save=FALSE)
