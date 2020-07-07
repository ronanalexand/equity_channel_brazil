# TVP-BVAR
require(bvarsv)
fit <- bvar.sv.tvp(base_M,p = 1,nrep = 100000,nburn = 50000,
                   k_B = 4, k_A = 4, k_sig = 1, k_Q = 0.01, k_S = 0.1, k_W = 0.01, 
                   pQ = NULL, pW = NULL, pS = NULL)

# Impulse responses
# error term VCV matrix at time point t is used
impulse.responses(fit,response.variable = 1,impulse.variable = 4,t = 49)

impulse.responses(fit,response.variable = 1,impulse.variable = 4,t = 95)

impulse.responses(fit,response.variable = 1,impulse.variable = 4,t = 159)

#diagonal elements are set to their averages over time, 
#whereas the off-diagonal elements are specific to time t
impulse.responses(fit,response.variable = 1,impulse.variable = 4,t = 49,scenario = 3)

impulse.responses(fit,response.variable = 1,impulse.variable = 4,t = 95,scenario = 3)

impulse.responses(fit,response.variable = 1,impulse.variable = 4,t = 159,scenario = 3)

# TVP-BVAR
require(bvarsv)
fit.1 <- bvar.sv.tvp(base_M,p = 1,nrep = 100000,nburn = 50000,
                     k_B = 4, k_A = 4, k_sig = 1, k_Q = 0.1, k_S = 0.1, k_W = 0.1, 
                     pQ = NULL, pW = NULL, pS = NULL)

# Impulse responses
# error term VCV matrix at time point t is used
impulse.responses(fit.1,response.variable = 1,impulse.variable = 4,t = 49)

impulse.responses(fit.1,response.variable = 1,impulse.variable = 4,t = 95)

impulse.responses(fit.1,response.variable = 1,impulse.variable = 4,t = 159)

#diagonal elements are set to their averages over time, 
#whereas the off-diagonal elements are specific to time t
impulse.responses(fit.1,response.variable = 1,impulse.variable = 4,t = 49,scenario = 3)

impulse.responses(fit.1,response.variable = 1,impulse.variable = 4,t = 95,scenario = 3)

impulse.responses(fit.1,response.variable = 1,impulse.variable = 4,t = 159,scenario = 3)
