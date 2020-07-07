#### criando funcoes para VAR(p) ####
##### Bivariada return e credito ####
modelo_credito = na.omit(xts::merge.xts(dsaldo_credPJPF_M,return_M))
chart.Correlation(modelo_credito)
acf(modelo_credito,lag.max = 10)

base_credito = ts(modelo_credito, start = c(2002,02), freq = 12)
VARselect(base_credito, lag.max = 12, type = "none")

grangertest(dsaldo_credPJPF_M ~ return_M, order = 12, data = base_credito) 
# Return causa credito

#### Bivariada return e ibc-br ####
modelo_ibc_br = na.omit(xts::merge.xts(dibc_br_M,return_M))
chart.Correlation(modelo_ibc_br)
acf(modelo_ibc_br,lag.max = 10)

base_ibc_br = ts(modelo_ibc_br, start = c(2003,02), freq = 12)
VARselect(base_ibc_br, lag.max = 12, type = "none")

grangertest(dibc_br_M ~ return_M, order = 1, data = base_ibc_br) # Return causa IBC-Br

#### Bivariada return e investimento ####
modelo_invest = na.omit(xts::merge.xts(dinvest,return_T))
chart.Correlation(modelo_invest)
acf(modelo_invest,lag.max = 10)

base_invest = ts(modelo_invest, start = c(2002,02), freq = 4)
VARselect(base_invest, lag.max = 12, type = "none")

grangertest(invest ~ return_T, order = 1, data = base_invest) # Return causa Invest

# Multivariada Mensal
#### Variações IBC-Br, Saldo Crédito PJ&PF, r, Return_M ####
base_dados_M = na.omit(xts::merge.xts(dibc_br_M,dsaldo_credPJPF_M,r_M,return_M))
chart.Correlation(base_dados_M)
acf(base_dados_M,lag.max = 12,plot = F)

base_M = ts(base_dados_M, start = c(2003,02), freq = 12)
VARselect(base_M, lag.max = 24, type = "none")

grangertest(dibc_br_M ~ return_M, order = 2, data = base_M) 
# Return causa IBC-Br
grangertest(dibc_br_M ~ r_M, order = 2, data = base_M)
# juros real não causa IBC-Br
grangertest(dibc_br_M ~ dsaldo_credPJPF_M, order = 2, data = base_M) 
# Credito PJ&PF nao causa IBC-Br

grangertest(dsaldo_credPJPF_M ~ dibc_br_M, order = 2, data = base_M)
# IBC-Br causa Credito PJ&PF
grangertest(dsaldo_credPJPF_M ~ r_M, order = 2, data = base_M)
# juros real não causa Credito PJ&PF
grangertest(dsaldo_credPJPF_M ~ return_M, order = 2, data = base_M)
# retorno do ibovespa causa Credito PJ&PF

grangertest(r_M ~ dibc_br_M, order = 2, data = base_M)
# IBC-Br não causa taxa de juros real
grangertest(r_M ~ dsaldo_credPJPF_M, order = 2, data = base_M)
# crédito PJ&PF não causa taxa de juros real
grangertest(r_M ~ return_M, order = 2, data = base_M)
# retorno do ibovespa causa taxa de juros real

grangertest(return_M ~ dibc_br_M, order = 2, data = base_M) 
# IBC-Br não causa retorno do ibovespa
grangertest(return_M ~ dsaldo_credPJPF_M, order = 2, data = base_M) 
# crédito não causa retorno
grangertest(return_M ~ r_M, order = 2, data = base_M)
# juros real não causa retorno

# Multivariada Trimestral com PIB-Br
#### Variações PIB-Br, Saldo Crédito PJ&PF, r, Return_T #####
base_dados_T = na.omit(xts::merge.xts(dPIB_br,dsaldo_credPJPF_T,r_T,return_T))
chart.Correlation(base_dados_T)
acf(base_dados_T,lag.max = 4,plot = T)

base_T = ts(base_dados_T, start = c(2002,02), freq = 4)
VARselect(base_T, lag.max = 8, type = "none")

grangertest(PIB_br ~ return_T, order = 2, data = base_T) 
# Return causa IBC-Br
grangertest(PIB_br ~ r_T, order = 2, data = base_T) 
# juros real causa IBC-Br
grangertest(PIB_br ~ dsaldo_credPJPF_T, order = 2, data = base_T) 
# Credito PJ&PF  causa IBC-Br

grangertest(return_T ~ PIB_br, order = 2, data = base_T) 
# PIB_br não causa Return
grangertest(r_T ~ PIB_br, order = 2, data = base_T) 
# PIB_br não causa juros real
grangertest(dsaldo_credPJPF_T ~ PIB_br, order = 2, data = base_T) 
# PIB_br não causa CRÉDITO PJ&PF

# Multivariada Trimestral com Investimento
#### Variações Investimento, Saldo Crédito PJ&PF, r, Return_T ####
base_dados_invet_T = na.omit(xts::merge.xts(dinvest,dsaldo_credPJPF_T,r_T,return_T))
chart.Correlation(base_dados_invet_T)
acf(base_dados_invet_T,lag.max = 4,plot = T)

base_invest_T = ts(base_dados_invet_T, start = c(2002,01), freq = 4)
VARselect(base_invest_T, lag.max = 8, type = "none")

grangertest(invest ~ return_T, order = 2, data = base_invest_T) 
# Return causa Investimento
grangertest(invest ~ r_T, order = 2, data = base_invest_T) 
# juros real não causa Investimento
grangertest(invest ~ dsaldo_credPJPF_T, order = 2, data = base_invest_T) 
# Credito PJ&PF causa Investimento

grangertest(return_T ~ invest, order = 2, data = base_invest_T) 
# Investimento não causa Return
grangertest(r_T ~ invest, order = 2, data = base_invest_T) 
# Investimento não causa r
grangertest(dsaldo_credPJPF_T ~ invest, order = 2, data = base_invest_T) 
# Investimento não causa CRÉDITO PJ&PF

# Multivariada Trimestral
#### PIB, Investimento, IPCA, Saldo Créd PJ&PF, r, Return_T ####
base_dados_var_ag = na.omit(xts::merge.xts(dPIB_br,dinvest,ipca_T,
                                           dsaldo_cred,swap_360_T,return_normal))
chart.Correlation(base_dados_var_ag)
acf(base_dados_var_ag,lag.max = 4,plot = T)

base_PIISCR = ts(base_dados_var_ag, start = c(2002,02), freq = 4)
VARselect(base_PIISCR, lag.max = 4, type = "none")

#### exportando os dados ####
require(writexl)
write.zoo(modelo_credito,file=paste0(mydir,"/Mestrado/artigo/artigos/","modelo_credito.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
write.zoo(modelo_ibc_br,file=paste0(mydir,"/Mestrado/artigo/artigos/","modelo_ibc_br.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
write.zoo(modelo_invest,file=paste0(mydir,"/Mestrado/artigo/artigos/","modelo_invest.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
write.zoo(base_dados_M,file=paste0(mydir,"/Mestrado/artigo/artigos/","base_dados_M.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
write.zoo(base_dados_T,file=paste0(mydir,"/Mestrado/artigo/artigos/","base_dados_T.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
write.zoo(base_dados_invet_T,file=paste0(mydir,"/Mestrado/artigo/artigos/","base_dados_invet_T.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
write.zoo(base_dados_var_ag,file=paste0(mydir,"/Mestrado/artigo/artigos/","base_dados_var_ag.csv"),
          index.name="date",row.names=FALSE,col.names=TRUE,sep=",")
