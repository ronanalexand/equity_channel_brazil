##############################################
#########         PACOTES              #######
##############################################

# Pacotes para coletar os dados
suppressMessages(require(tidyquant))
suppressMessages(require(Quandl))
suppressMessages(require(BETS))
suppressMessages(require(ecoseries))
suppressMessages(require(rbcb))

# Pacotes para manusear os dados
suppressMessages(require(tidyverse))
suppressMessages(require(tbl2xts))
suppressMessages(require(PerformanceAnalytics))

# Pacote para facilitar o uso de arquivos do projeto
suppressMessages(require(rprojroot))


# Pacote para gerar os gráficos e tabelas
suppressMessages(require(dygraphs))
suppressMessages(library(ggplot2))
suppressMessages(library(ggfortify))
suppressMessages(library(scales))
suppressMessages(library(ggpubr))
suppressMessages(library(ggthemes))
theme_set(theme_gdocs())
suppressMessages(library(expss))
suppressMessages(library(gridExtra))

# Pacote para deflacionar séries
suppressMessages(require(deflateBR))

# Pacote para Bayesian Structural Vector Autoregression e VAR
suppressMessages(require(bvartools))
suppressMessages(require(vars))

##############################################
#######       PATH DEFAULT              ######
##############################################

# Garantir que o path para o projeto é definido automaticamente. 
# Isso nos garante que uma vez clonado o projeto ele pode ser 
# executado sem necessidade de configurações alternativas de path.

root = rprojroot::is_rstudio_project 
mydir = root$find_file()   # Encontra o diretório no qual o projeto atual está salvo

##############################################
######         QUANDL API KEY           ######
##############################################

quandl_api_key("g5wHJHsCYggaz3fbfAtX")


####
## OPÇÕES PARA A COLETA
####

from = "2001-12-01"
type = "xts"

####
## DADOS ####
####

# Índice Bovespa
bvsp = quantmod::getSymbols("^BVSP", src = "yahoo", 
                            periodicity='monthly',
                            auto.assign = FALSE, 
                            from = from, return.class = type) %>% 
  na.omit() %>%
  .$BVSP.Close


if(format(today(), "%d")<15){
  bvsp = head(bvsp,n = -2)
} else {
  bvsp = head(bvsp,n = -1)
}

inicio = first(index(bvsp))
ultimo = last(index(bvsp))
actual_dates <- seq.Date(from = as.Date(as.yearmon(inicio)), 
                         to = as.Date(as.yearmon(ultimo)), by = "month")

a = format(today(), "%m/%Y")
a = log(deflate(nominal_values = as.xts(bvsp),
                nominal_dates = actual_dates,
                real_date = format(as.yearmon(ultimo)-(1/12), "%m/%Y"), 
                index = "ipca"))

bvsp_def = a

#a = log(bvsp)
return = (a$BVSP.Close-xts::lag.xts(a$BVSP.Close))*100

return_M = to.monthly(apply.monthly(return,FUN = sum))[,4]
colnames(return_M) = "return_M"

return_T = to.quarterly(apply.quarterly(return,FUN = sum))[,4] 
colnames(return_T) = "return_T"

return_normal = (log(bvsp$BVSP.Close)-xts::lag.xts(log(bvsp$BVSP.Close)))*100
colnames(return_normal) = "return"
return_normal = to.quarterly(apply.quarterly(return_normal,FUN = sum))[,4] 
colnames(return_normal) = "return"

# IBOV trimestral
bvsp_T = to.quarterly(apply.quarterly(bvsp,FUN = mean))[,4]
colnames(bvsp_T) = "bvsp_T"

# Índice de Atividade Econômica do Banco Central (IBC-Br) - com ajuste sazonal
ibc_br = BETSget(24364, data.frame = T) %>%
  dplyr::as.tbl() %>%
  dplyr::select(date, ibc_br = value) %>%
  tbl2xts::tbl_xts(., cols_to_xts = "ibc_br") %>%
  .[paste0(from,"/")]

ibc_br_M = log(ibc_br)
dibc_br_M = (ibc_br_M$ibc_br-xts::lag.xts(ibc_br_M$ibc_br))*100
dibc_br_M = to.monthly(dibc_br_M)[,4]
colnames(dibc_br_M) = "dibc_br_M"


# PIB trimestral - Dados dessazonalizados - Produto Interno Bruto a preços de mercado	
PIB_br = BETSget(22109, data.frame = T) %>%
  dplyr::as.tbl() %>%
  dplyr::select(date, PIB_br = value) %>%
  tbl2xts::tbl_xts(., cols_to_xts = "PIB_br") %>%
  .[paste0(from,"/")]

PIB_br_log = log(PIB_br)
dPIB_br = (PIB_br_log$PIB_br-xts::lag.xts(PIB_br_log$PIB_br))*100

# FBCF do PIB trimestral - Dados dessazonalizados - Índice início 1995
invest = BETSget(22113, data.frame = T) %>%
  dplyr::as.tbl() %>%
  dplyr::select(date, invest = value) %>%
  tbl2xts::tbl_xts(., cols_to_xts = "invest")

invest_log = to.quarterly(log(invest))[,4]
dinvest = (invest_log$`log(invest).Close`-
             xts::lag.xts(invest_log$`log(invest).Close`))*100
dinvest = dinvest[paste0(from,"/")] # selecionar para o período de interesse
colnames(dinvest) = "invest"

# CDI Acumulado
cdi_acum = series_ipeadata(32237,periodicity = "M")  %>%
  as.data.frame() %>%
  dplyr::as.tbl() %>%
  dplyr::select(date = serie_32237.data, cdi_acum = serie_32237.valor) %>%
  tbl2xts::tbl_xts(., cols_to_xts = "cdi_acum")

cdi_acum = cumprod(cdi_acum/100+1)

cdi_acum = cdi_acum[paste0(from,"/")] # selecionar para o período de interesse
cdi_acum_M = to.monthly(cdi_acum)[,4]
colnames(cdi_acum_M) = "cdi_acum_M"

# Taxa referencial de swaps - DI pré-fixada - 360 dias - média do período
swap_360 = series_ipeadata(1900214364,periodicity = "M")  %>%
  as.data.frame() %>%
  dplyr::as.tbl() %>%
  dplyr::select(date = serie_1900214364.data, swap_360 = serie_1900214364.valor) %>%
  tbl2xts::tbl_xts(., cols_to_xts = "swap_360")

#dswap_360 = (swap_360$swap_360-
#               xts::lag.xts(swap_360$swap_360))

swap_360_M = to.monthly(swap_360)[,4]
colnames(swap_360_M) = "swap_360_M"
swap_360_T = to.quarterly(apply.quarterly(swap_360_M,FUN = mean))[,4]
colnames(swap_360_T) = "swap_360_T"

# IPCA - geral - índice (dez. 1993 = 100)
ipca_nivel = series_ipeadata(36482, periodicity = "M") %>%
  as.data.frame() %>%
  dplyr::as.tbl() %>%
  dplyr::select(date = serie_36482.data, ipca_nivel = serie_36482.valor) %>%
  tbl2xts::tbl_xts(., cols_to_xts = "ipca_nivel")

ipca_nivel = to.monthly(ipca_nivel)[,4]
colnames(ipca_nivel) = "ipca_nivel"
ipca_nivel_M = ipca_nivel[paste0(from,"/")] # selecionar para o período de interesse
colnames(ipca_nivel_M) = "ipca_nivel_M"

#Expectativa média de Inflação - IPCA - taxa acumulada para os próximos doze meses
ipca = series_ipeadata(1693254712, periodicity = "M") %>%
  as.data.frame() %>%
  dplyr::as.tbl() %>%
  dplyr::select(date = serie_1693254712.data, ipca = serie_1693254712.valor) %>%
  tbl2xts::tbl_xts(., cols_to_xts = "ipca")

ipca = to.monthly(ipca)[,4]
colnames(ipca) = "ipca"
ipca_M = ipca[paste0(from,"/")] # selecionar para o período de interesse
colnames(ipca_M) = "ipca_M"

ipca_T = to.quarterly(apply.quarterly(ipca_M,FUN = mean))[,4]
colnames(ipca_T) = "ipca_T"

# taxa real
r = ((1+swap_360/100)/(1+ipca/100)-1)*100

r = to.monthly((r))[,4]
colnames(r) = "r"
r_M = r[paste0(from,"/")] # selecionar para o período de interesse
colnames(r_M) = "r_M"

r_T = to.quarterly(apply.quarterly(r_M,FUN = mean))[,4]
colnames(r_T) = "r_T"


# Saldo - Pessoas jurídicas e Físicas - Total
saldo_credPJPF = BETSget(20539, data.frame = T) %>%
  dplyr::as.tbl() %>%
  dplyr::select(date, saldo_credPJPF = value) %>%
  tbl2xts::tbl_xts(., cols_to_xts = "saldo_credPJPF")

saldo_credPJPF = saldo_credPJPF[paste0(from,"/")] # selecionar para o período de interesse

saldo_credPJPF = ts(saldo_credPJPF,
                    start = c(2001,12),
                    frequency = 12)

require(seastests)
require(seasonalview)
require(seasonal)

isSeasonal(saldo_credPJPF)

acf(saldo_credPJPF,lag.max = 60)
pacf(saldo_credPJPF,lag.max = 60)
#AR(1) - Decrescimento exponencial e Decrescimento exponencial

require(tseries)
adf.test(saldo_credPJPF)

m = seas(saldo_credPJPF)
fivebestmdl(m)

# crédito dessazonalizado mensal
saldo_credPJPF = final(m)
# variação do crédito dessazonalizado mensal
dsaldo_cred = diff(log(final(m)))*100

isSeasonal(saldo_credPJPF)
isSeasonal(dsaldo_cred)

# crédito dessazonalizado trimestral
saldo_credPJPF_T = to.quarterly(apply.quarterly(as.xts(saldo_credPJPF),FUN = mean))[,4]
colnames(saldo_credPJPF_T) = "saldo_credPJPF_T"
# crédito deflacionado em nível
inicio = first(index(saldo_credPJPF))
ultimo = last(index(saldo_credPJPF))
actual_dates <- seq.Date(from = as.Date(as.yearmon(inicio)), 
                         to = as.Date(as.yearmon(ultimo)), by = "month")

a = format(today(), "%m/%Y")
saldo_credPJPF_def = log(deflate(as.xts(saldo_credPJPF), 
                actual_dates,
                format(last(index(as.xts(saldo_credPJPF))),"%m/%Y"), "ipca"))


# crédito trimestral deflacionado em nível
saldo_credPJPF_def_T = to.quarterly(apply.quarterly(saldo_credPJPF_def,FUN = mean))[,4]

# variação do crédito deflacionado
dsaldo_credPJPF_M = to.monthly((saldo_credPJPF_def))[,4]
colnames(dsaldo_credPJPF_M) = "dsaldo_credPJPF_M"
dsaldo_credPJPF_M = (dsaldo_credPJPF_M$dsaldo_credPJPF_M-
                       xts::lag.xts(dsaldo_credPJPF_M$dsaldo_credPJPF_M))*100
dsaldo_credPJPF_M = dsaldo_credPJPF_M[paste0(from,"/")] # selecionar para o período de interesse

dsaldo_credPJPF_T = to.quarterly(apply.quarterly(dsaldo_credPJPF_M,FUN = sum))[,4]
colnames(dsaldo_credPJPF_T) = "dsaldo_credPJPF_T"
