
# ------------------- Pacotes utilizados ---------------------
library(SPREDA)
library(boot)
library(lattice)
source("https://raw.githubusercontent.com/CodeOwl94/ross-reliability/master/ReliabilitySupportFns.R")
library(readxl)

#Comando para ler o arquivo
exa1 <- read_excel("MC.xlsx", sheet = "BD")

#Exibir o arquivo
dim(exa1)

#Retorna A quantidade de itens por categoria - 
# Dados suspensos à direita, Dados suspensos À esquerda, Dados Compleots
stack(table(exa1$fail))

# F = Falha
# S = Suspensão
# Transforma os itens F e S em números, Sendo 1 Falha e 0 Suspensão
exa1$fail <- ifelse(exa1$fail=="S","T",as.character(exa1$fail))
exa1.dat <- data.frame(time=exa1$time,
                       event=1-as.numeric(as.logical(exa1$fail)))
stack(table(exa1.dat$event))   




#Distribuição empírica de frequência cumulativa das medições completas do tempo até a falha.
plot(ecdf(exa1.dat[exa1.dat$event==1,"time"]),main="",xlab="tempo",
     verticals=T,las=1,adj=0.5) 
abline(v=quantile(exa1.dat[exa1.dat$event==1,"time"],probs=c(0.25,0.5,0.75)),
       col='red',lwd=2,lty=2) # add on quartiles
title("F(t)", adj=1)


#Vamos observar a Frequencia pelo Hitograma
hist(exa1.dat[exa1.dat$event==1,"time"],main="",col="lightgrey",
     xlab="Duração - Dados completos",las=1,adj=0.5)
title("Histograma", adj=1)



# Gráficos de F(t) para a distribuição Weibull, Lognormal, Normal e Exponencial
#Probability.Plots(exa1.dat)


#No campo dist, eu posso modificar o parâmetro para o tipo de distribuição que eu quero
#Probability.Plots(exa1.dat,dist="Weibull")
#Probability.Plots(exa1.dat,dist="Normal")
#Probability.Plots(exa1.dat,dist="Lognormal")
#Probability.Plots(exa1.dat,dist="Exponential")


# Estimação dos Parâmetros por MLEa
exa1.spreda <- Lifedata.MLE(Surv(time,event)~1,
                            data=exa1.dat,
                            dist="weibull")
summary(exa1.spreda)

beta.spreda <- 1 / unname(exp(exa1.spreda$coef[2]))
eta.spreda <- unname(exp(exa1.spreda$coef[1]))

# Intervalo de Confiança de 95%
beta.95cl_hi <- 1 / (summary(exa1.spreda)$coefmat["sigma","95% Lower"])
beta.95cl_lo <- 1 / (summary(exa1.spreda)$coefmat["sigma","95% Upper"])
eta.95cl_lo <- exp(summary(exa1.spreda)$coefmat["(Intercept)","95% Lower"])
eta.95cl_hi <- exp(summary(exa1.spreda)$coefmat["(Intercept)","95% Upper"])
(beta.ests <- c(lower=beta.95cl_lo,est=beta.spreda,upper=beta.95cl_hi))

(eta.ests <- c(lower=eta.95cl_lo,est=eta.spreda,upper=eta.95cl_hi))

Probability.Plots(exa1.dat,dist="Weibull")


# Taxa de Risco
hazard.plot.w2p(beta=beta.spreda,eta=eta.spreda,time=exa1.dat$time,line.colour="blue")
title("Taxa de Risco", adj=1)

#Confaibilidade
Reliability.plot.w2p(beta=beta.spreda,eta=eta.spreda,time=exa1.dat$time,line.colour="blue")
# linha de referência
abline(v=30,col="lightgray",lty=2)
title("R(t)", adj=1)

# Porbabilidade de Falha para t=30
Calc.Unreliability.w2p(beta=beta.spreda,eta=eta.spreda,time=30)


#Calcular MTTF 
MTBF.exa1 <- Weibull.2p.Expectation(eta=eta.spreda,beta=beta.spreda)





