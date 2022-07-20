
##################################################****
#                Article 2                   ####
##################################################*
#************************************************
#              import data                     ####
#*************************************************

library(quantmod)
library(xts)
library(zoo)
w_indice <- c("^GSPC","000001.SS","^N225","^GDAXI","^FTSE",
              "^FCHI","FTSEMIB.MI","^GSPTSE","IMOEX.ME",
              "BTC-GSPC.CloseD","ETH-GSPC.CloseD",
              "GSPC.CloseDT-GSPC.CloseD","BTC-GSPC.CloseD","ETH-GSPC.CloseD","LTC-GSPC.CloseD")

start_date <- Sys.Date()-380
end_date <- Sys.Date()
envt1 <- new.env()
getSymbols(w_indice,env=envt1,from=start_date, to=end_date)
data <- do.call(merge, eapply(envt1, Cl))
dataframe<- as.data.frame(na.omit (data)) 

#----------------descriptive statistics------------------####
#*

datana <- na.omit(data)  # as zoo
R_data <- diff(log(datana)) # as zoo
R_data <- R_data[-1,]
R_dataframe <- as.data.frame(R_data)
R_dataframe <- na.omit(R_dataframe)
colnames(R_dataframe) <- c(  "GSPC.Close","China","Japan", "Germany",
                           "UK","France","Italy","Canada",
                           "RGSPC.Closesia","Tether","Bitcoin","Ethereum","Litecoin")

library(fBasics)
desc <- do.call(data.frame, 
                list(mean = apply(R_dataframe, 2, mean),
                     sd = apply(R_dataframe, 2, sd),
                     median = apply(R_dataframe, 2, median),
                     min = apply(R_dataframe, 2, min),
                     max = apply(R_dataframe, 2, max),
                     skew = apply(R_dataframe, 2, sampleSKEW),
                     kurt = apply(R_dataframe,2,sampleKURT)))
desc


desc <- cbind(rownames(desc),desc)
library(writexl)
write_xlsx(desc,"descstatistics.xlsx") 

return <- cbind(rownames(R_dataframe),R_dataframe)
colnames(return)[1] <- "Date"
write_xlsx(return,"return.xlsx")


getSymbols("^VIX",source="yahoo" ,from=start_date, to=end_date)
vix <- na.omit(VIX$VIX.AdjGSPC.Closeted)
vix <- diff(log(vix$VIX.AdjGSPC.Closeted))
plot(vix)


#********normality test************##
normalTest(R_dataframe$Russia,method="jb")






library(fBasics)
library(xts)
R_data_month <- R_data[endpoints(R_data,'month')]

#********modelling Garch***************************
library(xts)

alpha <- 0.1
beta <- 0.8 
omega <- var(R_data$GSPTSE.Close)*(1-alpha-beta)
e <- R_data$GSPTSE.Close-mean(R_data$GSPTSE.Close)  
e2 <- e^2  
nobs <- length(R_data$GSPTSE.Close)
predvar <- rep(NA,nobs)
predvar[1] <- var(R_data$GSPTSE.Close)
for (t in 2:nobs) {
  predvar[t] <- omega + alpha * e2[t-1] +beta * predvar[t-1]
}
predvol <- sqrt(predvar)
predvol <- xts(predvol, order.by = index(R_data))
#plot(predvol, type='l', main=" Litecoin Volatility")
colnames(predvol)<- c("value")
library(scales)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tibble)
predvol %>%
  as.data.frame() %>%
  rownames_to_column("Date") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  ggplot(aes(Date, value)) +
  geom_line() +
  scale_x_date(
    date_breaks = "1 month", 
    labels = date_format("%b\n%Y")) +
  theme_minimal() + labs(title = "Canada index Volatility",
                         subtitle = "Garch(1,1)")








# 
# 
# colnames(R_data_month) <- c("Litecoin","Thether","Etheruem","RGSPC.Closesia","GSPC.Close",
#                             "China","germany","France","UK","Japan","Canada",
#                             "Italy","Bitcoin")
# 
# 
# 
# lab <- list("2021/07","2021/08","2021/09","2021/10",
#             "2021/11","2021/12","2022/1","2022/02",
#             "2022/03","2022/04","2022/05","2022/06","2022/07")
# stars(R_data_month, draw.segments = TRUE, labels = lab, ncol = 5,
#       cex=0.5,key.loc = c(7, 0.5), mar = c(4, 0, 0, 0))
# #-----------------export to excel-----------------------####
# library(writexl)  
# nw <- data.frame(date=index(R_data), coredata(R_data)) # add index and convert to dataframe
# colnames(nw) <- c("date","Brasil","Indonesia","Naturalgaz","Japan","India","Turkey",
#                   "China","Bitcoin","France","Silver",
#                   "Italy","UK","Crudeoil","Canada","Germany",
#                   "Argentina","Aluminium","Gold","Southkorea",
#                   "Suger","Mexico","Litecoin","RGSPC.Closesia","Sp500")
# 
# write_xlsx(nw,"NT.xlsx") 
# 
# 
# 





#********************************************************
# ****   Granger caGSPC.Closeality Network         ****   ####
#*********************************************************
library(igraph)
library(readxl)
edge <- read_excel("edge ukrain.xlsx")
noeuds <- read_excel("vertice.xlsx")
edge <- read_excel("edge sincenov21.xlsx") # after Nov-21
Neural <- graph_from_data_frame(d=edge, vertices=noeuds, directed=T)


#E(Neural)$weight <- E(Neural)$pro
#Neural[c(1:3),c(1:3)]
#------------------degree centrality--------------------####
Neural_deg <- degree(Neural,mode=c("all"))
V(Neural)$degree <- Neural_deg
V(Neural)$degree
which.max(Neural_deg)
#----------------eigenvector centrality------------------####
Neural_eig <- evcent(Neural)$vector  
V(Neural)$eigen <- Neural_eig
which.max(Neural_eig)

#------------Betweenness centrality-----------------####
Neural_bet <- betweenness(Neural,directed=F)
V(Neural)$betweeness <- Neural_bet
which.max(Neural_bet)

#---------------plot graph--------------------####
DF <- as_long_data_frame(Neural)
set.seed(1010)
colrss <- c("tomato2","antiquewhite2")
V(Neural)$color <- colrss[V(Neural)$type]
edge.start <- ends(Neural, es=E(Neural), names=F)[,1]
edge.col <- V(Neural)$color[edge.start]
plot(Neural,layout=layout_in_circle, vertex.shape="circle",
     vertex.size=Neural_deg*5,vertex.label.cex=0.9,edge.width=2,
     edge.arrow.size=0.5, edge.color=edge.col) 
legend(x=-2, y=-1.1, c("Cryptocurrencies","Equity markets"),
       pch=21, col="#777777", pt.bg=colrss, pt.cex=1.5, cex=1, bty="n",
       ncol=1)



#*******************************
# **** wavelet analysis **** #####
#*******************************
library(biwavelet)
library(zoo)
library(xts)

t1 <- cbind(1:197,R_dataframe$Tether)
t2 <- cbind(1:197,R_dataframe$China)
t1[is.na(t1)] <- 0
t2[is.na(t2)] <- 0
nrands=100
sum(is.na(t1))
sum(is.na(t2))
wtcr <- wtc(t1,t2,nrands=nrands)
par(mar=c(5,4,5,7),+0.1)
#par(oma=c(1,1,1,0),mar=c(0,4,0.5,5),+0.1)
plot(wtcr,plot.phase=TRUE,xaxt='n',lty.coi=1,col.coi="grey",lwd.coi=2, 
     lwd.sig=2, arrow.lwd=0.03, arrow.len=0.08, ylab="Frequency",xlab="Years-Month",
     plot.cb= T, main="WTC : Tether- SSE index", cex.main=0.8)

n <- c("2021-07","2021-08","2021-09","2021-10","2021-11",
       "2021-12","2022-01","2022-02","2022-03","2022-04","2022-05",
       "2022-06","2022-07")
axis(1, at = c(seq(0,195,16)), n )








