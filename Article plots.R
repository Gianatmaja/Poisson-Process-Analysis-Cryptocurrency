#Libraries
library(ggplot2)

#Setup
Bitcoin = read.csv('BTC-USD.csv')
Ethereum = read.csv('ETH-USD.csv')
Litecoin = read.csv('LTC-USD.csv')
NEM = read.csv('XEM-USD.csv')
Stellar = read.csv('XLM-USD.csv')
Monero = read.csv('XMR-USD.csv')
XRP = read.csv('XRP-USD.csv')

#Plot 1
ggplot(data = Bitcoin) + 
  geom_line(aes(x = as.Date(Date), y = Adj.Close, group = 1), col = "gold") + 
  theme_minimal() + ggtitle("Bitcoin Price Trend", subtitle = "1 Jan 2016 - 31 Dec 2020\n") + 
  xlab("\nDate") + ylab("Closing Price\n") + geom_vline(xintercept = as.Date("2020-03-01"), 
                                                        linetype = 2, 
                                                        color = "red", size = 1) + 
  geom_text(x = as.Date("2019-10-10"), y = 22000, 
            label = "Start of \nPandemic \nRestrictions", color = "red") +
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))

# Plot 2
ggplot(data = XRP) + 
  geom_line(aes(x = as.Date(Date), y = Adj.Close, group = 1), col = "violet") + 
  theme_minimal() + ggtitle("XRP Price Trend", subtitle = "1 Jan 2016 - 31 Dec 2020\n") + 
  xlab("\nDate") + ylab("Closing Price\n") + geom_vline(xintercept = as.Date("2020-03-01"), 
                                                        linetype = 2, 
                                                        color = "red", size = 1) + 
  geom_text(x = as.Date("2019-10-10"), y = 2.2, 
            label = "Start of \nPandemic \nRestrictions", color = "red") +
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))

# Setup
Bitcoin_change = 100*(Bitcoin$Adj.Close - Bitcoin$Adj.Close[1])/Bitcoin$Adj.Close[1]
Ether_change = 100*(Ethereum$Adj.Close - Ethereum$Adj.Close[1])/Ethereum$Adj.Close[1]
Litecoin_change = 100*(Litecoin$Adj.Close - Litecoin$Adj.Close[1])/Litecoin$Adj.Close[1]
Nem_change = 100*(NEM$Adj.Close - NEM$Adj.Close[1])/NEM$Adj.Close[1]
Stellar_change = 100*(Stellar$Adj.Close - Stellar$Adj.Close[1])/Stellar$Adj.Close[1]
Monero_change = 100*(Monero$Adj.Close - Monero$Adj.Close[1])/Monero$Adj.Close[1]
Xrp_change = 100*(XRP$Adj.Close - XRP$Adj.Close[1])/XRP$Adj.Close[1]


Change_base = data.frame(
  Date = as.Date(Bitcoin$Date),
  Bitcoin = Bitcoin_change,
  Ethereum = Ether_change,
  Litecoin = Litecoin_change,
  NEM = Nem_change,
  Stellar = Stellar_change,
  Monero = Monero_change,
  XRP = Xrp_change
)

head(Change_base, 5)

# Plot 3
ggplot(data = Change_base) + 
  geom_line(aes(x = Date, y = log(Bitcoin+20)), col = "gold") + 
  geom_line(aes(Date, log(Ethereum+20)), col = "springgreen") + 
  geom_line(aes(Date, log(Litecoin+20)), col = "azure3") + 
  geom_line(aes(Date, log(NEM+20)), col = "orange") + 
  geom_line(aes(Date, log(Stellar+20)), col = "steelblue2") +
  geom_line(aes(Date, log(Monero+20)), col = "tomato") + 
  geom_line(aes(Date, log(XRP+20)), col = "violet") + 
  theme_minimal() + xlim(as.Date("2016-01-01"), as.Date("2021-06-01")) +
  xlab("\nDate") + ylab("% Change\n") +
  ggtitle("% Change from 1 Jan 2016 (Log(Y + 20) Transformation)", subtitle = " ") +
  geom_text(x = as.Date("2021-03-10"), y = log(Change_base$Bitcoin[1827]+20), 
            label = "Bitcoin", color = "gold") +
  geom_text(x = as.Date("2021-04-10"), y = log(Change_base$Ethereum[1827]+20), 
            label = "Ethereum", color = "springgreen") +
  geom_text(x = as.Date("2021-03-10"), y = log(Change_base$Litecoin[1827]+20)-0.5, 
            label = "Litecoin", color = "azure3") +
  geom_text(x = as.Date("2021-03-10"), y = log(Change_base$NEM[1827]+20), 
            label = "NEM", color = "orange") +
  geom_text(x = as.Date("2021-03-10"), y = log(Change_base$Stellar[1827]+20)+0.5, 
            label = "Stellar", color = "steelblue2") +
  geom_text(x = as.Date("2021-03-10"), y = log(Change_base$Monero[1827]+20), 
            label = "Monero", color = "tomato") + 
  geom_text(x = as.Date("2021-03-10"), y = log(Change_base$XRP[1827]+20), 
            label = "XRP", color = "violet") +
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white", hjust = 0.5, size = 20, face = "bold"))

# Setup
Daily_Changes = function(Data){
  Closing_price = Data$Close
  Pct_change = 100*(Closing_price[2:1827] - Closing_price[1:1826])/Closing_price[1:1826]
  
  return(Pct_change)
}

BtcChange = Daily_Changes(Bitcoin)
EthChange = Daily_Changes(Ethereum)
LtcChange = Daily_Changes(Litecoin)
NemChange = Daily_Changes(NEM)
XlmChange = Daily_Changes(Stellar)
XrmChange = Daily_Changes(Monero)
XrpChange = Daily_Changes(XRP)

Daily_change_df = data.frame(
  Date = as.Date(Bitcoin$Date[2:1827]),
  Bitcoin = BtcChange,
  Ethereum = EthChange,
  Litecoin = LtcChange,
  NEM = NemChange,
  Stellar = XlmChange,
  Monero = XrmChange,
  XRP = XrpChange
)

head(Daily_change_df, 5)

# Plot 4
ggplot(data = Daily_change_df) + 
  geom_boxplot(aes(x = "NEM", y = NEM), col = "orange", fill = "orange", alpha = 0.5) +
  geom_boxplot(aes(x = "Bitcoin", y = Bitcoin), col = "gold" ,fill = "gold", alpha = 0.5) + 
  geom_boxplot(aes(x = "Ethereum", y = Ethereum), col = "springgreen", fill = "springgreen", alpha = 0.5) +
  geom_boxplot(aes(x = "Litecoin", y = Litecoin), col = "azure3", fill = "azure3", alpha = 0.5) +
  geom_boxplot(aes(x = "Stellar", y = Stellar), col = "steelblue2", fill = "steelblue2", alpha = 0.5) + 
  geom_boxplot(aes(x = "Monero", y = Monero), col = "tomato", fill = "tomato", alpha = 0.5) +
  geom_boxplot(aes(x = "XRP", y = XRP), col = "violet", fill = "violet", alpha = 0.5) + 
  xlab("\nCryptocurrency") + ylab("Daily Change (%)\n") + 
  ggtitle("Distribution of the Daily Changes (%)", subtitle = "1 Jan 2016 - 31 Dec 2020\n") + 
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))

# Plot 5
ggplot(data = Daily_change_df) + 
  geom_line(aes(x = Date, y = Ethereum, group = 1), col = "springgreen", alpha = 0.7) +
  xlab("\nDate") + ylab("% Change\n") + 
  ggtitle("Daily Price Changes (%) of Ethereum", subtitle = "1 Jan 2016 - 31 Dec 2020\n") +
  theme_minimal() + 
    theme(plot.background = element_rect(fill = "black"), 
          panel.grid.major = element_line(size = 0.25, linetype = 3,
                                          colour = "grey"),
          panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                          colour = "grey"),
          axis.title.x = element_text(colour = "white"),
          axis.title.y = element_text(colour = "white"),
          axis.text.x = element_text(colour = "white"),
          axis.text.y = element_text(colour = "white"),
          plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
          plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))

# Plot 6
ggplot(data = Daily_change_df) + 
  geom_line(aes(x = Date, y = Stellar, group = 1), col = "steelblue2") +
  xlab("\nDate") + ylab("% Change\n") + 
  ggtitle("Daily Price Changes (%) of Stellar", subtitle = "1 Jan 2016 - 31 Dec 2020\n") +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))

# Setup
Count_lambda = function(Data, thres){
  
  Closing_price = Data$Close
  Pct_change = 100*(Closing_price[2:1827] - Closing_price[1:1826])/Closing_price[1:1826]
  
  if (thres < 0){
    sol = length(which(Pct_change <= thres))
  }else{
    sol = length(which(Pct_change >= thres))
  }
  
  return(sol/1826)
}

# Plot 7 
ggplot(data = Daily_change_df) + 
  geom_line(aes(x = Date, y = Bitcoin, group = 1), col = "gold") +
  xlab("\nDate") + ylab("% Change\n") + 
  ggtitle("Daily Price Changes (%) of Bitcoin", subtitle = "1 Jan 2016 - 31 Dec 2020\n") +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))

# Setup
Dates = Bitcoin$Date
Crypto = c("Bitcoin", "Ethereum", "Litecoin", "NEM", "Stellar", "Monero", "XRP")
N = length(Crypto)
Gain_5 = vector("numeric", N)
Gain_10 = vector("numeric", N)
Gain_15 = vector("numeric", N)
Gain_20 = vector("numeric", N)
Loss_5 = vector("numeric", N)
Loss_10 = vector("numeric", N)
Loss_15 = vector("numeric", N)
Loss_20 = vector("numeric", N)
for (i in 1:N){
  if (i == 1){
    Data = Bitcoin
  }else if (i == 2){
    Data = Ethereum
  }else if (i == 3){
    Data = Litecoin
  }else if (i == 4){
    Data = NEM
  }else if (i == 5){
    Data = Stellar
  }else if (i == 6){
    Data = Monero
  }else{
    Data = XRP
  }
  
  Gain_5[i] = Count_lambda(Data, 5)
  Gain_10[i] = Count_lambda(Data, 10)
  Gain_15[i] = Count_lambda(Data, 15)
  Gain_20[i] = Count_lambda(Data, 20)
  Loss_5[i] = Count_lambda(Data, -5)
  Loss_10[i] = Count_lambda(Data, -10)
  Loss_15[i] = Count_lambda(Data, -15)
  Loss_20[i] = Count_lambda(Data, -20)
}
Gain_lambda = matrix(c(Gain_5, Gain_10, Gain_15, Gain_20), ncol = 4)  #col 1 = +5%, col 2 = +10%, ...
Loss_lambda = matrix(c(Loss_5, Loss_10, Loss_15, Loss_20), ncol = 4)  #Same, but - instead of +

PP_analysis = function(Key, Gain = TRUE){
  Ind = which(Crypto == Key)
  
  if(Gain == TRUE){
    lambdas = Gain_lambda[Ind,]
  }else{
    lambdas = Loss_lambda[Ind,]
  }
  Range = seq(from = 0, to = 50, by = 1)
  
  Over5 = dpois(Range, 365*lambdas[1])
  Over10 = dpois(Range, 365*lambdas[2])
  Over15 = dpois(Range, 365*lambdas[3])
  Over20 = dpois(Range, 365*lambdas[4])
  
  Sol = data.frame(
    Frequency = Range,
    Over_5_percent = Over5,
    Over_10_percent = Over10,
    Over_15_percent = Over15,
    Over_20_percent = Over20
  )
  
  return(Sol)
}


PP_output = PP_analysis("Bitcoin", T)
Gain_status = T
if(Gain_status == TRUE){
  colors = c("Over 5%" = "gold", "Over 10%" = "olivedrab2", "Over 15%" = "springgreen2", "Over 20%" = "seagreen4")
}else{
  colors = c("Over 5%" = "plum1", "Over 10%" = "purple", "Over 15%" = "hotpink", "Over 20%" = "orange")
}

# Plot 8 and 9
ggplot(data = PP_output) +
  geom_area(aes(x = Frequency, y = Over_5_percent, fill = "Over 5%"), size = 2, alpha = 0.6) +
  geom_area(aes(x = Frequency, y = Over_10_percent, fill = "Over 10%"), size = 2, alpha = 0.6) + 
  geom_area(aes(x = Frequency, y = Over_15_percent, fill = "Over 15%"), size = 2, alpha = 0.6) +
  geom_area(aes(x = Frequency, y = Over_20_percent, fill = "Over 20%"), size = 2, alpha = 0.6) +
  labs(x = "\nFrequency",
       y = "Probability\n",
       fill = " ") +
  scale_fill_manual(values = colors)+ ggtitle("Probabilities for Daily Losses of Different Magnitudes",
                                              subtitle = "Period: 1 Year\n") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0, size = 17 , face = "bold")) +
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"), 
        plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))

# Plot 10
ggplot(data = Daily_change_df) + 
  geom_line(aes(x = Date, y = XRP, group = 1), col = "violet") +
  xlab("\nDate") + ylab("% Change\n") + 
  ggtitle("Daily Price Changes (%) of XRP", subtitle = "1 Jan 2016 - 31 Dec 2020\n") +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))

# Setup
Dates = Bitcoin$Date
Crypto = c("Bitcoin", "Ethereum", "Litecoin", "NEM", "Stellar", "Monero", "XRP")
N = length(Crypto)
Gain_5 = vector("numeric", N)
Gain_10 = vector("numeric", N)
Gain_15 = vector("numeric", N)
Gain_20 = vector("numeric", N)
Loss_5 = vector("numeric", N)
Loss_10 = vector("numeric", N)
Loss_15 = vector("numeric", N)
Loss_20 = vector("numeric", N)
for (i in 1:N){
  if (i == 1){
    Data = Bitcoin
  }else if (i == 2){
    Data = Ethereum
  }else if (i == 3){
    Data = Litecoin
  }else if (i == 4){
    Data = NEM
  }else if (i == 5){
    Data = Stellar
  }else if (i == 6){
    Data = Monero
  }else{
    Data = XRP
  }
  
  Gain_5[i] = Count_lambda(Data, 5)
  Gain_10[i] = Count_lambda(Data, 10)
  Gain_15[i] = Count_lambda(Data, 15)
  Gain_20[i] = Count_lambda(Data, 20)
  Loss_5[i] = Count_lambda(Data, -5)
  Loss_10[i] = Count_lambda(Data, -10)
  Loss_15[i] = Count_lambda(Data, -15)
  Loss_20[i] = Count_lambda(Data, -20)
}
Gain_lambda = matrix(c(Gain_5, Gain_10, Gain_15, Gain_20), ncol = 4)  #col 1 = +5%, col 2 = +10%, ...
Loss_lambda = matrix(c(Loss_5, Loss_10, Loss_15, Loss_20), ncol = 4)  #Same, but - instead of +

PP_analysis = function(Key, Gain = TRUE){
  Ind = which(Crypto == Key)
  
  if(Gain == TRUE){
    lambdas = Gain_lambda[Ind,]
  }else{
    lambdas = Loss_lambda[Ind,]
  }
  Range = seq(from = 0, to = 65, by = 1)
  
  Over5 = dpois(Range, 365*lambdas[1])
  Over10 = dpois(Range, 365*lambdas[2])
  Over15 = dpois(Range, 365*lambdas[3])
  Over20 = dpois(Range, 365*lambdas[4])
  
  Sol = data.frame(
    Frequency = Range,
    Over_5_percent = Over5,
    Over_10_percent = Over10,
    Over_15_percent = Over15,
    Over_20_percent = Over20
  )
  
  return(Sol)
}

PP_output = PP_analysis("XRP", F)
Gain_status = F
if(Gain_status == TRUE){
  colors = c("Over 5%" = "gold", "Over 10%" = "olivedrab2", "Over 15%" = "springgreen2", "Over 20%" = "seagreen4")
}else{
  colors = c("Over 5%" = "plum1", "Over 10%" = "purple", "Over 15%" = "hotpink", "Over 20%" = "orange")
}
# Plot 11 and 12
ggplot(data = PP_output) +
  geom_area(aes(x = Frequency, y = Over_5_percent, fill = "Over 5%"), size = 2, alpha = 0.6) +
  geom_area(aes(x = Frequency, y = Over_10_percent, fill = "Over 10%"), size = 2, alpha = 0.6) + 
  geom_area(aes(x = Frequency, y = Over_15_percent, fill = "Over 15%"), size = 2, alpha = 0.6) +
  geom_area(aes(x = Frequency, y = Over_20_percent, fill = "Over 20%"), size = 2, alpha = 0.6) +
  labs(x = "\nFrequency",
       y = "Probability\n",
       fill = " ") + xlim(0,65) +
  scale_fill_manual(values = colors)+ ggtitle("Probabilities for Daily Losses of Different Magnitudes",
                                              subtitle = "Period: 1 Year\n") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0, size = 17 , face = "bold")) +
  theme(plot.background = element_rect(fill = "black"), 
        panel.grid.major = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        panel.grid.minor = element_line(size = 0.25, linetype = 3,
                                        colour = "grey"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"),
        legend.title = element_text(color = "white"),
        legend.text = element_text(color = "white"), 
        plot.title = element_text(colour = "white", hjust = 0.45, size = 20, face = "bold"),
        plot.subtitle = element_text(colour = "white", hjust = 0.45, size = 12))


