library(readr)
library(lubridate)

setwd("C:/Users/wikaw/Desktop/R projekt/sp500")
list.files()
prices  <- read.csv("prices.csv", stringsAsFactors = FALSE)
returns <- read.csv("returns.csv", stringsAsFactors = FALSE)

# Daty biorę z pliku returns i je konwersuje
dates <- parse_date_time(returns$Date, orders = c("dmy", "ymd", "mdy"))

# usuwam wiersze z błędną datą
returns <- returns[!is.na(returns$Date), ]

# synchronizacja długości
n_prices  <- nrow(prices)
n_returns <- nrow(returns)
n_common  <- min(n_prices, n_returns)

prices  <- prices[1:n_common, ]
returns <- returns[1:n_common, ]

# Tworzę sztuczny indeks jako sumę cen spółek
prices$SUMA <- rowSums(prices, na.rm = TRUE)

index_SP500 <- data.frame(
  Date  = as.Date(returns$Date), 
  Price = prices$SUMA
)

# licze stopy zwrotu indeksu w ułamkach
index_SP500$returns_prc <- c(0, diff(index_SP500$Price) / head(index_SP500$Price, -1))

# sprawdzam
any(is.na(index_SP500$Date))   
nrow(index_SP500)              
head(index_SP500)


##### BUY AND HOLD (BH) #####

#wartosc NA zmieniona na 0, żeby możliwe były obliczenia
index_SP500$returns_prc[1] <- 0

# krzywa kapitału
C0 <- 1000
index_SP500$capital <- C0 * cumprod(1 + index_SP500$returns_prc)

library(ggplot2)

ggplot(index_SP500, aes(x = Date, y = capital)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Krzywa kapitału Buy & Hold",
       x = "Data",
       y = "Kapitał (zł)") +
  theme_minimal()


#roczna stopa zwrotu
library(dplyr)
library(lubridate)

annual_returns_BH <- index_SP500 %>%
  mutate(year = year(Date),
         r = returns_prc) %>%
  group_by(year) %>%
  summarise(annual_return = prod(1 + r, na.rm = TRUE) - 1)

print(annual_returns_BH)

# średnia stopa zwrotu z całego okresu 
# a) arytmetyczna  19.7%
mean_annual_return <- mean(annual_returns_BH$annual_return, na.rm = TRUE)
# b) geometryczna 19.4%
CAGR <- (prod(1 + annual_returns_BH$annual_return))^(1/nrow(annual_returns_BH)) - 1

#Zmienność	roczna.: 
ann_vol_BH <- sd(index_SP500$returns_prc, na.rm=TRUE) * sqrt(12)

#ann_vol_BH = 0.161648129

#Sharpe	ratio, stopa wolna od ryzyka ustalona na poziomie 3.7% - 
# oprocentowanie rocznych obligacji skarbowych RP, źródło: Investing.com, 
# dostęp 22.12.2025r.: 0.9477951
rf_monthly <- (1+0.037)^(1/12) - 1

sharpe_BH <- (mean(index_SP500$returns_prc - rf_monthly, na.rm=TRUE) /
                sd(index_SP500$returns_prc, na.rm=TRUE)) * sqrt(12)
# sharpe_BH = 0.95927566

#Maksymalne	obsunięcie (max	drawdown) 
maxDD_BH <- min(index_SP500$capital / cummax(index_SP500$capital) - 1, na.rm = TRUE)

# maxDD_BH = -0.209280872

summary(index_SP500$returns_prc)
head(index_SP500$returns_prc, 20)


###### Momentum 12-1 (M12) ######

library(dplyr)
library(lubridate)
library(ggplot2)

# Dodajemy kolumnę momentum Mt
index_SP500$momentum <- NA

for (t in 13:nrow(index_SP500)) {
  index_SP500$momentum[t] <- (index_SP500$Price[t-1] / index_SP500$Price[t-12]) - 1
}
index_SP500$position <- ifelse(index_SP500$momentum > 0, 1, 0)

# Zwrot strategii = zwrot indeksu * pozycja
index_SP500$momentum_return <- index_SP500$returns_prc * index_SP500$position

index_SP500$momentum_capital <- NA
# Ustawiam początkowy kapitał tam, gdzie zaczynają się transakcje
start_idx <- min(which(!is.na(index_SP500$momentum_return)))
index_SP500$momentum_capital[start_idx] <- 1000

for (t in (start_idx+1):nrow(index_SP500)) {
  if (!is.na(index_SP500$momentum_return[t])) {
    index_SP500$momentum_capital[t] <- index_SP500$momentum_capital[t-1] * (1 + index_SP500$momentum_return[t])
  } else {
    index_SP500$momentum_capital[t] <- index_SP500$momentum_capital[t-1]
  }
}


#średnia miesięczna stopa zwrotu
# a) arytmetyczna->  0.9874133
mean(index_SP500$momentum_return, na.rm = TRUE)*100
#b) geometryczna-> annual 0.1106315
valid_returns <- index_SP500$momentum_return[!is.na(index_SP500$momentum_return)]
geom_mean_monthly <- exp(mean(log(1 + valid_returns))) - 1  # miesięczna
geom_mean_annual <- (1 + geom_mean_monthly)^12 - 1  # roczna

#krzywa kapitału
ggplot(index_SP500, aes(x = Date, y = momentum_capital)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Krzywa kapitału strategii momentum 12–1",
    x = "Data",
    y = "Kapitał (zł)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#zmienność roczna: 0.04663742
sd(index_SP500$momentum_return, na.rm = TRUE)

#Sharpe	ratio, stopa wolna od ryzyka ustalona na poziomie 3.7% - 
# oprocentowanie rocznych obligacji skarbowych RP, źródło: Investing.com, 
# dostęp 22.12.2025r.:  0.1430555
rf_monthly <- (1+0.037)^(1/12) - 1
rf_annual <- 0.037  # 3.7% rocznie
rf_monthly <- (1 + rf_annual)^(1/12) - 1

excess_returns <- valid_returns - rf_monthly
sharpe_monthly <- mean(excess_returns) / sd(valid_returns)
sharpe_annual <- sharpe_monthly * sqrt(12)

# sharpe_annual = 0.5081967

#Test	t	różnicy	średnich	stóp	zwrotu (od 01.12.2017, wtedy pojawia się momentum)

t.test(index_SP500$returns_prc[!is.na(index_SP500$momentum_return)],
       index_SP500$momentum_return[!is.na(index_SP500$momentum_return)],
       paired = TRUE)

####### STRATEGIA MA #########
library(lubridate)
library(zoo)
library(dplyr)
library(ggplot2)

setwd("C:/Users/wikaw/Desktop/R projekt/sp500")
list.files()
prices  <- read.csv("prices.csv", stringsAsFactors = FALSE)
returns <- read.csv("returns.csv", stringsAsFactors = FALSE)

# Daty biorę z pliku returns i je konwersuje
dates <- parse_date_time(returns$Date, orders = c("dmy", "ymd", "mdy"))

# usuwam wiersze z błędną datą
valid_idx <- !is.na(dates)
returns <- returns[valid_idx, ]
prices <- prices[valid_idx, ]
dates <- dates[valid_idx]
# synchronizuje długości
n_common <- min(nrow(prices), nrow(returns))
prices <- prices[1:n_common, ]
returns <- returns[1:n_common, ]
dates <- dates[1:n_common]

#tworzę indeks
if(is.numeric(prices[,1])) {
  prices$SUMA <- rowSums(prices, na.rm = TRUE)
} else {
  prices$SUMA <- rowSums(prices[,-1], na.rm = TRUE)
}

index_SP500 <- data.frame(
  Date = floor_date(as.Date(dates), "month"),
  Price = prices$SUMA
)

# podział danych
n_total <- nrow(index_SP500)
n_in_sample <- round(n_total * 0.7)

in_sample <- index_SP500[1:n_in_sample, ]
out_sample <- index_SP500[(n_in_sample+1):n_total, ]

C0 <- 1000  # kapitał początkowy

# strategia MA
run_ma_strategy <- function(data, fast_period, slow_period, initial_capital) {
  
  # Średnie kroczące
  data$SMA_fast <- rollmean(data$Price, k = fast_period, fill = NA, align = "right")
  data$SMA_slow <- rollmean(data$Price, k = slow_period, fill = NA, align = "right")
  
  # Sygnał i pozycja
  data$signal <- ifelse(data$SMA_fast > data$SMA_slow, 1, 0)
  data$position <- lag(data$signal, n = 1, default = 0)
  
  # Stopy zwrotu
  data$market_return <- c(NA, diff(data$Price) / data$Price[-nrow(data)])
  data$return_strategy <- data$position * data$market_return
  data$return_strategy[is.na(data$return_strategy)] <- 0
  
  # Kapitał
  data$capital <- initial_capital * cumprod(1 + data$return_strategy)
  
  return(data)
}

# dobór parametrów szybkiej i wolnej
fast_period <- 1  # 1 miesiąc (szybka średnia)
slow_period <- 3  # 3 miesiące (wolna średnia)

# konstrukcja strategii na in-sample
in_sample <- run_ma_strategy(in_sample, fast_period, slow_period, C0)

# testowanie na out of sample (zamrożone parametry)
# Kapitał początkowy OOS = ostatni kapitał IS
initial_capital_oos <- tail(in_sample$capital, 1)
out_sample <- run_ma_strategy(out_sample, fast_period, slow_period, initial_capital_oos)

# funkcja do obliczania metryk
calculate_metrics <- function(data, sample_name) {
  
  # Oczyszczone dane (bez NA)
  clean_returns <- data$return_strategy[!is.na(data$return_strategy)]
  clean_capital <- data$capital[!is.na(data$capital)]
  
  if (length(clean_returns) == 0) return(NULL)
  
  # Podstawowe obliczenia
  final_capital <- tail(clean_capital, 1)
  total_return <- (final_capital / C0 - 1) * 100
  
  # CAGR (roczna stopa zwrotu)
  n_months <- length(clean_returns)
  years <- n_months / 12
  cagr <- (final_capital / C0)^(1/years) - 1
  
  # Zmienność roczna
  monthly_vol <- sd(clean_returns)
  annual_vol <- monthly_vol * sqrt(12)
  
  # Sharpe ratio
  rf_monthly <- (1 + 0.037)^(1/12) - 1
  sharpe_monthly <- (mean(clean_returns) - rf_monthly) / monthly_vol
  sharpe_annual <- sharpe_monthly * sqrt(12)
  
  # Maksymalne obsunięcie
  running_max <- cummax(clean_capital)
  drawdown <- (clean_capital / running_max) - 1
  max_drawdown <- min(drawdown) * 100
  
  # Wyniki
  results <- data.frame(
    Próbka = sample_name,
    `Kapitał końcowy` = round(final_capital, 2),
    `Całkowity zwrot (%)` = round(total_return, 2),
    `Roczna stopa zwrotu (%)` = round(cagr * 100, 2),
    `Zmienność roczna (%)` = round(annual_vol * 100, 2),
    `Sharpe ratio` = round(sharpe_annual, 3),
    `Max drawdown (%)` = round(max_drawdown, 2),
    check.names = FALSE
  )
  
  return(results)
}

# obliczam dla in sample i out of sample
metrics_is <- calculate_metrics(in_sample, "In-Sample")
metrics_oos <- calculate_metrics(out_sample, "Out-of-Sample")

# Obliczenie zwrotów Buy & Hold
index_SP500$bh_return <- c(NA, diff(index_SP500$Price) / index_SP500$Price[-nrow(index_SP500)])
index_SP500$bh_return[is.na(index_SP500$bh_return)] <- 0

# Podział zwrotów BH na IS i OOS
bh_returns_is <- index_SP500$bh_return[2:(n_in_sample)]
bh_returns_oos <- index_SP500$bh_return[(n_in_sample+1):n_total]

# Przygotowanie danych do testu
is_returns_clean <- in_sample$return_strategy[!is.na(in_sample$return_strategy)]
oos_returns_clean <- out_sample$return_strategy[!is.na(out_sample$return_strategy)]

# Test t dla in-sample
if (length(is_returns_clean) > 1 && length(bh_returns_is) > 1) {
  min_length <- min(length(is_returns_clean), length(bh_returns_is))
  t_test_is <- t.test(
    is_returns_clean[1:min_length],
    bh_returns_is[1:min_length],
    paired = TRUE
  )
}

# Test t dla out-of-sample
if (length(oos_returns_clean) > 1 && length(bh_returns_oos) > 1) {
  min_length <- min(length(oos_returns_clean), length(bh_returns_oos))
  t_test_oos <- t.test(
    oos_returns_clean[1:min_length],
    bh_returns_oos[1:min_length],
    paired = TRUE
  )
}


# Krzywa kapitału
all_data <- rbind(
  in_sample %>% mutate(Próbka = "In-Sample"),
  out_sample %>% mutate(Próbka = "Out-of-Sample")
)

ggplot(all_data, aes(x = Date, y = capital, color = Próbka, linetype = Próbka)) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = in_sample$Date[n_in_sample], 
             linetype = "dashed", color = "gray") +
  labs(
    title = "Strategia MA Crossover - Krzywa kapitału",
    subtitle = paste("Parametry: SMA", fast_period, "/", slow_period),
    x = "Data",
    y = "Kapitał (zł)",
    color = "",
    linetype = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("In-Sample" = "blue", "Out-of-Sample" = "red"))

######## MINI RISK PARITY ##########
library(tidyverse)
library(PerformanceAnalytics)
library(zoo)
library(xts)
library(quadprog)
library(readr)
library(ggplot2)

# Wczytanie danych
setwd("C:/Users/wikaw/Desktop/R projekt/sp500")
list.files()
returns_raw <- read.csv("returns.csv")

# Data jako indeks czasu
dates <- as.Date(returns_raw$Date)
returns_mat <- returns_raw %>%
  select(-Date) %>%
  as.matrix()

returns_xts <- xts(returns_mat, order.by = dates)

# podział IS/OOS
n <- nrow(returns_xts)
split_point <- floor(0.7 * n)

ret_IS  <- returns_xts[1:split_point, ]
ret_OOS <- returns_xts[(split_point+1):n, ]

# funkcja: wagi Risk Parity każdy składnik wnosi taki sam marginal contribution to risk
risk_parity_weights <- function(cov_mat) {
  n <- ncol(cov_mat)
  w <- rep(1/n, n)
  
  for (i in 1:100) {
    mrc <- cov_mat %*% w
    rc  <- w * mrc
    
    rc[rc <= 0] <- 1e-8  # zabezpieczenie
    
    w <- w * (mean(rc) / rc)
    w <- w / sum(w)
  }
  return(as.numeric(w))
}

# strategia mini risk parity: okno 2-6 miesięcy
mini_rp_strategy <- function(returns_data, window, n_assets) {
  
  T <- nrow(returns_data)
  N <- ncol(returns_data)
  
  weights_all <- matrix(0, T, N)
  
  for (t in (window+1):(T-1)) {
    
    hist_data <- returns_data[(t-window):(t-1), ]
    
    # wybór najmniej zmiennych akcji
    vols <- apply(hist_data, 2, sd, na.rm = TRUE)
    vols[is.na(vols)] <- Inf
    
    selected <- order(vols)[1:n_assets]
    
    cov_mat <- cov(hist_data[, selected], use = "pairwise.complete.obs")
    
    if (any(is.na(cov_mat)) || det(cov_mat) <= 1e-10) next
    
    w_sub <- risk_parity_weights(cov_mat)
    
    weights_all[t, selected] <- w_sub
  }
  
  port_ret <- xts(rowSums(weights_all * coredata(returns_data)), order.by = index(returns_data))
  port_ret <- xts(port_ret, order.by = index(returns_data))
  
  # usuwamy początkowy okres bez pozycji
  port_ret <- port_ret[(window+1):nrow(port_ret)]
  
  return(port_ret)
}

# dobór parametrów in-sample 
# liczba aktywów: 2,3,4
# okno: 2,3,4,5,6 miesięcy
windows <- 2:6
assets_n <- 2:4

results_IS <- data.frame()

for (w in windows) {
  for (k in assets_n) {
    
    strat_ret <- mini_rp_strategy(ret_IS, window = w, n_assets = k)
    
    ann_ret <- as.numeric(Return.annualized(strat_ret))
    ann_vol <- as.numeric(StdDev.annualized(strat_ret))
    sharpe  <- as.numeric(SharpeRatio.annualized(strat_ret))
    max_dd  <- as.numeric(maxDrawdown(strat_ret))
    
    results_IS <- rbind(results_IS, data.frame(
      Window_months = w,
      Assets = k,
      AnnReturn = ann_ret,
      AnnVol = ann_vol,
      Sharpe = sharpe,
      MaxDD = max_dd
    ))
  }
}

results_IS[order(-results_IS$Sharpe), ]

# wybór najlepszego zestawu
best_row <- results_IS[which.max(results_IS$Sharpe), ]
best_window <- best_row$Window_months
best_assets <- best_row$Assets

best_window  # 4
best_assets  # 2

# test out-of-sample
rp_IS_ret  <- mini_rp_strategy(ret_IS,  best_window, best_assets)
rp_OOS_ret <- mini_rp_strategy(ret_OOS, best_window, best_assets)

# statystyki
get_stats <- function(ret) {
  c(
    AnnReturn = Return.annualized(ret),
    AnnVol    = StdDev.annualized(ret),
    Sharpe    = SharpeRatio.annualized(ret),
    MaxDD     = maxDrawdown(ret)
  )
}

rbind(
  IS  = get_stats(rp_IS_ret),
  OOS = get_stats(rp_OOS_ret)
)
#      AnnReturn    AnnVol   Sharpe      MaxDD
# IS  0.21351005 0.1745672 1.223082 0.11444660
# OOS 0.04636221 0.1360741 0.340713 0.08382016

# t-test
t_IS  <- t.test(coredata(rp_IS_ret))
t_OOS <- t.test(coredata(rp_OOS_ret))

t_IS 
# t = 2.1378, df = 37, p-value = 0.0392
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#  0.0009126319 0.0340403130
# sample estimates:
#   mean of x 
# 0.01747647 
t_OOS
# t = 0.42777, df = 13, p-value = 0.6758
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   -0.01818944  0.02717115
# sample estimates:
#  mean of x 
# 0.004490853 

# krzywa kapitału
calculate_capital_curve <- function(returns, initial_capital = 1000) {
  capital <- initial_capital * cumprod(1 + returns)
  return(capital)
}

capital_IS <- calculate_capital_curve(rp_IS_ret, 1000)
capital_OOS <- calculate_capital_curve(rp_OOS_ret, 1000)

# dane do wykresu
capital_data <- data.frame(
  Date = c(index(capital_IS), index(capital_OOS)),
  Capital = c(coredata(capital_IS), coredata(capital_OOS)),
  Sample = c(rep("In-Sample", length(capital_IS)), 
             rep("Out-of-Sample", length(capital_OOS)))
)

# wykres krzywej kapitału
library(ggplot2)

capital_plot <- ggplot(capital_data, aes(x = Date, y = Capital, color = Sample)) +
  geom_line(size = 1.2) +
  labs(
    title = "Krzywa kapitału strategii Mini Risk Parity",
    subtitle = paste("Parametry: okno =", best_window, "miesięcy, aktywa =", best_assets),
    x = "Data",
    y = "Kapitał (zł)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("In-Sample" = "blue", "Out-of-Sample" = "red")) +
  theme(legend.position = "bottom")

print(capital_plot)


# rolling sharpe
rolling_sharpe_safe <- function(ret) {
  n <- nrow(ret)
  
  width <- min(24, floor(n/2))
  width <- max(width, 6)  # minimum 6 okresów
  
  rollapply(ret, width, SharpeRatio.annualized,
            by.column = FALSE, align = "right")
}

roll_sharpe_IS  <- rolling_sharpe_safe(rp_IS_ret)
roll_sharpe_OOS <- rolling_sharpe_safe(rp_OOS_ret)

chart.TimeSeries(roll_sharpe_IS,  main = "Rolling Sharpe – Mini Risk Parity (IS)")
chart.TimeSeries(roll_sharpe_OOS, main = "Rolling Sharpe – Mini Risk Parity (OOS)")

##### WYKRES PORÓWNUJĄCY WSZYSTKIE STRATEGIE (OOS) #####
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

C0 <- 1000

# BUY & HOLD OOS
# Zwroty tylko w okresie OOS
index_oos <- out_sample
bh_oos_returns <- c(0, diff(index_oos$Price) / head(index_oos$Price, -1))

# Usuń pierwszy element jeśli ma NA
if(any(is.na(bh_oos_returns))) {
  bh_oos_returns[is.na(bh_oos_returns)] <- 0
}

bh_oos_capital <- C0 * cumprod(1 + bh_oos_returns)

bh_df <- data.frame(
  Date = index_oos$Date,
  BuyHold = bh_oos_capital
)

# MOMENTUM 12-1 OOS
# Musimy mieć dane historyczne dla obliczenia momentum
# Dlatego potrzebujemy danych sprzed OOS

# Tworzę pełny szereg cen od początku danych
full_prices <- rbind(in_sample, out_sample)
full_prices$full_index <- 1:nrow(full_prices)

# Obliczam momentum dla całego okresu
full_prices$momentum <- NA
for (t in 13:nrow(full_prices)) {
  full_prices$momentum[t] <- (full_prices$Price[t-1] / full_prices$Price[t-12]) - 1
}

full_prices$position <- ifelse(full_prices$momentum > 0, 1, 0)
full_prices$bh_return <- c(0, diff(full_prices$Price) / head(full_prices$Price, -1))
full_prices$mom_return <- full_prices$position * full_prices$bh_return

# Wybieram tylko OOS okres
oos_start_idx <- nrow(in_sample) + 1
momentum_oos <- full_prices[oos_start_idx:nrow(full_prices), ]
momentum_oos$mom_return[is.na(momentum_oos$mom_return)] <- 0

mom_oos_capital <- C0 * cumprod(1 + momentum_oos$mom_return)

mom_df <- data.frame(
  Date = momentum_oos$Date,
  Momentum = mom_oos_capital
)

# MA 
ma_df <- data.frame(
  Date = out_sample$Date,
  MA = C0 * (out_sample$capital / out_sample$capital[1])
)

# MINI RISK PARITY
rp_oos_capital <- C0 * cumprod(1 + coredata(rp_OOS_ret))

rp_df <- data.frame(
  Date = index(rp_OOS_ret),
  MiniRiskParity = as.numeric(rp_oos_capital)
)

# łącze dane
comparison_oos <- bh_df %>%
  full_join(mom_df, by = "Date") %>%
  full_join(ma_df, by = "Date") %>%
  full_join(rp_df, by = "Date") %>%
  arrange(Date)

# Przekształcenie do formatu długiego
comparison_long <- comparison_oos %>%
  pivot_longer(
    cols = -Date,
    names_to = "Strategy",
    values_to = "Capital"
  )

# WYKRES
ggplot(comparison_long, 
       aes(Date, Capital, color = Strategy)) +
  geom_line(size = 1.2, na.rm = TRUE) + 
  labs(title = "Porównanie wartości portfela w czasie (OOS)",
       subtitle = "Start = 1000 USD na początku okresu testowego",
       x = NULL,
       y = "Kapitał ($)") +
  theme_minimal() +
  theme(
    legend.position = "right",  
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.key = element_rect(fill = "white", color = NA),
    legend.key.width = unit(1.5, "cm")
  ) +
  scale_color_manual(
    values = c(
      "BuyHold" = "black",
      "Momentum" = "red",
      "MA" = "blue",
      "MiniRiskParity" = "green"
    ),
    labels = c(
      "BuyHold" = "BH",
      "Momentum" = "M12",
      "MA" = "SMA",
      "MiniRiskParity" = "MRP"
    )

  )
