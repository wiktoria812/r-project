setwd("C:/Users/wikaw/Desktop/R projekt/crypto")
Ethereum <- read.csv("coin_Ethereum.csv")
Ethereum$Date <- as.Date(Ethereum$Date)


# wyrzucam ceny max, min i open, będę operowała tylko na cenach zamknięcia w każdy dzień
Ethereum$High <- NULL
Ethereum$Low  <- NULL
Ethereum$Open <- NULL

colnames(Ethereum)[colnames(Ethereum) == "Close"] <- "price"

Ethereum$returns <- 
  c(NA, round((diff(Ethereum$price) / head(Ethereum$price, -1)), 2))



##### Buy and Hold (BH) #####

#wartosc NA zmieniona na 0, żeby zrobić krzywą kapitału
Ethereum$returns[1] <- 0

# krzywa kapitału
C0 <- 1000
Ethereum$capital <- C0 * cumprod(1 + Ethereum$returns)

library(ggplot2)
library(scales)


ggplot(Ethereum, aes(x = Date, y = capital)) +
  geom_line(color = "blue", linewidth = 1.2) +
  scale_y_continuous(
    n.breaks = 6,
    labels = scales::label_number(scale = 1e-6, suffix = " mln")
  ) +
  labs(title = "Krzywa kapitału Buy & Hold",
       x = "Data",
       y = "Kapitał ($)") +
  theme_minimal()


#roczna stopa zwrotu
library(dplyr)
library(lubridate)

annual_returns_BH <- Ethereum %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(
    end_capital = last(capital)
  ) %>%
  mutate(
    annual_return = end_capital / lag(end_capital) - 1
  )

annual_returns_BH <- annual_returns_BH %>%
  mutate(annual_return = round(annual_return, 2))
annual_returns_BH$annual_return[1] <- NA

# średnia stopa zwrotu z całego okresu (wart początkowa = 1)
# a) arytmetyczna 
mean(annual_returns_BH$annual_return)

# b) geometryczna 
round((tail(annual_returns_BH$end_capital, 1) / head(annual_returns_BH$end_capital, 1))^(1 / (nrow(annual_returns_BH) - 1)) - 1, 4)

#Zmienność	roczna.: 1,21 (roczna zmienność stóp zwrotu wynosi 121%)
#si_roczne = si_dzienne * sqrt(365)

round(sd(Ethereum$returns, na.rm = TRUE)*sqrt(365),2)

#Sharpe	ratio, stopa wolna od ryzyka ustalona na poziomie 3.7% - 
# oprocentowanie rocznych obligacji skarbowych RP, źródło: Investing.com, 
# dostęp 22.12.2025r.:  1,6848 (dobry zwrot w stosunku do ponoszonego ryzyka)

(round((mean(Ethereum$returns - ((1 + 0.037)^(1/365) - 1), na.rm = TRUE) /
     sd(Ethereum$returns, na.rm = TRUE)) * sqrt(365),4))

#Maksymalne	obsunięcie (max	drawdown).:  -0,94 (ekstremalne obunięcie - w najgorszym momencie kapitał spadł
# o 94 % w stosunku do wcześniejszego szczytu)
round(min(Ethereum$capital / cummax(Ethereum$capital) - 1, na.rm = TRUE),2)

#Rolling Sharpe - relacja zysku do ryzyka w ciągu ostatniego roku
# Rolling Sharpe ≠ prognoza, np. wart > 1 mówi tylko, że ostatnie 12 miesięcy było dobre w stosunku do ryzyka
# diagnostyka reżimu, identyfikacja hossy/bessy i okresów chaosu

library(zoo)

window <- 365  # 12 miesięcy

Ethereum$rolling_sharpe <- rollapply(
  data = Ethereum$returns,
  width = window,
  FUN = function(x) {
    mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(365)
  },
  by.column = FALSE,
  align = "right",
  fill = NA
)

Ethereum$rolling_sharpe <- round(Ethereum$rolling_sharpe,4)

########################################### Momentum 12-1 (M12) #################################################

library(dplyr)
library(lubridate)

ETH_monthly <- Ethereum %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(price = last(price))

ETH_monthly$momentum <- NA

for (t in 13:nrow(ETH_monthly)) {
  ETH_monthly$momentum[t] <- (ETH_monthly$price[t-1] /
                                ETH_monthly$price[t-12]) - 1
}

ETH_monthly$position <- ifelse(ETH_monthly$momentum > 0, 1, 0)

#przenosimy na dane dzienne (zmiana pozycji raz w miesiącu)
Ethereum <- Ethereum %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  left_join(ETH_monthly %>% select(YearMonth,momentum, position),
            by = "YearMonth")

#zwrot strategii
Ethereum$momentum_return <- Ethereum$returns * Ethereum$position

#Kapitał strategii
Ethereum$momentum_capital <- NA
Ethereum$momentum_capital[1] <- 1000

for (t in 2:nrow(Ethereum)) {
  Ethereum$momentum_capital[t] <- Ethereum$momentum_capital[t-1] *
    (1 + ifelse(is.na(Ethereum$momentum_return[t]), 0, Ethereum$momentum_return[t]))
}

#krzywa kapitału
ggplot(Ethereum, aes(x = Date, y = momentum_capital)) +
  geom_line(color = "blue", linewidth = 1.2) +
  scale_y_continuous(
    n.breaks = 6,
    labels = scales::label_number(scale = 1e-3, suffix = " tys")
  ) +
  labs(title = "Krzywa kapitału Momenmtum 12-1",
       x = "Data",
       y = "Kapitał ($)") +
  theme_minimal()


#roczna stopa zwrotu
library(dplyr)
library(lubridate)

annual_returns_M12 <- Ethereum %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(
    end_capital = last(capital)
  ) %>%
  mutate(
    annual_return = end_capital / lag(end_capital) - 1
  )

annual_returns_M12 <- annual_returns_M12 %>%
  mutate(annual_return = round(annual_return, 2))
annual_returns_M12$annual_return[1] <- NA

# średnia stopa zwrotu z całego okresu (wart początkowa = 1)
# a) arytmetyczna 
round(mean(annual_returns_M12$annual_return, na.rm = TRUE),2)

# b) geometryczna 
round((tail(annual_returns_M12$end_capital, 1) 
       / head(annual_returns_M12$end_capital, 1))^(1 / (nrow(annual_returns_M12) - 1)) - 1, 2)

#Zmienność	roczna.: 0,05 (roczna zmienność stóp zwrotu wynosi 5%)
#si_roczne = si_dzienne * sqrt(365)
round(sd(Ethereum$momentum_return, na.rm = TRUE),2)

#Sharpe	ratio, stopa wolna od ryzyka ustalona na poziomie 3.7% - 
# oprocentowanie rocznych obligacji skarbowych RP, źródło: Investing.com, 
# dostęp 22.12.2025r.: 1,511  (dobry zwrot w stosunku do ponoszonego ryzyka)

(round((mean(Ethereum$momentum_return - ((1 + 0.037)^(1/365) - 1), na.rm = TRUE) /
          sd(Ethereum$momentum_return, na.rm = TRUE)) * sqrt(365),4))

#Maksymalne	obsunięcie (max	drawdown).:  -0,88 (ekstremalne obsunięcie - w najgorszym momencie kapitał spadł
# o 88 % w stosunku do wcześniejszego szczytu)
round(min(Ethereum$momentum_capital / cummax(Ethereum$momentum_capital) - 1, na.rm = TRUE),2)

#Rolling Sharpe
Ethereum$rolling_sharpe_momentum <- rollapply(
  data = Ethereum$momentum_return,
  width = window,
  FUN = function(x) {
    mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(365)
  },
  by.column = FALSE,
  align = "right",
  fill = NA
)

Ethereum$rolling_sharpe_momentum <- round(Ethereum$rolling_sharpe_momentum,4)


#Test	t	różnicy	średnich	stóp	zwrotu (od 01.12.2017, wtedy pojawia się momentum)

t.test(Ethereum$returns[!is.na(Ethereum$momentum_return)],
       Ethereum$momentum_return[!is.na(Ethereum$momentum_return)],
       paired = TRUE)

#p-value rzędu 0,51 > 0,05 - brak podstaw do odrzucenia H0
#brak istotnej statystycznie różnicy w średnich stopach zwrotu

#Interpretacja

#p-value > 0.05 → nie ma statystycznie istotnej różnicy między dziennymi zwrotami strategii momentum i buy-and-hold.

#Średnia różnica ≈ 0.00039 → dziennie momentum przynosi średnio +0.039% więcej, ale efekt jest bardzo mały i niestatystyczny.

#Przedział ufności obejmuje 0 → nie możemy odrzucić hipotezy, że średnia różnica wynosi 0.

#momentum redukuje ryzyko, chroni kapitał w trendach spadkowych


###################### Mean reversion: lookback 2–10 dni ######################

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)

# Przygotowanie danych
df_clean <- Ethereum %>%
  select(Date, price) %>%
  na.omit()

# Podział 70/30
n <- nrow(df_clean)
split_idx <- round(0.7 * n)
train_set <- df_clean[1:split_idx, ]
test_set  <- df_clean[(split_idx + 1):n, ]

# Funkcja SMA
get_sma <- function(x, n) {
  res <- rep(NA, length(x))
  for(i in n:length(x)) res[i] <- mean(x[(i-n+1):i])
  return(res)
}

# OPTYMALIZACJA (In-Sample) 
best_lb <- 2
max_ret <- -Inf

for (lb in 2:10) {
  sma_v <- get_sma(train_set$price, lb)
  sig <- ifelse(train_set$price < sma_v, 1, -1)
  sig <- c(NA, sig[-length(sig)])
  
  ret <- c(NA, diff(log(train_set$price)))
  total_p <- sum(sig * ret, na.rm = TRUE)
  
  if (!is.na(total_p) && total_p > max_ret) {
    max_ret <- total_p
    best_lb <- lb
  }
}

cat("Zoptymalizowany lookback:", best_lb, "\n")

# --- TEST (Out-of-Sample) ---
sma_oos <- get_sma(test_set$price, best_lb)
sig_oos <- ifelse(test_set$price < sma_oos, 1, -1)
sig_oos <- c(NA, sig_oos[-length(sig_oos)])

ret_oos <- c(NA, diff(test_set$price) / head(test_set$price, -1))
strat_ret_oos <- sig_oos * ret_oos

# Krzywa kapitału
df_MR <- test_set %>%
  mutate(
    returns = c(NA, diff(price) / head(price, -1)),
    position = sig_oos,
    strategy_return = ifelse(is.na(position * returns), 0, position * returns),
    capital = 1000 * cumprod(1 + strategy_return)
  )

# Wykres kapitału
ggplot(df_MR, aes(x = Date, y = capital)) +
  geom_line(color = "blue", linewidth = 0.8) +
  scale_y_continuous(
    n.breaks = 6,
    labels = scales::label_number(scale = 1e-6, suffix = " mln")
  ) +
  labs(
    title = paste("Krzywa kapitału Mean Reversion – Ethereum (LB =", best_lb, ")"),
    subtitle = "Zbiór out-of-sample",
    x = "Data",
    y = "Kapitał ($)"
  ) +
  theme_minimal()

#Wskaźniki

# Roczne stopy zwrotu
annual_returns_MR <- df_MR %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(
    annual_return = last(capital) / first(capital) - 1,
    end_capital = last(capital)
  )

# a) średnia arytmetyczna
mean_arithmetic_MR <- mean(annual_returns_MR$annual_return, na.rm = TRUE)

# b) średnia geometryczna (CAGR)
n_years_MR <- max(1, nrow(annual_returns_MR) - 1)
mean_geometric_MR <- (tail(df_MR$capital, 1) / head(df_MR$capital, 1))^(1 / n_years_MR) - 1

# c) zmienność roczna
volatility_annual_MR <- sd(df_MR$strategy_return, na.rm = TRUE) * sqrt(365)

# d) Sharpe ratio
rf_daily <- (1 + 0.037)^(1/365) - 1
sharpe_ratio_MR <- (mean(df_MR$strategy_return - rf_daily, na.rm = TRUE) /
                      sd(df_MR$strategy_return, na.rm = TRUE)) * sqrt(365)

# e) Max drawdown
max_drawdown_MR <- min(df_MR$capital / cummax(df_MR$capital) - 1, na.rm = TRUE)

# f) Rolling Sharpe
df_MR$rolling_sharpe <- rollapply(
  data = df_MR$strategy_return,
  width = min(365, nrow(df_MR)),
  FUN = function(x) {
    if(sd(x, na.rm=TRUE) == 0) return(NA)
    mean(x, na.rm = TRUE) / sd(x, na.rm = TRUE) * sqrt(365)
  },
  by.column = FALSE, align = "right", fill = NA
)

# Tabela wyników
summary_table_MR <- data.frame(
  Wskaźnik = c(
    "Średnia roczna stopa (arytmetyczna)",
    "Średnia roczna stopa (geometryczna)",
    "Roczna zmienność",
    "Sharpe ratio",
    "Maksymalne obsunięcie",
    "Kapitał końcowy"
  ),
  Wartość = c(
    paste0(round(mean_arithmetic_MR * 100, 2), "%"),
    paste0(round(mean_geometric_MR * 100, 2), "%"),
    paste0(round(volatility_annual_MR * 100, 2), "%"),
    round(sharpe_ratio_MR, 2),
    paste0(round(max_drawdown_MR * 100, 2), "%"),
    paste0(round(tail(df_MR$capital,1), 2), "$")
  )
)

summary_table_MR
###################### Volatility Targeting (VT) ###############################

library(zoo)
library(dplyr)
library(lubridate)

# Podział danych 70/30
n <- nrow(Ethereum)
split_idx <- round(0.7 * n)
train_set_VT <- Ethereum[1:split_idx, ]
test_set_VT  <- Ethereum[(split_idx + 1):n, ]

# Funkcja do obliczania zrealizowanej zmienności (rocznej)
get_vol <- function(returns, window) {
  rollapply(returns, width = window, FUN = sd,
            fill = NA, align = "right") * sqrt(365)
}

train_set_VT$returns <- 
  c(NA, diff(train_set_VT$price) / head(train_set_VT$price, -1))

# OPTYMALIZACJA (In-Sample)
targets <- c(0.10, 0.15)        # cele zmienności 10% i 15%
windows <- seq(20, 60, by = 10)  # 20-60 dni co 10

best_params <- list(target = 0, window = 0)
max_sharpe_is <- -Inf

for (tgt in targets) {
  for (win in windows) {
    vol_is <- get_vol(train_set_VT$returns, win)
    weight <- tgt / lag(vol_is)
    weight[weight > 2] <- 2     # limit dźwigni
    
    strat_ret_is <- weight * train_set_VT$returns
    
    sharpe_is <- (mean(strat_ret_is, na.rm=T) /
                    sd(strat_ret_is, na.rm=T)) * sqrt(365)
    
    if (!is.na(sharpe_is) && sharpe_is > max_sharpe_is) {
      max_sharpe_is <- sharpe_is
      best_params$target <- tgt
      best_params$window <- win
    }
  }
}

cat("Najlepsze parametry VT:",
    "Cel =", best_params$target,
    "Okno =", best_params$window, "\n")

# TEST (Out-of-Sample)
test_set_VT$returns <- 
  c(NA, diff(test_set_VT$price) / head(test_set_VT$price, -1))

vol_oos <- get_vol(test_set_VT$returns, best_params$window)
weight_oos <- best_params$target / lag(vol_oos)
weight_oos[weight_oos > 2] <- 2

df_VT <- test_set_VT %>%
  mutate(
    position = weight_oos,
    strategy_return = ifelse(is.na(position * returns), 0, position * returns),
    capital = 1000 * cumprod(1 + strategy_return)
  )

# Wykres kapitału
ggplot(df_VT, aes(x = Date, y = capital)) +
  geom_line(color="blue", linewidth=1) +
  labs(
    title = paste("Volatility Targeting – Ethereum",
                  "(Cel =", best_params$target*100,
                  "%, Okno =", best_params$window, "dni)"),
    x="Data", y="Kapitał ($)"
  ) +
  theme_minimal()

# WSKAŹNIKI 
annual_returns_VT <- df_VT %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(
    annual_return = last(capital) / first(capital) - 1
  )

# a) Średnia arytmetyczna
mean_arithmetic_VT <- mean(annual_returns_VT$annual_return, na.rm=TRUE)

# b) Geometryczna
mean_geometric_VT <- 
  (tail(df_VT$capital,1) / head(df_VT$capital,1))^(1/(nrow(annual_returns_VT)-1)) - 1

# c) Zmienność
volatility_annual_VT <- sd(df_VT$strategy_return, na.rm=TRUE)*sqrt(365)

# d) Sharpe
rf_daily <- (1+0.037)^(1/365)-1
sharpe_ratio_VT <- 
  (mean(df_VT$strategy_return - rf_daily, na.rm=TRUE) /
     sd(df_VT$strategy_return, na.rm=TRUE))*sqrt(365)

# e) Max Drawdown
max_drawdown_VT <- 
  min(df_VT$capital / cummax(df_VT$capital) - 1, na.rm=TRUE)

# Tabela
summary_table_VT <- data.frame(
  Wskaźnik = c(
    "Średnia roczna (aryt.)",
    "Średnia roczna (geom.)",
    "Zmienność roczna",
    "Sharpe",
    "Max Drawdown",
    "Kapitał końcowy"
  ),
  Wartość = c(
    paste0(round(mean_arithmetic_VT*100,2),"%"),
    paste0(round(mean_geometric_VT*100,2),"%"),
    paste0(round(volatility_annual_VT*100,2),"%"),
    round(sharpe_ratio_VT,2),
    paste0(round(max_drawdown_VT*100,2),"%"),
    paste0(round(tail(df_VT$capital,1),2),"$")
  )
)

# Tabela podsumowująca dla Buy & Hold
summary_table_BH <- data.frame(
  Wskaźnik = c(
    "Średnia roczna (aryt.)",
    "Średnia roczna (geom.)", 
    "Zmienność roczna",
    "Sharpe",
    "Max Drawdown",
    "Kapitał końcowy"
  ),
  Wartość = c(
    # Średnia arytmetyczna
    paste0(round(mean(annual_returns_BH$annual_return, na.rm = TRUE) * 100, 2), "%"),
    
    # Średnia geometryczna
    paste0(round((tail(annual_returns_BH$end_capital, 1) / 
                    head(annual_returns_BH$end_capital, 1))^(1/(nrow(annual_returns_BH)-1)) - 1, 4) * 100, "%"),
    
    # Zmienność roczna
    paste0(round(sd(Ethereum$returns, na.rm = TRUE) * sqrt(365) * 100, 2), "%"),
    
    # Sharpe ratio
    as.character(round((mean(Ethereum$returns - ((1 + 0.037)^(1/365) - 1), na.rm = TRUE) /
                          sd(Ethereum$returns, na.rm = TRUE)) * sqrt(365), 4)),
    
    # Max Drawdown
    paste0(round(min(Ethereum$capital / cummax(Ethereum$capital) - 1, na.rm = TRUE) * 100, 2), "%"),
    
    # Kapitał końcowy
    paste0(round(tail(Ethereum$capital, 1), 2), "$")
  )
)

# Poprawiam błąd w annual_returns_M12 - używam momentum_capital zamiast capital
annual_returns_M12 <- Ethereum %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(
    end_capital = last(momentum_capital)  
  ) %>%
  mutate(
    annual_return = end_capital / lag(end_capital) - 1
  )

annual_returns_M12 <- annual_returns_M12 %>%
  mutate(annual_return = round(annual_return, 2))
annual_returns_M12$annual_return[1] <- NA

# Tabela podsumowująca dla Momentum 12-1
summary_table_M12 <- data.frame(
  Wskaźnik = c(
    "Średnia roczna (aryt.)",
    "Średnia roczna (geom.)", 
    "Zmienność roczna",
    "Sharpe",
    "Max Drawdown",
    "Kapitał końcowy"
  ),
  Wartość = c(
    # Średnia arytmetyczna
    paste0(round(mean(annual_returns_M12$annual_return, na.rm = TRUE) * 100, 2), "%"),
    
    # Średnia geometryczna
    paste0(round((tail(annual_returns_M12$end_capital, 1) / 
                    head(annual_returns_M12$end_capital, 1))^(1/(nrow(annual_returns_M12)-1)) - 1, 2) * 100, "%"),
    
    # Zmienność roczna (POPRAWIONE: dodaj sqrt(365))
    paste0(round(sd(Ethereum$momentum_return, na.rm = TRUE) * sqrt(365) * 100, 2), "%"),
    
    # Sharpe ratio
    as.character(round((mean(Ethereum$momentum_return - ((1 + 0.037)^(1/365) - 1), na.rm = TRUE) /
                          sd(Ethereum$momentum_return, na.rm = TRUE)) * sqrt(365), 4)),
    
    # Max Drawdown
    paste0(round(min(Ethereum$momentum_capital / cummax(Ethereum$momentum_capital) - 1, na.rm = TRUE) * 100, 2), "%"),
    
    # Kapitał końcowy
    paste0(round(tail(Ethereum$momentum_capital, 1), 2), "$")
  )
)

# Ujednolicam nazwy wskaźników w summary_table_MR
summary_table_MR <- data.frame(
  Wskaźnik = c(
    "Średnia roczna (aryt.)",           # Zamiast: "Średnia roczna stopa (arytmetyczna)"
    "Średnia roczna (geom.)",           # Zamiast: "Średnia roczna stopa (geometryczna)"
    "Zmienność roczna",                 # Zamiast: "Roczna zmienność"
    "Sharpe",                           # Zamiast: "Sharpe ratio"
    "Max Drawdown",                     # Zamiast: "Maksymalne obsunięcie"
    "Kapitał końcowy"                   # OK
  ),
  Wartość = c(
    paste0(round(mean_arithmetic_MR * 100, 2), "%"),
    paste0(round(mean_geometric_MR * 100, 2), "%"),
    paste0(round(volatility_annual_MR * 100, 2), "%"),
    as.character(round(sharpe_ratio_MR, 2)),
    paste0(round(max_drawdown_MR * 100, 2), "%"),
    paste0(round(tail(df_MR$capital, 1), 2), "$")
  )
)

################ Porównanie 4 strategii ################

summary_all <- data.frame(
  Wskaźnik = summary_table_BH$Wskaźnik,
  `Buy & Hold` = summary_table_BH$Wartość,
  `Momentum 12-1` = summary_table_M12$Wartość,
  `Mean Reversion` = summary_table_MR$Wartość,
  `Volatility Targeting` = summary_table_VT$Wartość,
  check.names = FALSE
)

summary_all


###################### WIZUALIZACJA KRZYWYCH KAPITAŁU (OOS) ######################

# Okres OOS taki sam jak dla VT
common_dates <- df_VT$Date

# Buy & Hold 
df_BH_test <- Ethereum %>%
  filter(Date %in% common_dates) %>%
  select(Date, returns) %>%
  mutate(
    # Zastąp NA zerami dla obliczeń kumulatywnych
    returns = ifelse(is.na(returns), 0, returns),
    # Oblicz kapitał od nowa
    capital = 1000 * cumprod(1 + returns)
  )

# Momentum 12-1
df_M12_test <- Ethereum %>%
  filter(Date %in% common_dates) %>%
  select(Date, momentum_return) %>%
  mutate(
    momentum_return = ifelse(is.na(momentum_return), 0, momentum_return),
    capital = 1000 * cumprod(1 + momentum_return)
  )

# Mean Reversion już jest w df_MR
# Volatility Targeting już jest w df_VT

# Ramka porównawcza
df_comparison <- data.frame(
  Date = common_dates,
  BH = df_BH_test$capital,
  M12 = df_M12_test$capital,
  MR = df_MR$capital,
  VT = df_VT$capital
)

# Long format
library(tidyr)

df_plot <- df_comparison %>%
  pivot_longer(cols = -Date, names_to = "Strategia", values_to = "Kapital")

# Wykres
ggplot(df_plot, aes(x = Date, y = Kapital, color = Strategia)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Porównanie skumulowanych stóp zwrotu (OOS)",
    subtitle = "Start = 1000 USD na początku okresu testowego",
    x = "Data",
    y = "Kapitał ($)",
    color = "Strategia"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


###################### TEST T-STUDENTA (OOS) ########################

# Ramka ze stopami zwrotu
returns_comp <- data.frame(
  Date = common_dates,
  BH = df_BH$returns,
  M12 = df_M12$momentum_return,
  MR = df_MR$strategy_return,
  VT = df_VT$strategy_return
)

# Usuwamy ewentualne wiersze z NA, aby test t mógł zostać wykonan
returns_comp <- na.omit(returns_comp)

# Funkcja do przeprowadzania testu t dla pary strategii
# Hipoteza zerowa H0: Średni zwrot Strategii A = Średni zwrot Strategii B
# Hipoteza alternatywna H1: Średnie zwroty są istotnie różne
run_t_test <- function(s1, s2, label1, label2) {
  # Usuń NA tylko dla tej pary zmiennych
  valid_idx <- complete.cases(s1, s2)
  if(sum(valid_idx) < 2) {
    return(data.frame(
      Porównanie = paste(label1, "vs", label2),
      t_stat = NA,
      p_value = NA,
      Istotność = "Za mało danych"
    ))
  }
  
  test <- t.test(s1[valid_idx], s2[valid_idx], paired = TRUE)
  data.frame(
    Porównanie = paste(label1, "vs", label2),
    t_stat = round(test$statistic, 4),
    p_value = round(test$p.value, 4),
    Istotność = ifelse(test$p.value < 0.05, "Tak (p<0.05)", "Nie")
  )
}
# Wykonanie testów 
t_tests_results <- rbind(
  run_t_test(returns_comp$BH, returns_comp$M12, "Buy&Hold", "Momentum"),
  run_t_test(returns_comp$BH, returns_comp$MR, "Buy&Hold", "Mean Reversion"),
  run_t_test(returns_comp$BH, returns_comp$VT, "Buy&Hold", "Vol Targeting"),
  run_t_test(returns_comp$M12, returns_comp$MR, "Momentum", "Mean Reversion"),
  run_t_test(returns_comp$M12, returns_comp$VT, "Momentum", "Vol Targeting"),
  run_t_test(returns_comp$MR, returns_comp$VT, "Mean Reversion", "Vol Targeting")
)

# Wyświetlenie wyników testów
print("--- Wyniki Testu t-Studenta (OOS) ---")
t_tests_results


# Wizualizacja rozkładu stóp zwrotu (Boxplot)
returns_long <- returns_comp %>%
  pivot_longer(cols = -Date,
               names_to = "Strategia",
               values_to = "Daily_Return")

ggplot(returns_long,
       aes(x = Strategia, y = Daily_Return, fill = Strategia)) +
  geom_boxplot(alpha = 0.7) +
  coord_cartesian(ylim = c(-0.03, 0.03)) +
  labs(
    title = "Rozkład dziennych stóp zwrotu strategii",
    subtitle = "Okres Out-of-Sample",
    y = "Dzienny zwrot"
  ) +
  theme_minimal()

