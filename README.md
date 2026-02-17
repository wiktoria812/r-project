# Analiza Strategii Inwestycyjnych: S&P 500 oraz Ethereum

### Cel projektu

Projekt został zrealizowany w ramach ćwiczeń z przedmiotu Podstawy programowania oraz analiza danych w R. Głównym celem było porównanie efektywności różnych strategii inwestycyjnych na wybranych klasach aktywów (S&P500, FX(EUR/USD), Złoto oraz Ethereum). Niniejsze repozytorium zawiera moją indywidualną część pracy skupioną na:
* S&P 500 (reprezentującym tradycyjny rynek akcji),
* Ethereum (reprezentującym dynamiczny rynek kryptowalut).

Analiza miała na celu sprawdzenie, czy proste strategie techniczne (np. przecięcia średnich) pozwalają na osiągnięcie lepszych wyników niż pasywne podejście Buy & Hold.

### Struktura i opis kodu

Kod w języku R został podzielony na logiczne sekcje, które umożliwiają pełny proces analityczny.\
Pobieranie i czyszczenie danych: Wykorzystanie bibliotek (quantmod) do pobrania historycznych notowań S&P 500 oraz ETH z publicznych baz danych (Kaggle).

### Implementacja strategii

* Dla obu klas aktywów zostały wykorzystane strategie Buy & Hold (Benchmark służący do porównania wyników) oraz Momentum 12-1.
* Pozostałe stretegie dla S&P 500: Przecięcie Średnich Kroczących (Szybka = 1 miesiąc, Wolna = 3 miesiące), Mini Risk Parity (Aktywa = 2, Okno kowariancji = 4 miesiące).
* Pozostałe strategie dla Ethereum: Mean Reversion (Lookback = 2 dni), Volatility Targeting (Cel = 10%, Okno = 60 dni).
* Obliczenia finansowe: Wykorzystanie pętli i funkcji do obliczenia dziennych stóp zwrotu, skumulowanego zysku oraz miar ryzyka (np. obsunięcia kapitału).
* Wizualizacja: Wygenerowanie wykresów porównawczych stóp zwrotu oraz zmienności obu aktywów za pomocą ggplot2 lub bazowych funkcji graficznych R.

### Wyniki i wnioski

Szczegółowe wyniki analizy oraz graficzne przedstawienie danych znajdują się w dołączonych do repozytorium plikach PDF. 

### Kluczowe wnioski z mojej części projektu

* S&P 500: Wykazuje stabilny wzrost w długim terminie przy znacznie niższej zmienności niż rynek krypto. Strategie oparte na średnich kroczących pomagają ograniczyć ryzyko podczas dużych spadków rynkowych.
* Ethereum: Charakteryzuje się ekstremalnie wysokimi stopami zwrotu, ale również bardzo wysokim ryzykiem (Drawdown). W przypadku tego aktywa, odpowiednio dobrana strategia aktywna potrafiła znacząco poprawić wynik w stosunku do pasywnego trzymania jednostek podczas bessy.

Analiza pokazała wyraźną różnicę w profilu ryzyka do zysku między aktywami tradycyjnymi a cyfrowymi, co jest kluczowe przy budowie zdywersyfikowanego portfela.

### O autorce
Część projektu dotycząca S&P 500 oraz Ethereum została przygotowana przez Wiktorię (kod, analiza danych oraz opracowanie wniosków). 

### Uwagi
Projekt ma charakter edukacyjny i nie stanowi porady inwestycyjnej.
