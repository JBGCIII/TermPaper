# README

**VARIABLES**

The data set is created using APIs and Data donwloaded from CEPEA.

[Financial Variables]
PTAX: Exchange Rate USD/REAL downloaded via rbcb (Brazilian Central Bank API)

Prica Arabica:Per 60-kilo bag, type 6, delivered in São Paulo (capital). Term prices converted into cash prices by discounting the NPR tax (CEPEA)

Price Robusta: Per 60-kilo bag, type 6, in Espírito Santo state. Term prices converted into cash prices by discounting the NPR taX (CEPEA)

Close_USD_60_kg: Arabica Coffee Future for Arabica (KC=F), downloaded via Yahoo finance API,  converted from cents/lb to USD/60-kilo bag (* 0.01: converts cents/lb to USD/lb, 1 kilogram ≈ 2.20462 lbs, 60 kg ≈ 132.277 lbs.)


[Weather Variables] (NASAPOWER)
lon -45.43 lat -21.55 Minas Gerais, Brazil, a major coffee-producing region, especially known for Arabica coffee.

Temp_Max: Daily Man temp in Minas Gerais

Temp_Min: Daily Min temp in Minas Gerais

Humidity: Daily humidity in Minas Gerais

Solar Radiation: Daily Solar Radiation in Minas Gerais

Precipation_MM: Daily Precipation in Minas Gerais


**SCRIPTS**

NOTE, Scripts must be run sequentially in order to work, those with the same number can be run indepedent from each other (i.e you can run Pre_Var_Analysis.R before
Pre_Var_Analysis_Graphical.R)

1.Data_Set_Creation.R -> Creates the Coffe_Data_Set.csv

2.Data_Processing.R -> Creates a processed data set with log and log returns.

3.Pre_Var_Analysis_Graphical.R -> Creates graphs for the graphical analysis of whether the variable are stationary or not.

3.Pre_Var_Analysis.R -> Conducts Augmented Dickey–Fuller test (ADF) test,Kwiatkowski–Phillips–Schmidt–Shin (KPSS) test, and Ljung-Box test.

4.VAR_Analysis -> Var analysis of Coffe Futures for Arabica, Price for Arabica, Price for Robusta, and excahnge rate between usd and real.



