# Solar Forecasting ARIMA

**Forecasting monthly solar PV generation using ARMA and seasonal ARIMA models, including simulation of seasonal processes and autocorrelation structure analysis.**

This project explores how autoregressive models capture both short-term dependencies and seasonal patterns in renewable energy generation. Through analytical and simulation tasks, it studies model stability, stationarity, and prediction performance for solar power data.

---

## 📘 Project Overview

The repository contains the work for *Assignment 2: ARMA Processes and Seasonal Processes*, completed as part of a time series analysis course.  
It includes theoretical exercises on AR(2) models, seasonal model simulations, and practical forecasting of solar power generation.

---

## 🧩 Repository Structure

| File | Description |
|------|--------------|
| `assignment2_2024.pdf` | Original assignment instructions and theoretical background. |
| `assignment2_ex1.R` | Stability and autocorrelation analysis of an AR(2) process. |
| `assignment2_ex2.R` | Seasonal AR model applied to monthly solar power forecasting and model validation. |
| `assignment2_ex3.R` | Simulation of ARMA and seasonal time series, with ACF and PACF visualization. |
| `datasolar.csv` | Monthly solar PV generation data (year, month, power). |
| `Rplots/` | Plots of time series, ACF, PACF, and forecast results. |
| `time-series-analysis-as2.Rproj` | RStudio project configuration. |

---

## ⚙️ Methods and Tools

- **Models:**  
  - AR(2) model — used to study stability, invertibility, and autocorrelation.  
  - Seasonal AR(1) model — applied to predict 12-month ahead solar generation.  
  - Simulated seasonal ARMA processes — analyzed through ACF/PACF structures.

- **Techniques:**  
  - Stationarity and invertibility checks  
  - Model validation via i.i.d. residual tests  
  - Forecasting and prediction interval estimation  
  - Simulation of stochastic time series  

- **Tools:**  
  R, ggplot2 (visualization), xtable (LaTeX tables), base R time series functions.

---

## 👥 Contributors

- [@s233239](https://github.com/s233239) (zoewr)  
- [@kongehund](https://github.com/kongehund)  
- [@fenfen22](https://github.com/fenfen22) (Fenfen)

---

## 📚 References

- DTU Course material: *Time Series Analysis* (2024).

