# The Impact of Migration on Housing Prices in Russia (2021–2023)

This econometric research quantifies the effect of internal and external migration flows on residential real estate prices across Russian regions, covering both primary and secondary markets. Special thanks to my co-authors (Baigildin Arthur, Popov Platon, Dashevsky Daniil) for their contributions to this project.

## 📊 Project Overview
The study utilizes panel data from 80+ Russian regions for the period 2021–2023. The primary objective is to estimate price elasticity in relation to net migration rates while controlling for regional economic indicators.

## 📂 Repository Structure
* `Migration and housing prices.R` – Core R script containing data cleaning, hypothesis testing, and model estimation.
* `Datasets/` – Processed datasets sourced from Rosstat (Federal State Statistics Service), including price indices and migration metrics.
* `Project_description.pdf` – **Full research paper** containing detailed methodology, visual data analysis, and conclusions.

## 🛠 Methodology
* **Models:** Panel Data Regression (Fixed Effects and Random Effects models).
* **Tools:** R (utilizing `plm` for panel linear models, `ggplot2` for visualization, and `stargazer` for regression tables).
* **Control Variables:** GRP per capita, housing completion rates (supply-side), and average disposable income.

## 📈 Key Findings
> 1. A 1% increase in net migration inflow has a statistically significant positive impact on secondary market prices.
> 2. The effect of migration is more pronounced in regions with restricted housing supply.
> 3. Regional income levels remain the strongest predictor, though migration acts as a significant catalyst in metropolitan areas.

---
## Getting Started
To view the comprehensive analysis, including charts and regression tables, please refer to the [**Project_description.pdf**](./Project_description.pdf).
