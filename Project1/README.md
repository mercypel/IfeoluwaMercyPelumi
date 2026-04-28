# Sales & Weather Analysis

![R](https://img.shields.io/badge/Language-R-blue)
![Database](https://img.shields.io/badge/Database-SQLite-lightgrey)


---

## Project Overview

This project investigates how **weather conditions impact retail product sales** using SQL and R.

The analysis focuses on the **top 3 best-selling products**, combining sales and weather data to:

* Identify patterns in customer behaviour
* Quantify weather effects (temperature, rainfall)
* Build predictive models for demand forecasting

---

## Objectives

* Analyse the relationship between weather and sales
* Compare **Linear Regression** vs **Decision Tree models**
* Evaluate performance using **RMSE and R²**
* Provide **Business recommendations**

---

## Repository Structure

```bash
.
├── Project 1 file.R          # Main analysis script
├── project1_raw_data.db      # SQLite database
├── plots/                   # Generated visualisations
├── README.md                # Project documentation
```

---

## 🗄️ Dataset

The project uses a SQLite database with the following tables:

| Table     | Description                   |
| --------- | ----------------------------- |
| `train`   | Daily sales data              |
| `weather` | Weather conditions            |
| `key`     | Store–weather station mapping |
| `station` | Weather station metadata      |

---

## Methodology

### 🔹 Data Processing

* SQL joins to combine sales and weather data
* Date formatting and data type correction
* Missing value handling:

  * Median imputation (by station)
  * Snowfall → replaced with 0
* Outlier handling using **winsorisation (99.5th percentile)**

---

### 🔹 Feature Engineering

* `is_weekend` → Weekend indicator
* `is_rainy` → Rain indicator

---

### 🔹 Modelling

* **Linear Regression**
* **Decision Tree (rpart)**

Data split:

* 60% Training
* 20% Test
* 20% Validation

---

## 📈 Results

### 🔹 Model Performance (Validation RMSE)

| Product   | Linear Regression | Decision Tree | Best Model        |
| --------- | ----------------- | ------------- | ----------------- |
| Product 1 | 27.46             | 27.51         | Linear Regression |
| Product 2 | 24.52             | 24.58         | Linear Regression |
| Product 3 | 24.05             | 24.12         | Linear Regression |

📌 Key Findings:

* Linear regression consistently performs **slightly better**
* Differences between models are **minimal**
* **Low R² values** → Weather explains only a small portion of sales
* **Weekend effect** is the strongest predictor

---

## 📊 Visualisations

The script automatically generates:

* Temperature vs Sales scatter plots
* Rain vs No Rain boxplots
* Weekend vs Weekday comparisons
* Sales over time trends
* Decision tree diagrams

📁 Saved in the `/plots` folder

---

## ▶️ How to Run

### 1️⃣ Install Dependencies

```r
install.packages(c("DBI", "RSQLite", "rpart", "rpart.plot"))
```

---

### 2️⃣ Set Working Directory

```r
setwd("your/project/folder")
```

---

### 3️⃣ Run the Script

```r
source("Project 1 file.R")
```

---

### 4️⃣ Outputs

* Model metrics printed in console
* Plots saved in `/plots`

---

## 💡 Key Insights
* Product 1 performs better on cooler, dry days
* Product 2 shows little to no sensitivity to weather
* Product 3 is slightly affected by rain
* Weekends consistently increase sales across all products

📄 For detailed analysis, model interpretation, and full recommendations, see the full report included in this repository.
---




