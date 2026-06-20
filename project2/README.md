# **Risk Classification Model**

This project builds a machine‑learning model that classifies software applications as either **high‑risk** or **low‑risk** based on their behavioural features. The raw data was supplied in a sparse format, so a key part of the work involved transforming it into a usable dataset, training a reliable model, and packaging everything so another developer can run predictions without difficulty.

---

## **📁 Project Structure**

```
project2/
│
├── data_preparation.py          # Converts sparse raw files into a clean dataset
├── model_training.py            # Trains the XGBoost model and saves outputs
├── model_testing.py             # Evaluates the model on the full dataset
├── deployment_test.py           # Loads the packaged model and runs predictions
│
├── processed/
│   └── processed_data.csv       # Clean dataset used for modelling
│
├── model/
│   ├── xgboost_model.pkl        # Trained model saved using pickle
│   └── feature_columns.pkl      # List of feature names used during training
│
├── plots/
│   └── feature_importance_top15.png   # Top 15 feature‑importance plot
│
└── documentation.pdf            # Full project documentation
```

---

## **📌 Project Overview**

The goal of this project is to classify software applications into two categories:

- **0 — Low Risk**  
- **1 — High Risk**

The raw data was provided in a sparse format, where each row contained a risk score followed by only the features triggered by that application. The project involved:

- parsing and expanding the sparse data  
- renaming feature numbers using a mapping file  
- creating a binary risk indicator  
- removing duplicate rows before splitting  
- training an XGBoost classifier  
- evaluating performance on unseen data  
- generating a feature‑importance plot  
- packaging the model and feature list for deployment  

The final model meets the requirement of achieving **over 90% precision and recall** on the validation split.

---

## **🧹 Data Preparation**

Each raw row was parsed into a dictionary and expanded into a full table using pandas. Any feature not present in a row naturally becomes a missing value (`NaN`). This is intentional — XGBoost handles missing values internally, so no manual zero‑filling is required.

A mapping file was used to replace feature numbers with meaningful names, and a binary label (`risk_indicator`) was created by marking risk scores below 0.30 as low risk and scores at or above 0.30 as high risk.

The final processed dataset was saved as:

```
processed/processed_data.csv
```

---

## **🤖 Model Training**

An XGBoost classifier was chosen because it performs well on high‑dimensional, sparse datasets and handles missing values natively.

Before splitting the data, duplicate rows were removed to ensure that the validation set contained only unseen applications. The dataset was then split into an 80/20 train–validation split using stratification.

The model achieved strong results on the validation set, with both precision and recall above the required 90% threshold. These values reflect genuine performance because the validation set contains only unseen data.

A feature‑importance plot showing the **Top 15 most influential features** was generated and saved to the `plots` folder.

---

## **📦 Packaged Model**

The trained model is saved as:

```
model/xgboost_model.pkl
```

The exact list of feature columns used during training is saved separately as:

```
model/feature_columns.pkl
```

This ensures that any deployment environment can recreate the correct input structure. The model expects:

- all feature columns in the same order  
- missing or unavailable features left as `NaN`  
- no `risk_score` or `risk_indicator` columns  

The model returns:

- `1` → High risk  
- `0` → Low risk  

---

## **🧪 Deployment Test**

The file `deployment_test.py` demonstrates how to:

- load the model using `pickle`  
- load the feature column list  
- prepare input rows correctly  
- generate predictions  
- compare predicted vs actual labels  

This confirms that the packaged model works as expected outside the training environment.

---

## **▶️ How to Run the Project**

### **1. Prepare the data**
```
data_preparation.py
```

### **2. Train the model**
```
model_training.py
```

### **3. Evaluate the model**
```
model_testing.py
```

### **4. Run sample predictions**
```
sample_test_script.py
```

---

## **📄 Requirements**

Install dependencies:

```
pip install -r requirements.txt
```

---


