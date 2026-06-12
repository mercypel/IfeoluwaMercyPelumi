import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split  # divides the data into training and testing parts
from sklearn.metrics import precision_score, recall_score, classification_report  # measure model performance
from xgboost import XGBClassifier  # machine learning model
import matplotlib.pyplot as plt
import joblib  # save the trained model to a file
import os  # create folders and work with file paths

# 1. Load the processed dataset
df = pd.read_csv("processed/processed_data.csv")

# 2. Separating features and target
X = df.drop(columns=["risk_indicator", "risk_score"])  # drop target and risk_score to avoid leakage
y = df["risk_indicator"]  # the correct labels the model must learn

# 3. Training and validation split (80% train, 20% test)
X_train, X_val, y_train, y_val = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)

# 4. Building XGBoost classifier model
model = XGBClassifier(
    n_estimators=300,       # builds 300 decision trees
    max_depth=6,           # each tree can go 6 levels deep
    learning_rate=0.05,    # learn slowly and carefully
    subsample=0.9,         # use 90% of the data for each tree
    colsample_bytree=0.9,  # use 90% of the features for each tree
    eval_metric="logloss", # measure how well the model fits
    random_state=42        # make results repeatable
)

# 5. Training the model
model.fit(X_train, y_train)

# 6. Predicting on the validation data
y_pred = model.predict(X_val)

# 7. Evaluating the performance of the model
precision = precision_score(y_val, y_pred)
recall = recall_score(y_val, y_pred)

print("\nMODEL PERFORMANCE")
print("------------------")
print(f"Precision: {precision:.3f}")
print(f"Recall:    {recall:.3f}")
print("\nClassification Report:")
print(classification_report(y_val, y_pred))

# 8. Save the trained model
os.makedirs("model", exist_ok=True)
joblib.dump(model, "model/xgboost_model.pkl")
print("\nModel saved to: model/xgboost_model.pkl")
