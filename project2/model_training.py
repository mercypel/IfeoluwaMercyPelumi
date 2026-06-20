import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split  # divides the data into training and testing parts
from sklearn.metrics import precision_score, recall_score, classification_report  # measure model performance
from xgboost import XGBClassifier  # machine learning model
import matplotlib.pyplot as plt
import os  # create folders and work with file paths
import pickle #to save and load objects

# 1. Loading the processed dataset
df = pd.read_csv("processed/processed_data.csv")

# 2. Remove duplicate rows to avoid leakage across train/validation
df = df.drop_duplicates()

# 3. Separating features and target
X = df.drop(columns=["risk_indicator", "risk_score"])  # drop target and risk_score to avoid leakage
y = df["risk_indicator"]  # the correct labels the model must learn

# 4. Save feature column list for deployment
os.makedirs("model", exist_ok=True)
with open("model/feature_columns.pkl", "wb") as f:
    pickle.dump(list(X.columns), f)

# 5. Training and validation split (80% train, 20% test)
X_train, X_val, y_train, y_val = train_test_split(
    X, y, test_size=0.2, random_state=42, stratify=y
)

# 6. Building XGBoost classifier model
model = XGBClassifier(
    n_estimators=300,       # builds 300 decision trees
    max_depth=6,           # each tree can go 6 levels deep
    learning_rate=0.05,    # learn slowly and carefully
    subsample=0.9,         # use 90% of the data for each tree
    colsample_bytree=0.9,  # use 90% of the features for each tree
    eval_metric="logloss", # measure how well the model fits
    random_state=42        # make results repeatable
)

# 7. Training the model
model.fit(X_train, y_train)

# 8. Predicting on the validation data
y_pred = model.predict(X_val)

# 9. Evaluating the performance of the model
precision = precision_score(y_val, y_pred)
recall = recall_score(y_val, y_pred)

print("\nMODEL PERFORMANCE")
print("------------------")
print(f"Precision: {precision:.3f}")
print(f"Recall:    {recall:.3f}")
print("\nClassification Report:")
print(classification_report(y_val, y_pred))

# 10. Save the trained model using pickle (required by brief)
with open("model/xgboost_model.pkl", "wb") as f:
    pickle.dump(model, f)

print("\nModel saved to: model/xgboost_model.pkl")

# 9. Top 15 Feature Importance Plot

# Get feature importances as a Series for easy sorting
importances = pd.Series(model.feature_importances_, index=X.columns)

# Selecting top 15
top15 = importances.sort_values(ascending=False).head(15)

# Plot
plt.figure(figsize=(12, 8))
top15.sort_values().plot(kind="barh")
plt.xlabel("Importance Score")
plt.ylabel("Top 15 Features")
plt.title("XGBoost Feature Importance (Top 15)")
plt.tight_layout()

#saving plot
os.makedirs("plots", exist_ok=True)
plt.savefig("plots/feature_importance_top15.png")
plt.close()

print("Top 15 feature importance plot saved to: plots/feature_importance_top15.png")