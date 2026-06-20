import pandas as pd
from sklearn.metrics import precision_score, recall_score, classification_report, confusion_matrix
import pickle

# 1. Loading the already saved model
with open("model/xgboost_model.pkl", "rb") as f:
    model = pickle.load(f)

# 2. Loading the feature column list
with open("model/feature_columns.pkl", "rb") as f:
    feature_cols = pickle.load(f)

# 3. Loading the existing processed dataset
df = pd.read_csv("processed/processed_data.csv")

# 4. Prepare features using the saved feature list
X = df[feature_cols]
y = df["risk_indicator"]

# 5. Make predictions
y_pred = model.predict(X)

# 6. Evaluate performance
precision = precision_score(y, y_pred)
recall = recall_score(y, y_pred)
report = classification_report(y, y_pred)
cm = confusion_matrix(y, y_pred)

print("\nMODEL TESTING RESULTS (Full Dataset)")
print("-------------------------------------")
print(f"Precision: {precision:.3f}")
print(f"Recall:    {recall:.3f}")
print("\nClassification Report:")
print(report)

print("Confusion Matrix:")
print(cm)