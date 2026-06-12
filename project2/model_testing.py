import pandas as pd
from sklearn.metrics import precision_score, recall_score, classification_report, confusion_matrix
import joblib

# 1. Loading the already saved model
model = joblib.load("model/xgboost_model.pkl")

# 2. Loading the existing processed dataset
df = pd.read_csv("processed/processed_data.csv")

# 3. Prepare features (also dropping the risk_indicator + risk_score)
X = df.drop(columns=["risk_indicator", "risk_score"])
y = df["risk_indicator"]

# 4. Make predictions
y_pred = model.predict(X)

# 5. Evaluating the model performance
prediction_correctness = precision_score(y, y_pred)
recall = recall_score(y, y_pred)
report = classification_report(y, y_pred)
cm = confusion_matrix(y, y_pred)

print("\nMODEL TESTING RESULTS")
print("----------------------")
print(f"Precision: {prediction_correctness:.3f}")
print(f"Recall:    {recall:.3f}")
print("\nClassification Report:")
print(report)

print("Confusion Matrix:")
print(cm)