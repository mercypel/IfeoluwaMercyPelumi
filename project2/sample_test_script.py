import pandas as pd
import pickle

# 1. Load the trained model
with open("model/xgboost_model.pkl", "rb") as f:
    model = pickle.load(f)

# 2. Load the feature column list
with open("model/feature_columns.pkl", "rb") as f:
    feature_cols = pickle.load(f)

# 3. Load the processed dataset
df = pd.read_csv("processed/processed_data.csv")

# 4. Selecting only 5 rows as sample
sample = df.sample(5, random_state=42)

# 5. Prepare features by dropping risk_score and risk_indicator
X_sample = sample[feature_cols]

# 6. Run predictions
predictions = model.predict(X_sample)

# 7. Combine sample + predictions
output = sample.copy()
output["predicted_risk_indicator"] = predictions

# 8. Print results
print("\nSAMPLE PREDICTIONS")
print("-------------------")
print(output[["risk_score", "risk_indicator", "predicted_risk_indicator"]])