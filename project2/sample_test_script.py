import pandas as pd
import joblib

# 1. Loading the model
model = joblib.load("model/xgboost_model.pkl")

# 2. Loading the processed data set
df = pd.read_csv("processed/processed_data.csv")

# 3. Selecting only 5 rows as sample
sample = df.sample(5, random_state=42)

# 4. Preparing features (risk_indicator + risk_score)
X_sample = sample.drop(columns=["risk_indicator", "risk_score"])

# 5. Runing the predictions
predictions = model.predict(X_sample)

# 6. Combine sample + predictions to check correctness
output = sample.copy() #creates a seperate independent copy of same dataframe
output["predicted_risk_indicator"] = predictions #adds the prediction to it

# 7. Print results
print("\nSAMPLE PREDICTIONS")
print("-------------------")
print(output[["risk_score", "risk_indicator", "predicted_risk_indicator"]])