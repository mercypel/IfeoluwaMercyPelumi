import pandas as pd #handles tables
import glob #finds files matching a pattern
import os #handles folders and paths

# 1. Loading the feature mapping file/ reading the raw sparse format
mapping_path = "feature_name_to_number_mapping.csv"
mapping = pd.read_csv(mapping_path) #loads the CSV file into a table

# Turning the feature number paired wih the feature name into a dictionary to rename the column easily
mapping_dict = dict(zip(mapping["feature_number"], mapping["feature_name"]))

## Building a clean table with one row per application
# 2. Parse each row because the data is not in a normal tabular data
#Turns the data into a spreadsheet
def parse_row(line):
    parts = line.strip().split()
    risk_score = float(parts[0])
    features = {}

    for item in parts[1:]:
        num, val = item.split(":")
        features[int(num)] = float(val)

    return risk_score, features

# 3. Loading raw monthly files
raw_files = glob.glob("project2_raw_data/*")

all_rows = [] #creating an empty list

for file in raw_files:
    with open(file, "r") as f: #opens the file so it can be read line by line
        for line in f: #for each line in the file (it represent one app)
            risk, feats = parse_row(line) #returns the risk score and dictionary of feature number: value pairs
            feats["risk_score"] = risk #adds risk score into the dictionary
            all_rows.append(feats) #adds dictionary to the bucket

# 4. Converting to DataFrame
df = pd.DataFrame(all_rows) #puts it in dataframe to get a clean table for modelling

# 5. Renaming feature columns
df = df.rename(columns=mapping_dict) #all features number are replaced with feature names because a model needs reasonable features names

# 6. Creating binary target label
df["risk_indicator"] = (df["risk_score"] >= 0.30).astype(int)# If risk_score is 0.30 or higher, label = 1 else 0

# 7. Saving processed dataset
os.makedirs("processed", exist_ok=True) #Create a folder called processed
output_path = "processed/processed_data.csv"
df.to_csv(output_path, index=False)

print("Data preparation complete.")
print(f"Processed file saved to: {output_path}")
print(df.head())
