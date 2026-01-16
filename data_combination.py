import os
import pandas as pd
from io import StringIO

BASE_DIR = r"C:\Users\85251\Documents\Pipe_File"

def read_data(i: int) -> pd.DataFrame: #return dataframe for each output
    path = os.path.join(BASE_DIR, f"{i}.data")

    with open(path, "r", encoding="utf-8") as f:
        vheader = f.readline().strip() #first line (removed whitespace)
        body = f.read() #remaining lines (read start from where the pointer is)

    version = int(vheader.split("=", 1)[1]) #get the version (first 1 mean do exactly 1 split)

    lines = body.splitlines() #split the lines of body
    # guard against empty / malformed files
    if len(lines) < 2:
        raise ValueError(f"File {path} has no data lines") #len = 1 means only has column names

    df = pd.read_csv(StringIO(body), sep="|") #read_csv expects a file object, where StringIO can help

    # ensure only first 100 rows if there is an -END- etc.
    df = df.iloc[:100].copy()

    # add model, version 
    df["version"] = version
    df["model"] = i

    cols = ["model", "version"] + [c for c in df.columns if c not in ("model", "version")]
    df = df[cols] #reorder the column
    
    return df
dfs = []

for i in range(1, 11):
    dfs.append(read_data(i)) #add a df below

df_all = pd.concat(dfs, axis=0, ignore_index=True) #concat one time only is more efficient

df_all.to_csv(
    os.path.join(BASE_DIR, "output.csv"),
    index=False,
    encoding="utf-8",
)

