import pandas as pd
import numpy as np

# Input: dataframe with variable data for each discrete trait for use in GLM 
# Output: dataframe with variable pairs and correlation value 

filename = input("Enter tsv file:")
df = pd.read_csv(filename,sep='\t')

df2 = df.corr()
df2 = df2.stack().reset_index()
df2.columns = ['Var1','Var2','correlation']
print(df2)

df2.to_csv('out.csv')


