import csv
import pandas as pd 


# Input file
input_file = "combined.csv"
output_file = "deduplicated_output_file.csv"

df=pd.read_csv(input_file)
df['method']=df['Unnamed: 0'].str.split("(").str[0]
df['pretreatment']=df['Unnamed: 0'].str.split("(").str[1].str[:-1]
df.drop(['Unnamed: 0','Enose'],axis=1,inplace=True)
print(df.pivot(index='method', columns='pretreatment', values='ML_Means'))
df.pivot(index='method', columns='pretreatment', values='ML_Means').to_csv("all_results_class.csv")
