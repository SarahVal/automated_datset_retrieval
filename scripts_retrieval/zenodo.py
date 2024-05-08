from func_dataset_retrieval import retrieve_zenodo, create_file
import time




queries = {10: 'Québec AND density AND species'}
offset = 0
limit = 50
year_min = 1980
year_max = 2022

df_zenodo = retrieve_zenodo(queries)

sufix =  time.time()
name = f'zenodo_{sufix}.csv'
df_zenodo.to_csv(name, sep=",", header=True, index=False)

errorsFilename = f'errors_{sufix}.json'
#create_file(errorsFilename, errors)





