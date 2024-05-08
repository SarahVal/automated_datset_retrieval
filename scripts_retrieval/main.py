from func_dataset_retrieval import retrieve_semantic, create_file
import time




queries = {10: 'Québec AND density AND species'}
offset = 0
limit = 50
year_min = 1980
year_max = 2022

df_semantic, errors = retrieve_semantic(queries, offset = offset, limit = limit, year_min = year_min, year_max = year_max)

sufix =  time.time()
name = f'semantic_scholar_9_{sufix}.csv'
df_semantic.to_csv(name, sep=",", header=True, index=False)

errorsFilename = f'errors_{sufix}.json'
create_file(errorsFilename, errors)





