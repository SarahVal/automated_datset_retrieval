from func_dataset_retrieval import retrieve_semantic

queries = {10: 'Québec AND density AND species'}
offset = 0
limit = 50
year_min = 1980
year_max = 2022

df_semantic = retrieve_semantic(queries, offset = offset, limit = limit, year_min = year_min, year_max = year_max)

