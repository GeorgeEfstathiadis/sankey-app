import os 
import pandas as pd
import random


ids = ['m8PDtlLB4h',
       'lnxbM8b6z6',
       'nbFk9o5oLS',
        '3vyLLz5nZh',
        'FxbA5YpEHk',
        'R3RIn8m3DK',
        'uMoYIqoOYe',
        'IljqftXu0f',
        'JMzRy5f52y',
        'L0DdF6SZOG']

ids = [[i] * 3 for i in ids]
ids = [i for s in ids for i in s]

pathnos = [n for n in range(1,4)] * 10

d = {1: 'day1 - day2', 2: 'day2 - day3', 3: 'day3 - day4'}
pathname = [d[n] for n in pathnos]

f1 = [random.choice(['', 'Y']) for i in range(10)]
f1 = [[x]*3 for x in f1]
f1 = [i for s in f1 for i in s]

f2 = [random.randint(0, 100) for i in range(30)]

treatments = ['T1', 'T2', 'NoTrt']
start = [random.choice(treatments) for i in range(30)] 
end = start[1:]
end.append(random.choice(treatments))
end[2::3] = [random.choice(treatments) for i in range(10)] 

data = pd.DataFrame({'USUBJID':ids, 'NODE_S':start, 'NODE_E':end, 'PATHNO':pathnos, 'PATHNAME':pathname, 'FILTER_Y':f1, 'FILTER_2':f2})
data.to_csv('sankey_dummy_data.csv',index=False)
