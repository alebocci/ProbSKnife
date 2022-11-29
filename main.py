import subprocess
from parse import *
import pandas as pd
import sys
import os


#Getting starting partitioning from arguments
Pstart = sys.argv[1]

#Building query string to write in file
queryString = 'query(futureCost('+Pstart+',smallExample,[charact1,charact2,charact3,charact4,data2,data4,data5],3,RES)).'

#Write query string in file query.pl
with open('query.pl', 'w') as f:
    f.write(queryString)

#Running problog program with query
result = subprocess.run(['python', '-m','problog','mainPr.pl','query.pl','--combine'], stdout=subprocess.PIPE)

os.remove('query.pl')

################# Parse Output phase ####################
format_string = '{},{:d},([{}], {}, {:d})): {:f} '

s=str(result.stdout,'utf-8')
s=s.replace('\t', ' ')
lines = s.splitlines()

labs = []
parts = []
costs = []
probs =[]
for line in lines :
    parsed = parse(format_string, line)
    labs.append(parsed[2])
    parts.append(parsed[3])
    costs.append(parsed[4])
    probs.append(parsed[5])

####### Bulding pandas datafame from parsed result


df = pd.DataFrame()
df['labellings']=labs
df['partitionings']=parts
df['costs']=costs
df['probabilities']=probs

minGroup = df.groupby(['labellings']).costs.min()
minGroup = minGroup.reset_index()
keys = list(minGroup.columns.values)
i1 = df.set_index(keys).index
i2 = minGroup.set_index(keys).index
minFilter = df[i1.isin(i2)] #filter out every row without min cost per labelling

sumProb = minFilter.groupby(['partitionings','costs']).probabilities.sum()
sumProb = sumProb.reset_index()


sumProb['expectedCost'] = sumProb['costs'] * sumProb['probabilities']
expectedCost = sumProb['expectedCost'].sum()
del sumProb['expectedCost']

####### Print result
print('Starting from the partitionig '+Pstart+' all the reachable partitionings with cost and probability are:')
print(sumProb.to_string())
print('\nThe expected cost is '+str(expectedCost))





