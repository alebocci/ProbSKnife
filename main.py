import subprocess
from parse import *
import pandas as pd
import sys
import os


#####Class partitioning
class Partitioning:
    def __init__(self, pstring):
        self.domains = []
        for domains in findall('[{}]', pstring):
            for d in domains:
                d=d.replace(',',' ')
                sws = d.split()
                sws.sort()
                self.domains.append(sws)

    def __str__(self):
        resString =str(self.domains)
        resString = resString.replace('\'','')    
        return resString

    def __eq__(self, other):
        if isinstance(other, Partitioning):
            if(not len(self.domains)==len(other.domains)):
                return False
            for d in self.domains:
                if(not d in other.domains):
                    return False
            return True
        return False

    def __ne__(self, obj):
        return not self == obj
    
    def __lt__(self, other):
        return len(self.domains) < len(other.domains)
    
    def __hash__(self):
        domTup = ()
        for d in self.domains:
            domTup +=tuple(d)
        return hash(domTup)
######################

#Arguments check
usage = 'Usage: "main.py PARTITIONING DLIMIT -l[optional]"\nPARTITIONING\tstarting partitioning\nDLIMIT\t\tan integer\n-l\t\tdifferentiate partitioning by labels'

if(len(sys.argv)<3):
    print(usage)
    exit()

#Starting partitioning argument
Pstart = sys.argv[1]
p0 = Partitioning(Pstart)
if(p0.domains==[]):
    print(usage)
    exit()
#Domain limit argument
Dlimit = sys.argv[2]
if(not Dlimit.isdigit()):
    print(usage)
    exit()

#Labelling on partitioning argument
labellingsP = False
if(len(sys.argv)==4):
    if(sys.argv[3]=='-l'):
        labellingsP = True
    else:
        print(usage)
        exit()

#Query string to write in file
queryString = 'query(futureCost('+Pstart+',smallExample,'+Dlimit+',RES)).'

#Write query string in file query.pl
with open('query.pl', 'w') as f:
    f.write(queryString)

#Running problog program with query
output = subprocess.run(['python', '-m','problog','mainPr.pl','query.pl','--combine'], stdout=subprocess.PIPE)

#removing the query file
os.remove('query.pl')

################# Parsing Output phase ####################
format_string_output = '{},{:d},([{}],[{}],{:d})):{:f}'

s=str(output.stdout,'utf-8')
s=s.replace('\t', '')
s=s.replace(' ', '')
lines = s.splitlines()

labs = []
parts = []
costs = []
probs =[]
for line in lines :
    parsed = parse(format_string_output, line)
    if(parsed is None):
        #parsing problem, probably output error from problog
        print('Something went wrong, check the Problog model.')
        print(s)
        exit()
    labs.append(parsed[2])
    #if -l is active partitionings are strings, else are Partitioning object for group by equal ones
    if(labellingsP):
        parts.append(parsed[3])
    else:
        parts.append(Partitioning(parsed[3]))
    costs.append(parsed[4])
    probs.append(parsed[5])

####### Bulding pandas datafame from parsed result
df = pd.DataFrame()
df['labellings']=labs
df['partitionings']=parts
df['costs']=costs
df['probabilities']=probs

#groping by labelling to find minimum partitionings
minGroup = df.groupby(['labellings']).costs.min()
minGroup = minGroup.reset_index()

#filter out every row without min cost per labelling
keys = list(minGroup.columns.values)
i1 = df.set_index(keys).index
i2 = minGroup.set_index(keys).index
minFilter = df[i1.isin(i2)]

#grouping by partitioning for the final table
sumProb = minFilter.groupby(['partitionings','costs']).probabilities.sum()
sumProb = sumProb.reset_index()

#calculating expected cost considering only the minimum cost per labelling
exp = minFilter.drop_duplicates(subset=['labellings','costs','probabilities']).reset_index()
expectedCost = (exp['costs'] * exp['probabilities']).sum()
#if limit is two low there are not satisfiable labellings, this is their aggregate probability
impossible = exp['probabilities'].sum()

####### Print results
print('Starting from the partitionig '+Pstart+' all the reachable partitionings with cost and probability are:')
print(sumProb.to_string())
print('\nThe expected cost is '+str(expectedCost))
print('The probability to have a labelling not satisfiable with the set limit is '+str(1-impossible))
