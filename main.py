from utils import *
from datetime import datetime

#Arguments check
(appId,Dlimit,K,tables,labellingsP,timestamp) = checkArgs()

if(timestamp):
    print('Starting execution.')
    print('Retrieving application information.')
start = datetime.now()

(StartingLabelling,Dlimit,K)=checkApp(appId,K,Dlimit)

if(timestamp):
    print('Starting to create labelling probabilities at:'+str(datetime.now()-start)+'\n')
(labellings,Already) = queryLabellings(appId,StartingLabelling,K)

if(timestamp):
    if(not Already):
        print('Labelling created at: '+str(datetime.now()-start)+'\n')
        print('Search for all eligible partitionings.')
    else:
        print('Labelling created before.\n')
        print('Search for all eligible partitionings.')


partitionings = queryAllPartitionings(appId,StartingLabelling,Dlimit)

if(timestamp):
    print(str(len(partitionings))+' partitionings determined at: '+str(datetime.now()-start)+'\n')
    print('Determining cost for every partitioning.')

for p in partitionings:
    labs = []
    parts = []
    costs = []
    probs =[]
    for labelling, prob in labellings:
        
        fcs = queryFutureCost(appId,p,labelling,Dlimit)
        if(fcs is None):
            continue
        for (part,cost) in fcs:
            
            labs.append(labelling)
            if(labellingsP):
                parts.append(part)
            else:
                parts.append(Partitioning(part))
            costs.append(cost)
            probs.append(prob)
    
    (sumProb,expectedCost,impossible)=buildResults(labs,parts,costs,probs)
    printResults(p,sumProb,expectedCost,impossible,tables,labellingsP,timestamp,start)


