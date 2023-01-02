from utils import *
from datetime import datetime

#Arguments check
(appId,Dlimit,K,tables,labellingsP,timestamp) = checkArgs()

start = datetime.now()
if(timestamp):
    print('Starting execution.')
    print('Creating labelling probabilities.')
consultString=':- consult(\''+appId+'.pl\').'
queryLabellingsString = 'query(labellingK('+K+',L,D)).'
fileString = consultString+'\n'+queryLabellingsString

(labellingsPredicates,StartingLabelling) = queryLabellings(fileString,K)

if(timestamp):
    print('Labelling created at: '+str(datetime.now()-start)+'\n')
    print('Search for all eligible partitionings.')

queryAllPString = 'query(sKnife('+appId+','+StartingLabelling+','+Dlimit+',Pi)).'
fileString = consultString+'\n'+queryAllPString
partitionigs = queryAllPartitionings(appId,fileString,StartingLabelling,Dlimit)

if(timestamp):
    print('All partitioning determined at: '+str(datetime.now()-start)+'\n')
    print('Determining cost for every partitioning.')

for p in partitionigs:
    queryStringP = 'query(futureCost('+p+','+appId+','+Dlimit+',RES)).'
    fileString = consultString+'\n'+queryStringP
    parsedOutput = queryExpectedCostP(fileString,labellingsPredicates,labellingsP)

    (sumProb,expectedCost,impossible)=buildResults(parsedOutput)

    printResults(p,sumProb,expectedCost,impossible,tables,labellingsP,timestamp,start)