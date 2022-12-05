from utils import *


#Arguments check
(Dlimit,K,tables,labellingsP) = checkArgs()

queryLabellingsString = 'query(labellingK('+K+',L,D)).'
(labellingsPredicates,StartingLabelling) = queryLabellings(queryLabellingsString,K)

queryAllP = 'query(sKnife(smallExample,'+StartingLabelling+','+Dlimit+',Pi)).'
partitionigs = queryAllPartitionings(queryAllP,StartingLabelling,Dlimit)

for p in partitionigs:
    queryStringP = 'query(futureCost('+p+',smallExample,'+Dlimit+',RES)).'
    parsedOutput = queryExpectedCostP(queryStringP,labellingsPredicates,labellingsP)

    (sumProb,expectedCost,impossible)=buildResults(parsedOutput)

    printResults(p,sumProb,expectedCost,impossible,tables,labellingsP)