import sys
import os
import subprocess
import pandas as pd
from parse import *
from parse import compile
from datetime import datetime

class Partitioning:
    def __init__(self, pstring):
        self.domains = []
        for domains in findall('[{}]', pstring):
            for d in domains:
                d=d.replace(',',' ')
                sws = d.split()
                sws.sort()
                self.domains.append(tuple(sws))
        self.domains=tuple(self.domains)

    def __str__(self):
        return str(self.domains).replace('\'','').replace(',)',')').replace('(','[').replace(')',']')    

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
        return hash(self.domains)

def checkArgs():
    usage = 'Usage: "main.py APPID [-d DLIMIT] [-k CHANGELIMIT] [-f] [-l] [-h]"\n\tAPPID\t\tidentifier of the application to partition\n\tDLIMIT\t\tan integer\n\tCHANGELIMIT\tan integer\n\t-f\t\tshows full results in tables\n\t-l\t\tshows partitioning labels (impacts on groupby in table)\n\t-t\t\tshows timestamp of operations\n\t-h\t\tshows this help'
    args = sys.argv[1:]

    if(len(args)<1):
        print(usage)
        exit()
    
    appId=args[0]

    #print help
    if('-h' in args):
        print(usage)
        exit()

    #Without limit the query as 'inf' as upper bound for partioning dimension
    Dlimit='inf'
    #Check for domain limit argument
    if('-d' in args):
        i_ind=args.index('-d')
        if(not len(args)>i_ind+1):
            print(usage)
            exit()
        Dlimit = args[i_ind+1]
        if(not Dlimit.isdigit()):
            print(usage)
            exit()
    #Check for number of changes limit argument
    K = 'inf'
    if('-k' in args):
        i_ind=args.index('-k')
        if(not len(args)>i_ind+1):
            print(usage)
            exit()
        K = args[i_ind+1]
        if(not K.isdigit()):
            print(usage)
            exit()
    #Check for show tables argument and labelling of partitionings argument
    tables = False
    labellingsP = False
    timestamp = False
    if('-f' in args):
        tables = True
    if('-l' in args):
        labellingsP = True
    if('-t' in args):
        timestamp = True
    
    return (appId,Dlimit,K,tables,labellingsP,timestamp)

def checkApp(appId,K,Dlimit):
    consultFile = appId+'.pl'
    queryString1 = ' findall(S,software(S,_,_,_,_),Sws), length(Sws,N), write(N).'

    output = subprocess.run(['swipl', '-s', consultFile,'-g',queryString1,'-t','halt'], stdout=subprocess.PIPE,stderr=subprocess.DEVNULL)
               
    s=str(output.stdout,'utf-8')
    lines = s.splitlines()
    if(not lines):
        print('Something went wrong parsing the model software, check the Prolog model.')
        exit()

    MaxDlimit = int(lines[0])
    if(Dlimit=='inf' or int(Dlimit)>MaxDlimit):
        Dlimit=str(MaxDlimit)

    
    queryString2 = 'startingLabelling(S), length(S,N), write(S),write(N).'
    consultString = 'consult(\''+appId+'.pl\').'

    output = subprocess.run(['swipl', '-s', 'mainPr.pl','-g',consultString,'-g',queryString2,'-t','halt'], stdout=subprocess.PIPE,stderr=subprocess.DEVNULL)
    
    s=str(output.stdout,'utf-8')
    s=s.replace('\t', '')
    s=s.replace(' ', '')
    lines = s.splitlines()
    if(not lines):
        print('Something went wrong retrieving the starting labelling, check the Prolog model.')
        exit()

    res = lines[0]
    
    formatString = '[{}]{:d}'
    parsed = search(formatString, res)
    if(parsed is None):
        print(res)
        #parsing problem, probably output error from prolog
        print('Something went wrong parsing the starting labelling, check the Prolog model.')
        exit()
    
    MaxK = int(parsed[1])
    if(K=='inf' or int(K)>MaxK):
        K=str(MaxK)
    return (('['+parsed[0]+']',Dlimit,K))


def queryLabellings(appId,startingLabelling,K):
    labellingsFile='labellingK'+K+'_'+appId+'.pl'
    if(os.path.isfile(labellingsFile)):
        already = True
        #labelling probabilities already calculated and printed on file
        with open(labellingsFile, 'r') as labFile:
            lines = labFile.readlines()
            format_output1 = '{}::labelling0L([{}]).\n'
            labellings = []
            for line in lines :
                line = line.replace('\t', '')
                line = line.replace(' ', '')
                parsed = parse(format_output1, line)
                if(parsed is None):
                    print(line)
                    #parsing problem, probably output error from prolog
                    print('Something went wrong parsing the prob labelling file, delete it and retry.')
                    exit()
                prob = float(parsed[0])
                labelling = '['+parsed[1]+']'
                if(prob>0.0):
                    if(labelling==startingLabelling):
                        Labelling0 = ((startingLabelling,prob))
                    else:
                        labellings.append((labelling,prob))
    #calculate labellings and probabilities
    else:
        already=False
        queryString = 'findall((L,D),labellingK('+K+',L,D),Ls), write(Ls).'

        consultString = 'consult(\''+appId+'.pl\').'
        
        output = subprocess.run(['swipl', '-s','mainPr.pl','-g','set_prolog_stack(global, limit(25 000 000 000)).','-g',consultString,'-g',queryString,'-t','halt'], stdout=subprocess.PIPE)
                
        s=str(output.stdout,'utf-8')
        s=s.replace('\t', '')
        s=s.replace(' ', '')
        lines = s.splitlines()
        if(not lines):
            print('Something went wrong looking for all the labellings, check the Prolog model.')
            exit()

        res = lines[0]

        formatString = '([{}],{:d})'
        p = compile('({},{},{})')
        labellings = []
        denominator = 0.0
        for klabelling,distance in findall(formatString,res):
            labelling='['
            prob=1.0
            for (dc,lb,pr) in p.findall(klabelling):
                labelling+='('+dc+','+lb+'),'
                prob*=float(pr)
            if(prob<=0.0):
                continue
            labelling = labelling[:-1]+']'
            if(distance==0):
                Labelling0=((startingLabelling,prob))
            else:
                labellings.append((labelling,prob))
                denominator+=prob

        denominator=round(denominator,11)
        #Calculating change labelling probabilities
        probLabellings=[]
        labellingString =''
        sump=0.0
        for (l,p) in labellings:
            pl = round(p * (1 - Labelling0[1]) / denominator,11)
            sump+=pl
            plString =f'{pl:.11f}'
            labellingString+=plString+'::labelling0L('+l+').\n'
            probLabellings.append((l,pl))
        
        #Starting labelling probabilities takes the error fro summing all to 1
        labellingString += str(1-sump)+'::labelling0L('+Labelling0[0]+').\n'
        probLabellings.append((startingLabelling,(1-sump)))
        with open(labellingsFile, 'w') as f:
            f.write(labellingString)
        labellings=probLabellings

    return (labellings,already)

def queryAllPartitionings(appId,StartingLabelling,Dlimit):
    
    queryString = 'findall(Pi,sKnife('+appId+','+StartingLabelling+','+Dlimit+',Pi),Ps), write(Ps).'

    consultString = 'consult(\''+appId+'.pl\').'
        
    output = subprocess.run(['swipl', '-s','mainPr.pl','-g',consultString,'-g',queryString,'-t','halt'], stdout=subprocess.PIPE)
                
    s=str(output.stdout,'utf-8')
    s=s.replace('\t', '')
    s=s.replace(' ', '')
    if(s=='[]'):
        print('No initial partitioning available, change DLimit or check the model.')
        exit()
    lines = s.splitlines()
    if(not lines):
        print('Something went wrong looking for all the partitionings, check the Prolog model.')
        exit()

    res = lines[0]
    
    formatString='[({}])]'
    partitionings=[]
    for parsed in findall(formatString,res):
        partitioning='[('+parsed[0]+'])]'
        partitionings.append(partitioning)
    return partitionings

def queryFutureCost(appId,partitioning,labelling,Dlimit):
    queryStringP = 'futureCost('+partitioning+','+appId+','+Dlimit+','+labelling+',(Ps)), write(Ps).'

    consultString2 = 'consult(\''+appId+'.pl\').'
        
    output = subprocess.run(['swipl', '-s','mainPr.pl','-g',consultString2,'-g',queryStringP,'-t','halt'], stdout=subprocess.PIPE)
               
    s=str(output.stdout,'utf-8')
    s=s.replace('\t', '')
    s=s.replace(' ', '')
    if(s=='[]'):
        return None

    lines = s.splitlines()
    if(not lines):
        print(lines)
        print("Future cost query failed!")
        exit()
    res = lines[0]

    formatString='([({}])],{:d})'
    result=[]
    for partitioning, cost in findall(formatString,res):
        partString='[('+partitioning+'])]'
        result.append((partString,cost))
    return result

def buildResults(labs,parts,costs,probs):
    df = pd.DataFrame()
   
    df['labelling'] =labs
    df['partitioning']= parts
    df['cost']= costs
    df['probability']= probs
    
    #groping by labelling to find minimum partitionings
    minGroup = df.groupby(['labelling']).cost.min()
    minGroup = minGroup.reset_index()

    #filter out every row without min cost per labelling
    keys = list(minGroup.columns.values)
    i1 = df.set_index(keys).index
    i2 = minGroup.set_index(keys).index
    minFilter = df[i1.isin(i2)]

    
    #grouping by partitioning for the final table
    sumProb = minFilter.groupby(['partitioning','cost']).probability.sum()
    sumProb = sumProb.reset_index()

    #calculating expected cost considering only the minimum cost per labelling
    exp = minFilter.drop_duplicates(subset=['labelling','cost','probability']).reset_index()
    expectedCost = round((exp['cost'] * exp['probability']).sum(),10)

    return (sumProb,expectedCost)

def printResults(Pstring,sumProb,expectedCost,impossible,verbose,labellingsP,Timestamp,start):
    Pstring = Pstring[1:-1]
    p = Partitioning(Pstring)
    ndomains = len(p.domains)
    if(labellingsP):
        printP=Pstring
    else:
        printP=str(p)
    
    print('Partitioning '+printP+'\t\t\tcost: ('+str(ndomains)+', '+str(expectedCost)+').\tImpossible prob: '+str(impossible))
    if(verbose):
        print('All the reachable partitionings with cost and probability are:')
        print(sumProb.to_string()+'\n')
    if(Timestamp):
        print('Time from execution start: '+str(datetime.now()-start))