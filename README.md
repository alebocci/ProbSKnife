

<p><img align="left" width="100"> <h1>Probabilistic SKnife</h1></p>
Probabilistic SKnife (ProbSKnife) is a declarative prototype to evaluate the expected cost of all the partionings of an application with their cost <strong>&lt;number_of_domains, future_cost&gt;</strong>.
To calculate the future cost for all the eligible partitionings, <strong>ProbSKnife</strong> is launched from a Python script that parses the results, groups them by the labelling, considers the minimum cost to satsify each labelling, and aggregates by eligible partionings.

<br></br>
## Prerequisites

Before using **ProbSKnife** You need to install [ProbLog2](https://dtai.cs.kuleuven.be/problog/index.html) and Python.
## Tutorial

To try **ProbSKnife** with our predefined example:

1. Download or clone this repository.

2. Open a terminal in the project folder and run `python  main.py -h` to have the usage of ProbSKnife
   ```
   main.py APPID [-d DLIMIT] [-k CHANGELIMIT] [-f] [-l] [-h]"
        APPID           identifier of the application to partition
        DLIMIT          an integer
        CHANGELIMIT     an integer
        -f              shows full results in tables
        -l              shows partitioning labels (impacts on groupby in table)
        -t              shows timestamp of operations
        -h              shows this help
   ```
3. Execute the program!
As an example, running `python  main.py cloudModel -k 1 -d 5` calculates the costs for the partitionings with at most **5** domains and considering the change of labelling with at most **1** label change of the default example **cloudModel**.

   The output of such execution is
   ```
   Partitioning [[appManager, authenticator, db], [apiGateway], [aiLearning], [userConfig]]                        cost: (4, 3.6789741961)
   Partitioning [[db], [apiGateway], [aiLearning], [appManager, authenticator], [userConfig]]                      cost: (5, 0.0)
   Partitioning [[authenticator, db], [apiGateway], [aiLearning], [appManager], [userConfig]]                      cost: (5, 2.6278387115)
   Partitioning [[appManager, db], [apiGateway], [aiLearning], [authenticator], [userConfig]]                      cost: (5, 6.3068129076)
   Impossible prob: 0.17518924743
   ```
   Showing the 4 starting partitionings, their cost and the probability to reach a labelling making the application not safely partitionable.
   
   The probabilistic model with the chosed K is saved in a .pl file to be reused in the future. The format of the file is
   ```
   Probability::labelling0L(Labelling).
   ```
    The above execution creates the file ```labellingK1_cloudModel.pl``` with (part) of content
    ```
   0.09197435490::labelling0L([(networkData,low),(cryptedData,low),(userPreferences,medium),(userRequests,medium),(iotMeasurements,top),(iotEvents,top),(iotCommands,top),(tlsLibrary,top),(fromProvider,low),(aiFramework,low),(dbms,top),(networkLibrary,low),(dataLibrary,top)]).
   0.09197435490::labelling0L([(networkData,low),(cryptedData,low),(userPreferences,medium),(userRequests,medium),(iotMeasurements,top),(iotEvents,top),(iotCommands,top),(tlsLibrary,top),(fromProvider,low),(aiFramework,low),(dbms,top),(networkLibrary,low),(dataLibrary,low)]).
    ```
    Showing two labellings with 1 label different from the starting one and their probability.
    
4. By activating the argument `-t` the overall results are the same, what changes is the output information view: now the reachable partitionings are annotated with logging information and timestamps.

   As an example, running `python  main.py cloudModel -k 1 -d 5 -t` calculates the costs for the partitionings with at most **5** domains and considering the change of labelling with at most **1** label change of the default example **cloudModel**.

   The output of such execution is

   ```
   Starting execution.
   Retrieving application information.
   Starting to create labelling probabilities at:0:00:00.119381

   Labelling created before.

   Search for all eligible partitionings.
   4 partitionings determined at: 0:00:00.184154

   Determining cost for every partitioning.
   Partitioning [[appManager, authenticator, db], [apiGateway], [aiLearning], [userConfig]]                        cost: (4, 3.6789741961)
   Time from execution start: 0:00:01.465134
   Partitioning [[db], [apiGateway], [aiLearning], [appManager, authenticator], [userConfig]]                      cost: (5, 0.0)
   Time from execution start: 0:00:02.731906
   Partitioning [[authenticator, db], [apiGateway], [aiLearning], [appManager], [userConfig]]                      cost: (5, 2.6278387115)
   Time from execution start: 0:00:04.014362
   Partitioning [[appManager, db], [apiGateway], [aiLearning], [authenticator], [userConfig]]                      cost: (5, 6.3068129076)
   Time from execution start: 0:00:05.279141
   Impossible prob: 0.17518924743
   ```
   Showing the same output as before with annotated timestamp. Note that the labellaing probabilities where already created to reduce the overall execution time.
