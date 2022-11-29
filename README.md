

<p><img align="left" width="100"> <h1>Probabilistic SKnife</h1></p>
Probabilstic SKnife (ProbSKnife) is a declarative prototype to evaluate the expected cost of a partitioning expecting to change the initial labelling. For every possible labelling it calculates:

1. The probability of changing to the labelling

2. The eligible partitionings that satisfy the labelling with the cost of migration from the starting partitionings

To calculate the expected cost, **ProbSKnife** is launched from a Python script that parse the results, groups them by the labelling minimum and aggregates by eligible partionings.

<br></br>
## Prerequisites

Before using **ProbSKnife** You need to install [ProbLog2](https://dtai.cs.kuleuven.be/problog/index.html) and Python.
## Tutorial

To try **ProbSKnife**:

1. Download or clone this repository.

2. Open a terminal in the project folder and run `python  .\main.py StartingPartitioning`.
   E.g. ```python  .\main.py '[((top, safe), [south, west]), ((top, safe), [east, north])]'```

3. The output is a table that resumes every reachable partitioning with its cost, its probability to be reached and the expected cost to reach it from the starting labelling.

   E.g. for the starting partitioning ```[((top, safe), [south, west]), ((top, safe), [east, north])]```
   ```
                                                                         partitionings  costs  probabilities 
   0                    [((low, safe), [south, west]), ((low, safe), [east, north])]      0       0.125000
   1                     [((low, safe), [south, west]), ((top, low), [east, north])]      0       0.031250
   2    [((low, safe), [south, west]), ((top, low), [east]), ((top, safe), [north])]     20       0.031250
   3                    [((low, safe), [south, west]), ((top, safe), [east, north])]      0       0.031250
   4    [((low, safe), [south, west]), ((top, safe), [east]), ((top, low), [north])]     20       0.031250
   5    [((top, low), [south, east]), ((top, safe), [west]), ((low, safe), [north])]     80       0.031250
   6                     [((top, low), [south, west]), ((low, safe), [east, north])]      0       0.031250
   7                      [((top, low), [south, west]), ((top, low), [east, north])]      0       0.023438
   8     [((top, low), [south, west]), ((top, low), [east]), ((low, safe), [north])]     20       0.031250
   9     [((top, low), [south, west]), ((top, low), [east]), ((top, safe), [north])]     20       0.023438
   10                    [((top, low), [south, west]), ((top, safe), [east, north])]      0       0.023438
   11   [((top, low), [south, west]), ((top, safe), [east]), ((low, safe), [north])]     20       0.031250
   12    [((top, low), [south, west]), ((top, safe), [east]), ((top, low), [north])]     20       0.023438
   13   [((top, low), [south]), ((top, safe), [west, east]), ((low, safe), [north])]     60       0.031250
   14    [((top, low), [south]), ((top, safe), [west, east]), ((top, low), [north])]     60       0.023438
   15    [((top, low), [south]), ((top, safe), [west, north]), ((top, low), [east])]     60       0.023438
   16   [((top, low), [south]), ((top, safe), [west]), ((low, safe), [east, north])]     20       0.031250
   17    [((top, low), [south]), ((top, safe), [west]), ((top, low), [east, north])]     20       0.023438
   18   [((top, low), [south]), ((top, safe), [west]), ((top, safe), [east, north])]     20       0.023438
   19   [((top, safe), [south, east]), ((top, low), [west]), ((low, safe), [north])]     80       0.031250
   20                   [((top, safe), [south, west]), ((low, safe), [east, north])]      0       0.031250
   21                    [((top, safe), [south, west]), ((top, low), [east, north])]      0       0.023438
   22   [((top, safe), [south, west]), ((top, low), [east]), ((low, safe), [north])]     20       0.031250
   23   [((top, safe), [south, west]), ((top, low), [east]), ((top, safe), [north])]     20       0.023438
   24                   [((top, safe), [south, west]), ((top, safe), [east, north])]      0       0.023438
   25  [((top, safe), [south, west]), ((top, safe), [east]), ((low, safe), [north])]     20       0.031250
   26   [((top, safe), [south, west]), ((top, safe), [east]), ((top, low), [north])]     20       0.023438
   27   [((top, safe), [south]), ((top, low), [west, east]), ((low, safe), [north])]     60       0.031250
   28   [((top, safe), [south]), ((top, low), [west, east]), ((top, safe), [north])]     60       0.023438
   29   [((top, safe), [south]), ((top, low), [west, north]), ((top, safe), [east])]     60       0.023438
   30   [((top, safe), [south]), ((top, low), [west]), ((low, safe), [east, north])]     20       0.031250
   31    [((top, safe), [south]), ((top, low), [west]), ((top, low), [east, north])]     20       0.023438
   32   [((top, safe), [south]), ((top, low), [west]), ((top, safe), [east, north])]     20       0.023438
      ```
   Finally, all the expected costs are aggregated to give the expected cost of a partitioning.
   ```
   The expected cost is 23.125
   ```