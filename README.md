

<p><img align="left" width="100"> <h1>Probabilistic SKnife</h1></p>
Probabilistic SKnife (ProbSKnife) is a declarative prototype to evaluate the expected cost of a partitioning expecting to change the initial labelling. For every possible labelling <em>i</em> it calculates:

1. The probability of changing to the labelling _i_

2. The eligible partitionings that satisfy the labelling _i_ with the cost of migration from the starting partitionings

To calculate the expected cost for all the eligible partitionings, **ProbSKnife** is launched from a Python script that parses the results, groups them by the labelling, considers the minimum cost to satsify each labelling, and aggregates by eligible partionings. The result is the list of all eligible partitionings with the cost represented as a pair (number of domains, expected cost).

<br></br>
## Prerequisites

Before using **ProbSKnife** You need to install [ProbLog2](https://dtai.cs.kuleuven.be/problog/index.html) and Python.
## Tutorial

To try **ProbSKnife** with our predefined example:

1. Download or clone this repository.

2. Open a terminal in the project folder and run `python  main.py -h` to have the usage of ProbSKnife
   ```
   main.py [-d DLIMIT] [-k CHANGELIMIT] [-t] [-l] [-h]
        DLIMIT          an integer representing the number of domain maximum for the partitionings (default = unbounded)
        CHANGELIMIT     an integer representing the maximum number of changes admitted from the starting labelling
        -t              shows for every partitioning a table with the reachable partitionings with costs and probabilities
        -l              show partitioning labels (impacts on groupby in table)
        -h              shows this help
   ```

For example, running `python  main.py -k 1 -d 3` calculates the costs for the partitionings with at most **3** domains and considering the change of labelling with at most **1** label change.

The output of such execution is
```

```

The first output is a table that resumes every reachable partitioning with its cost, its probability to be reached. Then, the expected cost of the starting partitioning is printed. Finally, the probability to have not satisfied labelling is printed, this is different from zero if the `DLimit` is too low.

   E.g. executing ````python  main.py -k 1```` results in
   ```
                           partitioning   cost   probability
   0    [[south, west], [east, north]]      0       0.281250
   1  [[south], [west], [east, north]]     20       0.343750
   2  [[north, south], [west], [east]]     80       0.031250
   3  [[south, west], [east], [north]]     20       0.156250
   4  [[south], [north, west], [east]]     60       0.078125
   5  [[east, south], [west], [north]]     80       0.031250
   6  [[south], [east, west], [north]]     60       0.078125
   
   The expected cost is 24.375
   The probability to have a labelling not satisfiable with the set limit is 0.0
   ```

3. B) Activating the argument `-l` the overall results are the same, what changes is the output table view: now the reachable partitionings are annotated with labels for every domain.
 
   E.g. executing ```python  main.py [((top, safe), [south, west]), ((top, safe), [east, north])] 3 -l``` results in
   ```
                                                                       partitioning     cost     probability
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
   
   The expected cost is 24.375
   The probability to have a labelling not satisfiable with the set limit is 0.0
   ```
Note that the tables of 3(A) and 3(B) show the same results, in the first table the partitionings with the same domains are aggregated.

Note 2: The expected cost is not calculated on the tables, but on the minimum cost for every labelling change. The minimum could be not unique and this result in making the sum of the probabilities of the tables greater than 1.