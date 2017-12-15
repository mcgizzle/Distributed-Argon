# Distributed Argon

A distributed implementation of [argon](https://github.com/rubik/argon) built using Cloud Haskell with a PostgreSQL database.

## About
Distributed-Argon uses cloud haskell, implementing the _work-stealing_ and the _master/slave_ algorithm, for distributing the workload of argon, a library which measures code complexity. 

The program accepts a GitHub repository and then calculates the complexity for every file of every commit in the project, storing the results in a database. I created another repository [Charting-Complexity]() to generate the graphs.

## Implementation
I decided to implement two algorithms and graph their results against eachother.

1. Work-Stealing

A __worker__ nodes __steal__ work from the manager when they are available to work. the __manager__ adds the files to a queue. This queue is shared among all the workers and they take work from it. The __workers__ evaluate the complexity, return the result and request more work from the manager. This implementation is often referred to as the self-scheduling or work-stealing pattern.

[Link to implementation in the source](https://github.com/McGizzle/Distributed-Argon/blob/aa160c1d58f0ce72e3940e10a3876750533cc077/src/Lib.hs#L100)

2. Master/Slave

A __manager__ node decides on the distribution of the work. the __manager__ splits up the work evenly (per-file basis) and distributes an even amount to each worker.

[Link to implementation in the source](https://github.com/McGizzle/Distributed-Argon/blob/aa160c1d58f0ce72e3940e10a3876750533cc077/src/Lib.hs#L111)

The __manager__ stores the results it receives from the __workers__ in a database as they come in non-deterministically. 

## Discussion
As I would have expected, the work-stealing pattern was a faster approach on average. This can be seen from the sample results provided below. Rather than the manager sending files, and the workers waiting, it is faster for the manager to send work to whoever is ready and waiting. In the master/slave there is the potential for lost working time while a manager is waiting for a worker to finish some previous task. This do not occur with the work-stealing pattern however, as the manager simply sends the work to whoever requests it first.


## Results

### Work-Stealing
<p align="center">
  <img src="https://github.com/McGizzle/Distributed-Argon/blob/master/img/image1.png"/>
  </p>
  
### Master/Slave
<p align="center">
  <img src="https://github.com/McGizzle/Distributed-Argon/blob/master/img/image2.png"/>
</p>



## The database
A PostgreSQL database is used to store the revelant information relating to a repositories complexity and the time taken with various amounts of nodes. There a  database maintains two 

### Relations

__Repository__ 

| Id | Url | Nodes | Start Time| End time| Pattern |
| --- | --- | --- | --- | --- | --- |
| 1 | https://github.com... | 2 | 2017-11-26 15:02:36.830273+00 | 2017-11-26 15:03:25.63044+00 | work-stealing|

__Commit Info__ 

| Id | Commit | Start Time | End Time |
| --- | --- | --- | --- |
| 1 | 22939d... | 2017-11-26 15:02:36.830273+00 | 2017-11-26 15:02:36.830273+00 |

__Commit Results__

| Id | Commit | File Path | Complexity |
| --- | --- | ---- | --- |
| 1 | 22939d... |  Distributed-Argon/src/Lib.hs | JSON data |


## Prerequisites
* [PostgreSQL](https://www.postgresql.org/) to store the data.
* [Stack](https://docs.haskellstack.org/en/stable/README/) to build and run the project.

## To build with stack
``` stack build ```

## To run
Fire up two shells and execute the following scripts.
#### Start the worker nodes
```bash workers.sh```
#### Start the mananger node
```bash run.sh <Github Repository> <pattern>```
The patterns can be 
`work-stealing` or `master-slave`
#### Note
The number of workers can be edited by altering the worker.sh script.

## Viewing the results
I have built a graphical display of the results using Chart.js. A link to that repo can be found [here](https://github.com/McGizzle/Charting-Complexity)

Alternatively, as all the necessary information is stored in a database, it can therefore be manipulated in any way you see fit.

### Thanks
To all the [argon](https://github.com/rubik/argon) contributors for allowing me to display my distributed programming skills with their great library!
