# Distributed Argon

A distributed implementation of [argon](https://github.com/rubik/argon) built using Cloud Haskell.

## About
Distributed-Argon uses cloud haskell, implementing the 'master/slave' algorithm, for distributing the workload of argon, a library which measures code complexity.

## Implementation
A __manager__ node distributes the work to each __worker__ node. Each filepath is added to a queue by the __manager__ and distributed to the __workers__. The __workers__ evaluate the complexity, return the result and request more work. When all the work is complete, the __mananger__ returns the accumulated result. This implementation is often referred to as the self-scheduling or master/slave approach.
### Integrating argon
The argon library uses stream processing to find and evaluate the files. However this does not translate well to the distributed approach of splitting up the work and evaluating each file on a node one at a time. With this in mind I have included the argon library as a local dependency. This allows the program to create a process which passes a filepath to the argon executable and returns the result.

However, evaluating a single file on a node is overkill which leads to two options for future development:
  * Manipulate the argon source to subvert the piping.
  * Add entire directories to the work queue rather than individual files.

## To compile with stack
``` bash compile.sh ```

## To run
Fire up two shells and execute the following scripts.
#### Start the worker nodes
```bash workers.sh```
#### Start the mananger node
```bash run.sh <directory> ```

#### Note
The number of workers can be edited by altering the worker.sh script.

### Thanks
To all the [argon](https://github.com/rubik/argon) contributors for allowing me to display my distributed programming skills with their great library!
