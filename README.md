# Distributed Argon

A distributed implementation of [argon](https://github.com/rubik/argon) built using Cloud Haskell.

## About
Distributed-Argon uses a RESTful API implementing the 'master/slave' algorithm for distributing the workload of Argon, a library which measures code complexity. Each file which needs to be calculated is added to a queue by the master node. Worker nodes take work from this queue and send the result to the manager when complete.

## To compile with stack
``` bash compile.sh ```

## To run

### Start the worker nodes
```bash workers.sh```
### Start the mananger node
```bash run.sh <directory> ```
