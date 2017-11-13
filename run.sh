#!/bin/bash
stack exec Distributed-Argon worker localhost 7000 &
stack exec distributed-Argon worker localhost 7001 &
stack exec Distributed-Argon worker localhost 7002 &
stack exec Distributed-Argon manager $1 localhost 7003 
