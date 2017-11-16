#!/bin/bash
stack exec Distributed-Argon worker localhost 8000 &
stack exec distributed-Argon worker localhost 8001 &
stack exec Distributed-Argon worker localhost 8002 &
stack exec Distributed-Argon worker localhost 8003 &
stack exec Distributed-Argon worker localhost 8004 &
stack exec Distributed-Argon worker localhost 8005 

