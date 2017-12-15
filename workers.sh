#!/bin/bash
stack exec Distributed-Argon worker localhost 8000 &
stack exec distributed-Argon worker localhost 8001 &
stack exec Distributed-Argon worker localhost 8007 

