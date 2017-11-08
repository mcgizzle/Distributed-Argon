#!/bin/bash
stack exec RESTful-Argon worker localhost 7000 &
stack exec RESTful-Argon worker localhost 7001 &
stack exec RESTful-Argon worker localhost 7002 &
stack exec RESTful-Argon manager localhost $1 
