#!/bin/bash
stack exec RESTful-Argon worker localhost 9000 &
stack exec RESTful-Argon worker localhost 9001 &
stack exec RESTful-Argon manager localhost 9004 
