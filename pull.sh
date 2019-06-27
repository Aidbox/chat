#!/bin/bash
for i in {1..1000}
do
   curl http://localhost:8080/fooo &> /dev/null
done


