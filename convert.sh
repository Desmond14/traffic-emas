#!/usr/bin/env bash
./converter --problem_size $ProblemSize
zip result.zip result/*.json
rm result/*