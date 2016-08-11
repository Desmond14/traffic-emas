#!/bin/bash
#PBS -q plgrid
#PBS -l walltime=48:00:00
#PBS -l nodes=1:ppn=12
module load plgrid/apps/erlang

Time=15000
ProblemSize=20
MinCarsNumber=10
CarsNumberStep=10
MaxCarsNumber=60
Repetitions=25
MutationRate=0.20
MutationRange=0.20
Model=mas_skel
RandomizationChance=0.01


cd /people/plgslakomy/traffic-emas
make

for ((iter=$MinCarsNumber; iter<=$MaxCarsNumber; iter+=$CarsNumberStep)); do
    echo $(date '+%Y %b %d %H:%M')
    ./benchmark --time $Time --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange --cars_number $iter --randomization_chance 0.01 --repetitions $Repetitions
    echo $(date '+%Y %b %d %H:%M')
 done

for ((iter=$MinCarsNumber; iter<=$MaxCarsNumber; iter+=$CarsNumberStep)); do
    echo $(date '+%Y %b %d %H:%M')
    ./benchmark --time $Time --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange --cars_number $iter --randomization_chance 0.05 --repetitions $Repetitions
    echo $(date '+%Y %b %d %H:%M')
 done

for ((iter=$MinCarsNumber; iter<=$MaxCarsNumber; iter+=$CarsNumberStep)); do
    echo $(date '+%Y %b %d %H:%M')
    ./benchmark --time $Time --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange --cars_number $iter --randomization_chance 0.001 --repetitions $Repetitions
    echo $(date '+%Y %b %d %H:%M')
 done