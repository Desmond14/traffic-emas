#!/bin/bash
#PBS -q plgrid
#PBS -l walltime=08:00:00
#PBS -l nodes=1:ppn=12
module load plgrid/apps/erlang

ProblemSize=20
Iterations=30
MutationRate=0.20
MutationRange=0.20
Model=mas_skel

cd /people/plgslakomy/traffic-emas
make

cp priv/cars10.input priv/input.cars

echo "cars10.input" >> result.txt

echo $(date '+%Y %b %d %H:%M')

for t in 1000 2000 3000 4000 6000 8000 10000 12000 16000 20000 24000; do
    echo -n $t >> result.txt
    echo -n $'\t' >> result.txt
    for ((iter=0; iter<$Iterations; iter++)); do
        ./emas --time $t --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange
        echo -n $'\t' >> result.txt
     done
     echo "" >> result.txt
done

echo $(date '+%Y %b %d %H:%M')

cp priv/cars60.input priv/input.cars
echo "cars60.input" >> result.txt

for t in 1000 2000 3000 4000 6000 8000 10000 12000 16000 20000 24000; do
    echo -n $t >> result.txt
    echo -n $'\t' >> result.txt
    for ((iter=0; iter<$Iterations; iter++)); do
        ./emas --time $t --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange
        echo -n $'\t' >> result.txt
     done
     echo "" >> result.txt
done

echo $(date '+%Y %b %d %H:%M')

cp priv/cars30.input priv/input.cars
echo "cars30.input" >> result.txt

for t in 1000 2000 3000 4000 6000 8000 10000 12000 16000 20000 24000; do
    echo -n $t >> result.txt
    echo -n $'\t' >> result.txt
    for ((iter=0; iter<$Iterations; iter++)); do
        ./emas --time $t --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange
        echo -n $'\t' >> result.txt
     done
     echo "" >> result.txt
done

echo $(date '+%Y %b %d %H:%M')