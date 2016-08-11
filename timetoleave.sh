#!/usr/bin/env bash
InitialTime=2000
MaxTime=10000
TimeStep=1000
ProblemSize=20
Iterations=5
MutationRate=0.20
MutationRange=0.20
Model=mas_skel

#echo 1000 >> timetoleave.txt
#echo -n $'\t' >> timetoleave.txt
#for ((iter=0; iter<$Iterations; iter++)); do
#    ./emas --time 1000 --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange
#done
#echo -n $'\n' >> timetoleave.txt
for ((t=2000; t<=64000; t*=2)); do
    echo -n $t >> timetoleave.txt
    echo -n $'\t' >> timetoleave.txt
    for ((iter=0; iter<$Iterations; iter++)); do
        ./emas --time $t --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange
     done
     echo "" >> timetoleave.txt
done

#for ((t=12000; t<=20000; t+=4000)); do
#    echo -n $t >> timetoleave.txt
#    echo -n $'\t' >> timetoleave.txt
#    for ((iter=0; iter<$Iterations; iter++)); do
#        ./emas --time $t --genetic_ops traffic_ops --model $Model --problem_size $ProblemSize --mutation_rate $MutationRate --mutation_range $MutationRange
#        echo -n $'\t' >> result.txt
#     done
#     echo "" >> timetoleave.txt
#done