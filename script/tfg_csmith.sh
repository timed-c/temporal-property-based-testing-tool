#!/bin/bash
echo "COMPILING TFG GENERATE"
make
echo "TFG -> TIMED C"
STR="$(./generate-tfg)"
IFS=':' # space is set as delimiter
read -ra ADDR <<< "$STR" # str is read into an array as tokens separated by IFS
SEED=${ADDR[0]}
N=${ADDR[1]}
IFS=' '
echo ${SEED}
echo ${N}
mkdir test/${SEED}
cp test/time_${SEED}.c test/${SEED}
cp test/gen_${SEED}.c test/${SEED}
cp test/tfg_${SEED}.dot test/${SEED}
COM="gcc -w -I/home/saranya/Dokument/csmith-2.3.0/runtime/ -I/home/saranya/Dokument/tfg-monitor/timed-property-based-testing/include test/${SEED}/time_${SEED}.cil.c lib/test_lib.c -lpthread -L/home/saranya/Dokument/ktc/lib/ -lrt -lktc -lm -o test/${SEED}/time_${SEED} "

dot -Tpng test/${SEED}/tfg_${SEED}.dot -o test/${SEED}/graph_${SEED}.png && gnome-open test/${SEED}/graph_${SEED}.png 
echo "COMPILING TIMED C"

PERL5LIB=~/.opam/system/lib/perl5:~/.opam/system/lib/perl5/App:~/.opam/system/lib/perl5/App/Cilly ../../ktc/bin/ktc -I/home/saranya/Dokument/tfg-monitor/timed-property-based-testing/include/ --enable-ext0 -w --save-temps=test/${SEED} test/${SEED}/time_${SEED}.c  

echo "CSMITH - RANDOM C FUNCTIONS"
for (( VAR=1; VAR<=$N; VAR++ ))
do
/home/saranya/Dokument/csmith-2.3.0/src/csmith --nomain --no-int8  --no-uint8 --max-funcs 2 --no-longlong -o csmith/gen_csmith_${VAR}.c
echo  "void code_fragment${VAR}(){func_1();}" >> csmith/gen_csmith_${VAR}.c
COM+="csmith/gen_csmith_${VAR}.c "
done
echo "COMPILING GENERATED C FILES"
${COM}
gcc test/${SEED}/gen_${SEED}.c -o test/${SEED}/tfg_${SEED}
echo "GENERATING TFG LOGS"
test/${SEED}/tfg_${SEED} > test/${SEED}/tfg_${SEED}_log
cp test/${SEED}/tfg_${SEED}_log tfg_log
echo "GENERATING EXECUTION LOGS"
test/${SEED}/time_${SEED} > test/${SEED}/exe_${SEED}_log
cp test/${SEED}/exe_${SEED}_log exe_log
RES="$(checker/a.out)"
echo "${SEED} ${RES} 
