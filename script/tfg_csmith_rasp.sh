#!/bin/bash
ARG1=${1}
ARG2=${2:-0}
echo "COMPILING TFG GENERATE"
make
echo "TFG -> TIMED C"
if [[ "$ARG2" == '0' ]]; then
		STR="$(./generate-tfg)"
else
		STR="$(./generate-tfg $ARG2)"
fi
IFS=':' # space is set as delimiter
read -ra ADDR <<< "$STR" # str is read into an array as tokens separated by IFS
SEED=${ADDR[0]}
N=${ADDR[1]}
IFS=' '
echo ${SEED}
echo ${N}
rm -rf test/${SEED}
rm -rf exe-log 
rm -rf tfg-log
#rm -rf test-summary
mkdir test/${SEED}
cp test/time_${SEED}.c test/${SEED}
cp test/gen_${SEED}.c test/${SEED}
cp test/tfg_${SEED}.dot test/${SEED}
if [[ "$ARG1" == "--rasp=true" ]]; then
		COM="/opt/arm-rpi-4.9.3-linux-gnueabihf/bin/arm-linux-gnueabihf-gcc  -w -I/home/saranya/Dokument/csmith-2.3.0/runtime/ -I/home/saranya/Dokument/tfg-monitor/timed-property-based-testing/include test/${SEED}/time_${SEED}.cil.c lib/test_lib.c -lpthread -L/home/saranya/Dokument/ktc/lib/ -lrt  -lktcrasp -lm -o test/${SEED}/time_${SEED} "
fi
if [[ "$ARG1" == "--rasp=false" ]]; then
		COM="gcc -w -I/home/saranya/Dokument/csmith-2.3.0/runtime/ -I/home/saranya/Dokument/tfg-monitor/timed-property-based-testing/include test/${SEED}/time_${SEED}.cil.c lib/test_lib.c -lpthread -L/home/saranya/Dokument/ktc/lib/ -lmplogs -lrt -lktc -lm -o test/${SEED}/time_${SEED} "
fi

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
if [[ "$ARG1" == "--rasp=false" ]]; then
		sudo test/${SEED}/time_${SEED} > test/${SEED}/exe_${SEED}_log
		cp test/${SEED}/exe_${SEED}_log exe_log
fi
if [[ "$ARG1" == "--rasp=true" ]]; then
		EXE="time_${SEED}"
		scp test/${SEED}/$EXE pi@10.42.0.101:/home/pi/Documents/pbt
		ssh pi@10.42.0.101  "sudo /home/pi/Documents/pbt/$EXE " > exe_log
		cp exe_log test/${SEED}/exe_${SEED}_log
fi
echo "TESTING"
RES="$(checker/a.out)"
echo "${SEED} ${RES}" 
echo "${SEED} ${RES}" >> test-summary 


