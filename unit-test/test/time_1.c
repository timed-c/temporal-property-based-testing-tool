#include<stdio.h>
#include<cilktc.h>
#include"pbt.h"
task fun(){
	struct timespec testing_tt;
	long testing_et;
	testing_init_time(&testing_tt);
 	printf("Frag#1 \n");
	code_fragment1();
	testing_et = testing_entry(&testing_tt);
	stp(30, 20, ms);
	testing_exit(&testing_tt, testing_et, 2, "STP");
}
int main(){
	srand(1);
	fun();
}