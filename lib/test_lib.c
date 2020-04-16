#include <stdio.h>
#include <time.h>

//#define _POSIX_C_SOURCE 199309L

long testing_timspec_diff(struct timespec *start, struct timespec *stop, struct timespec *result)
{
    if ((stop->tv_nsec - start->tv_nsec) < 0) {
        result->tv_sec = stop->tv_sec - start->tv_sec - 1;
        result->tv_nsec = stop->tv_nsec - start->tv_nsec + 1000000000;
    } else {
        result->tv_sec = stop->tv_sec - start->tv_sec;
        result->tv_nsec = stop->tv_nsec - start->tv_nsec;
    }
	
    return ((result->tv_sec * 1000) + (((float) result->tv_nsec / (float)1000000)));
}

void testing_init_time(struct timespec* tt){
     printf("Frag#0 \n");
     clock_gettime(CLOCK_REALTIME, tt);
     //printf("init : %lld.%.9ld\n", (long long)tt->tv_sec, tt->tv_nsec);
}

long testing_entry(struct timespec* tt){
   struct timespec ts, result_ts;
   long result;
   clock_gettime(CLOCK_REALTIME, &ts);
   //printf("entry : %lld.%.9ld\n", (long long)ts.tv_sec, ts.tv_nsec);
   result = testing_timspec_diff(tt, &ts, &result);
   return result;
}

long testing_exit(struct timespec* tt, long entry_time, int id, char* stporftp){
   struct timespec ts, res;
   long exit_time;
   clock_gettime(CLOCK_REALTIME, &ts);
   exit_time = testing_timspec_diff(tt, &ts, &res);
   //printf("exit : %lld.%.9ld\n", (long long)ts.tv_sec, ts.tv_nsec);
   printf("%s#%d %ld %ld\n", stporftp, id, entry_time, exit_time);
}




