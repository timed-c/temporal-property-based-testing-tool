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

void testing_init_time(struct timespec* tt, struct timespec* ex){
     printf("Frag#0 \n");
     clock_gettime(CLOCK_REALTIME, tt);
	 clock_gettime(CLOCK_REALTIME, ex);
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

long testing_exit(struct timespec* tt, long* entry_time, int id, char* stporftp, int crtcl, long overshot, long* time_until_critical, struct timespec* ts){
   struct timespec res;
   long exit_time;
   char stpstring[] = "STP";
   clock_gettime(CLOCK_REALTIME, ts);
   exit_time = testing_timspec_diff(tt, ts, &res);
 
   //printf("exit : %lld.%.9ld\n", (long long)ts.tv_sec, ts.tv_nsec);
   if(strcmp(stporftp, stpstring) == 0){
		printf("%s#%d %ld %ld\n", stporftp, id, *entry_time, exit_time);
   }
   else{
		printf("%s#%d %ld %ld %ld %ld %ld\n", stporftp, id, *entry_time, exit_time, crtcl, overshot, *time_until_critical);

   }
   *time_until_critical = 0;
   return; 
}

long testing_compute_time_until_critical(struct timespec* ts){
   struct timespec now, res;
   long time_until_critical;
   clock_gettime(CLOCK_REALTIME, &now);
   time_until_critical = testing_timspec_diff(ts, &now, &res);
   return (time_until_critical); 
}





