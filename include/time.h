#pragma once

typedef long time_t;

struct timespec {
  time_t tv_sec;
  long tv_nsec;
};

typedef enum {
  CLOCK_REALTIME = 0,
  CLOCK_REALTIME_COARSE = 5,
} clockid_t;

time_t time(time_t *timer);
int clock_gettime(clockid_t clk_id, struct timespec *tp);
