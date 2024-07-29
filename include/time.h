#pragma once

#include <stddef.h>  // size_t

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

typedef long clock_t;
clock_t clock(void);

#define CLOCKS_PER_SEC  1000000

struct tm {
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  int tm_isdst;
};

struct tm *gmtime(const time_t *);
struct tm *localtime(const time_t *);
double difftime(time_t, time_t);
time_t mktime(struct tm *);
size_t strftime(char *, size_t, const char *, const struct tm *);
