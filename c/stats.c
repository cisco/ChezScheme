/* stats.c
 * Copyright 1984-2017 Cisco Systems, Inc.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#if defined(SOLARIS)
/* make gmtime_r and localtime_r visible */
#ifndef _REENTRANT
#define _REENTRANT
#endif
/* make two-argument ctime_r and two-argument asctime_r visible */
#define _POSIX_PTHREAD_SEMANTICS
#endif /* defined(SOLARIS) */

#include "system.h"

#ifdef WIN32
#include <sys/types.h>
#include <sys/timeb.h>
#else /* WIN32 */
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

static struct timespec starting_mono_tp;

static long adjust_time_zone(ptr dtvec, struct tm *tmxp, ptr given_tzoff);

/********  unique-id  ********/

#if (time_t_bits == 32)
#define S_integer_time_t(x) Sinteger32((iptr)(x))
#elif (time_t_bits == 64)
#define S_integer_time_t(x) Sinteger64(x)
#endif

#ifdef WIN32

#include <rpc.h>

ptr S_unique_id(void) {
  union {UUID uuid; U32 foo[4];} u;
  u.foo[0] = 0;
  u.foo[1] = 0;
  u.foo[2] = 0;
  u.foo[3] = 0;
  UuidCreate(&u.uuid);
  return S_add(S_ash(Sunsigned32(u.foo[0]), Sinteger(8*3*sizeof(U32))),
           S_add(S_ash(Sunsigned32(u.foo[1]), Sinteger(8*2*sizeof(U32))),
             S_add(S_ash(Sunsigned32(u.foo[2]), Sinteger(8*sizeof(U32))),
              Sunsigned32(u.foo[3]))));
}

#elif defined(USE_OSSP_UUID) /* WIN32 */

#include <ossp/uuid.h>

ptr S_unique_id(void) {
  uuid_t *uuid;
  U32 bin[4];
  void *bin_ptr = &bin;
  size_t bin_len = sizeof(bin);

  uuid_create(&uuid);
  uuid_make(uuid, UUID_MAKE_V4);
  uuid_export(uuid, UUID_FMT_BIN, &bin_ptr, &bin_len);
  uuid_destroy(uuid);

  return S_add(S_ash(Sunsigned32(bin[0]), Sinteger(8*3*sizeof(U32))),
           S_add(S_ash(Sunsigned32(bin[1]), Sinteger(8*2*sizeof(U32))),
             S_add(S_ash(Sunsigned32(bin[2]), Sinteger(8*sizeof(U32))),
              Sunsigned32(bin[3]))));
}

#elif defined(USE_NETBSD_UUID) /* USE_OSSP_UUID */

#include <uuid.h>

ptr S_unique_id(void) {
  uuid_t uuid;
  uint32_t status;
  unsigned char bin[16];
  ptr n;
  unsigned int i;

  uuid_create(&uuid, &status);
  uuid_enc_le(bin, &uuid);

  n = Sinteger(0);
  for (i = 0; i < sizeof(bin); i++) {
    n = S_add(n, S_ash(Sinteger(bin[i]), Sinteger(8*i)));
  }

  return n;
}

#else /* USE_NETBSD_UUID */

#include <uuid/uuid.h>

ptr S_unique_id(void) {
  union {uuid_t uuid; U32 foo[4];} u;
  u.foo[0] = 0;
  u.foo[1] = 0;
  u.foo[2] = 0;
  u.foo[3] = 0;
  uuid_generate(u.uuid);
  return S_add(S_ash(Sunsigned32(u.foo[0]), Sinteger(8*3*sizeof(U32))),
           S_add(S_ash(Sunsigned32(u.foo[1]), Sinteger(8*2*sizeof(U32))),
             S_add(S_ash(Sunsigned32(u.foo[2]), Sinteger(8*sizeof(U32))),
              Sunsigned32(u.foo[3]))));
}

#endif /* WIN32 */


/********  time and date support  ********/

#ifdef WIN32

static __int64 hires_cps = 0;

typedef void (WINAPI *GetSystemTimeAsFileTime_t)(LPFILETIME lpSystemTimeAsFileTime);

static GetSystemTimeAsFileTime_t s_GetSystemTimeAsFileTime = GetSystemTimeAsFileTime;

void S_gettime(INT typeno, struct timespec *tp) {
  switch (typeno) {
    case time_process: {
      FILETIME ftKernel, ftUser, ftDummy;

      if (GetProcessTimes(GetCurrentProcess(), &ftDummy, &ftDummy,
                          &ftKernel, &ftUser)) {
        __int64 kernel, user, total;
        kernel = ftKernel.dwHighDateTime;
        kernel <<= 32;
        kernel |= ftKernel.dwLowDateTime;
        user = ftUser.dwHighDateTime;
        user <<= 32;
        user |= ftUser.dwLowDateTime;
        total = user + kernel;
        tp->tv_sec = (time_t)(total / 10000000);
        tp->tv_nsec = (long)((total % 10000000) * 100);
        break;
      } else {
        clock_t n = clock();;
      /* if GetProcessTimes fails, we're probably running Windows 95 */
        tp->tv_sec = (time_t)(n / CLOCKS_PER_SEC);
        tp->tv_nsec = (long)((n % CLOCKS_PER_SEC) * (1000000000 / CLOCKS_PER_SEC));
        break;
      }
    }

    case time_thread: {
      FILETIME ftKernel, ftUser, ftDummy;

      if (GetThreadTimes(GetCurrentThread(), &ftDummy, &ftDummy,
                          &ftKernel, &ftUser)) {
        __int64 kernel, user, total;
        kernel = ftKernel.dwHighDateTime;
        kernel <<= 32;
        kernel |= ftKernel.dwLowDateTime;
        user = ftUser.dwHighDateTime;
        user <<= 32;
        user |= ftUser.dwLowDateTime;
        total = user + kernel;
        tp->tv_sec = (time_t)(total / 10000000);
        tp->tv_nsec = (long)((total % 10000000) * 100);
        break;
      } else {
        clock_t n = clock();;
      /* if GetThreadTimes fails, we're probably running Windows 95 */
        tp->tv_sec = (time_t)(n / CLOCKS_PER_SEC);
        tp->tv_nsec = (long)((n % CLOCKS_PER_SEC) * (1000000000 / CLOCKS_PER_SEC));
        break;
      }
    }

    case time_duration:
    case time_monotonic: {
      LARGE_INTEGER count;

      if (hires_cps == 0 && QueryPerformanceFrequency(&count))
        hires_cps = count.QuadPart;

      if (hires_cps && QueryPerformanceCounter(&count)) {
        tp->tv_sec = (time_t)(count.QuadPart / hires_cps);
        tp->tv_nsec = (long)((count.QuadPart % hires_cps) * (1000000000 / hires_cps));
        break;
      } else {
        DWORD count = GetTickCount();
        tp->tv_sec = (time_t)(count / 1000);
        tp->tv_nsec = (long)((count % 1000) * 1000000);
        break;
      }
    }

    case time_utc: {
      FILETIME ft; __int64 total;

      s_GetSystemTimeAsFileTime(&ft);
      total = ft.dwHighDateTime;
      total <<= 32;
      total |= ft.dwLowDateTime;
     /* measurement interval is 100 nanoseconds = 1/10 microseconds */
     /* adjust by number of seconds between Windows (1601) and Unix (1970) epochs */
      tp->tv_sec = (time_t)(total / 10000000 - 11644473600L);
      tp->tv_nsec = (long)((total % 10000000) * 100);
      break;
    }

    default:
      S_error1("S_gettime", "unexpected typeno ~s", Sinteger(typeno));
      break;
  }
}

static struct tm *gmtime_r(const time_t *timep, struct tm *result) {
  return gmtime_s(result, timep) == 0 ? result : NULL;
}

static struct tm *localtime_r(const time_t *timep, struct tm *result) {
  return localtime_s(result, timep) == 0 ? result : NULL;
}

static char *ctime_r(const time_t *timep, char *buf) {
  return ctime_s(buf, 26, timep) == 0 ? buf : NULL;
}

static char *asctime_r(const struct tm *tm, char *buf) {
  return asctime_s(buf, 26, tm) == 0 ? buf : NULL;
}

#else /* WIN32 */

void S_gettime(INT typeno, struct timespec *tp) {
  switch (typeno) {
    case time_thread:
#ifdef CLOCK_THREAD_CPUTIME_ID
      if (clock_gettime(CLOCK_THREAD_CPUTIME_ID, tp) == 0) return;
#endif
     /* fall through */
     /* to utc case in case no thread timer */
    case time_process:
#ifdef CLOCK_PROCESS_CPUTIME_ID
      if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, tp) == 0) return;
#endif
     /* fall back on getrusage if clock_gettime fails */
      {
        struct rusage rbuf;

        if (getrusage(RUSAGE_SELF,&rbuf) != 0)
          S_error1("S_gettime", "failed: ~s", S_strerror(errno));
        tp->tv_sec = rbuf.ru_utime.tv_sec + rbuf.ru_stime.tv_sec;
        tp->tv_nsec = (rbuf.ru_utime.tv_usec + rbuf.ru_stime.tv_usec) * 1000;
        if (tp->tv_nsec >= 1000000000) {
          tp->tv_sec += 1;
          tp->tv_nsec -= 1000000000;
        }
        return;
      }
    case time_duration:
    case time_monotonic:
#ifdef CLOCK_MONOTONIC_HR
      if (clock_gettime(CLOCK_MONOTONIC_HR, tp) == 0) return;
#endif
#ifdef CLOCK_MONOTONIC
      if (clock_gettime(CLOCK_MONOTONIC, tp) == 0) return;
#endif
#ifdef CLOCK_HIGHRES
      if (clock_gettime(CLOCK_HIGHRES, tp) == 0) return;
#endif
     /* fall through */
     /* to utc case in case no monotonic timer */
    case time_utc:
#ifdef CLOCK_REALTIME_HR
      if (clock_gettime(CLOCK_REALTIME_HR, tp) == 0) return;
#endif
#ifdef CLOCK_REALTIME
      if (clock_gettime(CLOCK_REALTIME, tp) == 0) return;
#endif
     /* fall back on gettimeofday if clock_gettime fails */
      {
        struct timeval tvtp;

        if (gettimeofday(&tvtp,NULL) != 0)
          S_error1("S_gettime", "failed: ~s", S_strerror(errno));
        tp->tv_sec = (time_t)tvtp.tv_sec;
        tp->tv_nsec = (long)(tvtp.tv_usec * 1000);
        return;
      }
    default:
      S_error1("S_gettime", "unexpected typeno ~s", Sinteger(typeno));
      break;
  }
}

#endif /* WIN32 */

ptr S_clock_gettime(I32 typeno) {
  struct timespec tp;
  time_t sec; I32 nsec;

  S_gettime(typeno, &tp);

  sec = tp.tv_sec;
  nsec = tp.tv_nsec;

  if (typeno == time_monotonic || typeno == time_duration) {
    sec -= starting_mono_tp.tv_sec;
    nsec -= starting_mono_tp.tv_nsec;
    if (nsec < 0) {
      sec -= 1;
      nsec += 1000000000;
    }
  }

  return Scons(S_integer_time_t(sec), Sinteger(nsec));
}

ptr S_gmtime(ptr tzoff, ptr tspair) {
  time_t tx;
  struct tm tmx;
  ptr dtvec = S_vector(dtvec_size);

  if (tspair == Sfalse) {
    struct timespec tp;

    S_gettime(time_utc, &tp);
    tx = tp.tv_sec;
    INITVECTIT(dtvec, dtvec_nsec) = Sinteger(tp.tv_nsec);
  } else {
    tx = Sinteger_value(Scar(tspair));
    INITVECTIT(dtvec, dtvec_nsec) = Scdr(tspair);
  }

  if (tzoff == Sfalse) {
    if (localtime_r(&tx, &tmx) == NULL) return Sfalse;
    tmx.tm_isdst = -1; /* have mktime determine the DST status */
    if (mktime(&tmx) == (time_t)-1) return Sfalse;
    (void) adjust_time_zone(dtvec, &tmx, Sfalse);
  } else {
    tx += Sinteger_value(tzoff);
    if (gmtime_r(&tx, &tmx) == NULL) return Sfalse;
    INITVECTIT(dtvec, dtvec_tzoff) = tzoff;
    INITVECTIT(dtvec, dtvec_isdst) = Sfalse;
    INITVECTIT(dtvec, dtvec_tzname) = Sfalse;
  }

  INITVECTIT(dtvec, dtvec_sec) = Sinteger(tmx.tm_sec);
  INITVECTIT(dtvec, dtvec_min) = Sinteger(tmx.tm_min);
  INITVECTIT(dtvec, dtvec_hour) = Sinteger(tmx.tm_hour);
  INITVECTIT(dtvec, dtvec_mday) = Sinteger(tmx.tm_mday);
  INITVECTIT(dtvec, dtvec_mon) = Sinteger(tmx.tm_mon + 1);
  INITVECTIT(dtvec, dtvec_year) = Sinteger(tmx.tm_year);
  INITVECTIT(dtvec, dtvec_wday) = Sinteger(tmx.tm_wday);
  INITVECTIT(dtvec, dtvec_yday) = Sinteger(tmx.tm_yday);

  return dtvec;
}

ptr S_asctime(ptr dtvec) {
  char buf[26];

  if (dtvec == Sfalse) {
    time_t tx = time(NULL);
    if (ctime_r(&tx, buf) == NULL) return Sfalse;
  } else {
    struct tm tmx;
    tmx.tm_sec = (int)Sinteger_value(Svector_ref(dtvec, dtvec_sec));
    tmx.tm_min = (int)Sinteger_value(Svector_ref(dtvec, dtvec_min));
    tmx.tm_hour = (int)Sinteger_value(Svector_ref(dtvec, dtvec_hour));
    tmx.tm_mday = (int)Sinteger_value(Svector_ref(dtvec, dtvec_mday));
    tmx.tm_mon = (int)Sinteger_value(Svector_ref(dtvec, dtvec_mon)) - 1;
    tmx.tm_year = (int)Sinteger_value(Svector_ref(dtvec, dtvec_year));
    tmx.tm_wday = (int)Sinteger_value(Svector_ref(dtvec, dtvec_wday));
    tmx.tm_yday = (int)Sinteger_value(Svector_ref(dtvec, dtvec_yday));
    tmx.tm_isdst = (int)Sboolean_value(Svector_ref(dtvec, dtvec_isdst));
    if (asctime_r(&tmx, buf) == NULL) return Sfalse;
  }

  return S_string(buf, 24) /* all but trailing newline */;
}

ptr S_mktime(ptr dtvec) {
  time_t tx;
  struct tm tmx;
  long orig_tzoff, tzoff;
  ptr given_tzoff;

  tmx.tm_sec = (int)Sinteger_value(Svector_ref(dtvec, dtvec_sec));
  tmx.tm_min = (int)Sinteger_value(Svector_ref(dtvec, dtvec_min));
  tmx.tm_hour = (int)Sinteger_value(Svector_ref(dtvec, dtvec_hour));
  tmx.tm_mday = (int)Sinteger_value(Svector_ref(dtvec, dtvec_mday));
  tmx.tm_mon = (int)Sinteger_value(Svector_ref(dtvec, dtvec_mon)) - 1;
  tmx.tm_year = (int)Sinteger_value(Svector_ref(dtvec, dtvec_year));

  given_tzoff = INITVECTIT(dtvec, dtvec_tzoff);
  if (given_tzoff == Sfalse)
    orig_tzoff = 0;
  else
    orig_tzoff = (long)UNFIX(given_tzoff);
  
  tmx.tm_isdst = -1; /* have mktime determine the DST status */
  if ((tx = mktime(&tmx)) == (time_t)-1) return Sfalse;

 /* mktime may have normalized some values, set wday and yday */
  INITVECTIT(dtvec, dtvec_sec) = Sinteger(tmx.tm_sec);
  INITVECTIT(dtvec, dtvec_min) = Sinteger(tmx.tm_min);
  INITVECTIT(dtvec, dtvec_hour) = Sinteger(tmx.tm_hour);
  INITVECTIT(dtvec, dtvec_mday) = Sinteger(tmx.tm_mday);
  INITVECTIT(dtvec, dtvec_mon) = Sinteger(tmx.tm_mon + 1);
  INITVECTIT(dtvec, dtvec_year) = Sinteger(tmx.tm_year);
  INITVECTIT(dtvec, dtvec_wday) = Sinteger(tmx.tm_wday);
  INITVECTIT(dtvec, dtvec_yday) = Sinteger(tmx.tm_yday);

  tzoff = adjust_time_zone(dtvec, &tmx, given_tzoff);

  if (tzoff != orig_tzoff) tx = tx - orig_tzoff + tzoff;

  return Scons(S_integer_time_t(tx), Svector_ref(dtvec, dtvec_nsec));
}

static long adjust_time_zone(ptr dtvec, struct tm *tmxp, ptr given_tzoff) {
  ptr tz_name = Sfalse;
  long use_tzoff, tzoff;

#ifdef WIN32
  {
    TIME_ZONE_INFORMATION tz;
    wchar_t *w_tzname;

    /* The ...ForYear() function is available on Windows Vista and later: */
    GetTimeZoneInformationForYear(tmxp->tm_year, NULL, &tz);
    
    if (tmxp->tm_isdst) {
      tzoff = (tz.Bias + tz.DaylightBias) * -60;
      w_tzname = tz.DaylightName;
    } else {
      tzoff = (tz.Bias + tz.StandardBias) * -60;
      w_tzname = tz.StandardName;
    }

    if (given_tzoff == Sfalse) {
      char *name = Swide_to_utf8(w_tzname);
      tz_name = Sstring_utf8(name, -1);
      free(name);
    }
  }
#else
  tzoff = tmxp->tm_gmtoff;
  if (given_tzoff == Sfalse) {
# if defined(__linux__) || defined(SOLARIS)
    /* Linux and Solaris set `tzname`: */
    tz_name = Sstring_utf8(tzname[tmxp->tm_isdst], -1);
# else
    /* BSD variants add `tm_zone` in `struct tm`: */
    tz_name = Sstring_utf8(tmxp->tm_zone, -1);
# endif
  }
#endif

  if (given_tzoff == Sfalse)
    use_tzoff = tzoff;
  else
    use_tzoff = (long)UNFIX(given_tzoff);

  INITVECTIT(dtvec, dtvec_isdst) = ((given_tzoff == Sfalse) ? Sboolean(tmxp->tm_isdst) : Sfalse);
  INITVECTIT(dtvec, dtvec_tzoff) = FIX(use_tzoff);
  INITVECTIT(dtvec, dtvec_tzname) = tz_name;

  return tzoff;
}

/********  old real-time and cpu-time support  ********/

ptr S_cputime(void) {
  struct timespec tp;

  S_gettime(time_process, &tp);
  return S_add(S_mul(S_integer_time_t(tp.tv_sec), FIX(1000)),
               Sinteger((tp.tv_nsec + 500000) / 1000000));
}

ptr S_realtime(void) {
  struct timespec tp;
  time_t sec; I32 nsec;

  S_gettime(time_monotonic, &tp);

  sec = tp.tv_sec - starting_mono_tp.tv_sec;
  nsec = tp.tv_nsec - starting_mono_tp.tv_nsec;
  if (nsec < 0) {
    sec -= 1;
    nsec += 1000000000;
  }
  return S_add(S_mul(S_integer_time_t(sec), FIX(1000)),
               Sinteger((nsec + 500000) / 1000000));
}

/********  initialization  ********/

void S_stats_init(void) {
#ifdef WIN32
  /* Use GetSystemTimePreciseAsFileTime when available (Windows 8 and later). */
  HMODULE h = LoadLibraryW(L"kernel32.dll");
  if (h != NULL) {
    GetSystemTimeAsFileTime_t proc = (GetSystemTimeAsFileTime_t)GetProcAddress(h, "GetSystemTimePreciseAsFileTime");
    if (proc != NULL)
      s_GetSystemTimeAsFileTime = proc;
    else
      FreeLibrary(h);
  }
#endif
  S_gettime(time_monotonic, &starting_mono_tp);
}
