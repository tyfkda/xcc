#pragma once

#if !defined(__wasm)
#include <sys/types.h>  // pid_t

#if defined(__APPLE__)
#define	SIGHUP     1
#define	SIGINT     2
#define	SIGQUIT    3
#define	SIGILL     4
#define	SIGTRAP    5
#define	SIGABRT    6
#define	SIGEMT     7
#define	SIGFPE     8
#define	SIGKILL    9
#define	SIGBUS     10
#define	SIGSEGV    11
#define	SIGSYS     12
#define	SIGPIPE    13
#define	SIGALRM    14
#define	SIGTERM    15
#define	SIGURG     16
#define	SIGSTOP    17
#define	SIGTSTP    18
#define	SIGCONT    19
#define	SIGCHLD    20
#define	SIGTTIN    21
#define	SIGTTOU    22
#define	SIGIO      23
#define	SIGXCPU    24
#define	SIGXFSZ    25
#define	SIGVTALRM  26
#define	SIGPROF    27
#define	SIGWINCH   28
#define	SIGINFO    29
#define	SIGUSR1    30
#define	SIGUSR2    31

#else
#define	SIGHUP     1
#define	SIGINT     2
#define	SIGQUIT    3
#define	SIGILL     4
#define	SIGTRAP    5
#define	SIGABRT    6
#define	SIGBUS     7
#define	SIGFPE     8
#define	SIGKILL    9
#define	SIGUSR1    10
#define	SIGSEGV    11
#define	SIGUSR2    12
#define	SIGPIPE    13
#define	SIGALRM    14
#define	SIGTERM    15
#define	SIGSTKFLT  16
#define	SIGCHLD    17
#define	SIGCONT    18
#define	SIGSTOP    19
#define	SIGTSTP    20
#define	SIGTTIN    21
#define	SIGTTOU    22
#define	SIGURG     23
#define	SIGXCPU    24
#define	SIGXFSZ    25
#define	SIGVTALRM  26
#define	SIGPROF    27
#define	SIGWINCH   28
#define	SIGIO      29
#define	SIGPWR     30
#define	SIGSYS     31

#define	SIGRTMIN   34
#define	SIGRTMAX   64
#endif

#define SIG_DFL  ((void (*)(int))0)

int kill(pid_t pid, int sig);
int raise(int sig);

void (*signal(int sig, void (*func)(int)))(int);
#endif
