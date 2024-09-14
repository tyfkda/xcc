#pragma once

typedef struct OnExitChain {
  struct OnExitChain *next;
  void (*func)(void);
} OnExitChain;

extern OnExitChain *__on_exit_chain;
