/**
 * Copyright 2013 Takeru Ohta <phjgt308@gmail.com>
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/prctl.h>

#define ERRMSG(Message) fprintf(stderr, "[error:%d] " Message ": pid=%d, ppid=%d, error=%s(%d)\n", __LINE__, getpid(), getppid(), strerror(errno), errno)
#define ERR_EXIT(Message) {ERRMSG(Message); exit(1);}

int parent_main(pid_t child_pid);

int main(int argc, char ** argv)
{
  if (argc < 2) {
    fprintf(stderr, "Usage: safeexec COMMAND [COMMAND_ARG...]\n");
    return 1;
  }
  {
    const char * command_path = argv[1];
    char ** command_args = &argv[1];
    int ret;

    ret = fork();
    if(ret == -1) { ERR_EXIT("fork() failed"); }

    if(ret > 0) {
      // parent
      if (prctl(PR_SET_PDEATHSIG, SIGTERM) == -1) { ERR_EXIT("parent prctl() failed"); }
      return parent_main(ret);
    } else {
      // child
      if (prctl(PR_SET_PDEATHSIG, SIGKILL)   == -1) { ERR_EXIT("child prctl() failed"); }
      if (execvp(command_path, command_args) == -1) { ERR_EXIT("execvp() faied"); }
    }
  }
  return 0;
}

int parent_main(pid_t child_pid)
{
  sigset_t sigs;
  siginfo_t info;
  struct timespec poll_timeout = {0, 50 * 1000 * 1000};  // 50 milliseconds
  int ret;

  if (sigfillset(&sigs)                     == -1) { ERRMSG("sigfillset() failed");  goto kill_child; }
  if (sigprocmask(SIG_SETMASK, &sigs, NULL) == -1) { ERRMSG("sigprocmask() failed"); goto kill_child; }
  
  while ((ret = sigtimedwait(&sigs, &info, &poll_timeout)) == -1) {
    if (errno != EAGAIN || feof(stdin)) {
      break;
    }
  }

  if (ret == -1 || info.si_signo != SIGCHLD) {
    struct timespec wait_timeout = {1, 0}; // 1 second

    if (kill(child_pid, SIGTERM) == -1) { ERRMSG("kill() failed"); goto kill_child; }

    if (sigemptyset(&sigs) == -1)        { ERRMSG("sigemptyset() failed"); goto kill_child; }
    if (sigaddset(&sigs, SIGCHLD) == -1) { ERRMSG("sigaddset() failed");   goto kill_child; }

    if (sigtimedwait(&sigs, NULL, &wait_timeout) == -1) {
      if(errno != EAGAIN) { ERRMSG("sigtimedwait() failed"); }
      goto kill_child;
    }
  }
  goto wait_child_exit;
  
 kill_child:
  if (kill(child_pid, SIGKILL) == -1) { ERR_EXIT("kill() failed"); }
  
 wait_child_exit:
  {
    int status;
    if (waitpid(child_pid, &status, 0) == -1) { ERR_EXIT("waitpid() failed"); }
    if (WIFEXITED(status)) {
      return WEXITSTATUS(status);
    }
    if (WIFSIGNALED(status)) {
      return -WTERMSIG(status);
    }
    return 1;
  }
}
