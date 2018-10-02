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
#include <sys/epoll.h>
#include <sys/signalfd.h>
#include <stdarg.h>
#define __USE_GNU
#define _GNU_SOURCE
#include <sched.h>

#define ERRMSGF(P) errmsgf P
static void errmsgf(char *str, ...)
{
  va_list ap;
  va_start(ap, str);
  fprintf(stderr, "%s:%d [error] ", __FILE__, __LINE__);
  vfprintf(stderr, str, ap);
  fprintf(stderr, ": pid=%d, ppid=%d, error=%s(%d)\n", getpid(), getppid(), strerror(errno), errno);
  va_end(ap);
}
#define ERRMSG(Message) ERRMSGF((Message))
#define ERRF_EXIT(Arg) {ERRMSGF(Arg); exit(1);}
#define ERR_EXIT(Message) ERRF_EXIT((Message))

enum HANDLE_RESULT {
  KILL_CHILD,
  WAIT_CHILD_EXIT
};

int parent_main(pid_t child_pid);
int set_affinities(pid_t pid, long cpuids);

int main(int argc, char ** argv)
{
  if (argc < 2) {
    fprintf(stderr, "Usage: safeexec [OPTIONS] COMMAND [COMMAND_ARG...]\n");
    fprintf(stderr, "Options\n");
    fprintf(stderr, "  --cpu CPU_MASK : Run process on the set of CPUs be eligible to run.\n");
    fprintf(stderr, "                   CPU_MASK determines the set of CPUs be eligible to run and is specified by hex(e.g. 0x01).\n");
    return 1;
  }
  {
    long cpuids = -1;
    
    argc--;
    argv++;
    
    // process option
    while(argc > 1){
        // cpuオプション
        if(strcmp(argv[0], "--cpu") == 0){
            if(argc < 2){
                ERR_EXIT("--cpu option needs CPU_MASK");
            }
            char *endptr = NULL;
            cpuids = strtol(argv[1], &endptr, 16);
            if(*endptr != '\0'){
                fprintf(stderr, "CPU_MASK must be specified by hex(e.g. 0x01)\n");
                return 1;
            }
            argc -= 2;
            argv += 2;
        }
        else{
            break;
        }
    }

    if(argc < 1){
        ERR_EXIT("COMMAND path is not found");
    }
      
    const char * command_path = argv[0];
    char ** command_args = &argv[0];
    int ret;
    int ppid = getpid();

    ret = fork();
    if (ret == -1) { ERR_EXIT("fork() failed"); }

    if(ret > 0) {
      // parent
      if (prctl(PR_SET_PDEATHSIG, SIGTERM) == -1) { ERR_EXIT("parent prctl() failed"); }
      return parent_main(ret);
    } else {
      // child
      if (prctl(PR_SET_PDEATHSIG, SIGKILL)   == -1) { ERR_EXIT("child prctl() failed"); }
      if (ppid != getppid()) { ERRF_EXIT(("parent process(%d) has been dead", ppid)); }
      if (set_affinities(getpid(), cpuids) == -1) { ERR_EXIT("set_affinity() failed"); }
      if (execvp(command_path, command_args) == -1) { ERRF_EXIT(("execvp() failed [command = %s]", command_path)); };
    }
  }
  return 0;
}

int set_affinities(pid_t pid, long cpuids)
{
    int i = 0;
    cpu_set_t set;
    CPU_ZERO(&set);
    const int bit_size = sizeof(cpuids) * 8;
    for (i = 0; i < bit_size; ++i) {
        if ((1l << i) & cpuids) {
            cpu_set_t tmp;
            CPU_ZERO(&tmp);
            CPU_SET(i, &tmp);
            CPU_OR(&set, &tmp, &set);
        }
    }
    return sched_setaffinity(pid, sizeof(set), &set);
}

int epoll_add(int epfd, int fd, uint32_t events) {
  struct epoll_event ev;
  ev.events  = events;
  ev.data.fd = fd;
  return epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &ev);
}

int kill_and_wait_sigchild(pid_t child_pid, int signal, uint32_t timeout_seconds) {
  sigset_t sigs;
  struct timespec wait_timeout = {timeout_seconds, 0};
  if (kill(child_pid, signal) == -1)   { ERRMSG("kill() failed");        return -1; }
  if (sigemptyset(&sigs) == -1)        { ERRMSG("sigemptyset() failed"); return -1; }
  if (sigaddset(&sigs, SIGCHLD) == -1) { ERRMSG("sigaddset() failed");   return -1; }
  if (sigtimedwait(&sigs, NULL, &wait_timeout) == -1) {
    if(errno != EAGAIN) { ERRMSG("sigtimedwait() failed"); }
    return -1;
  }
  return 0;
}

enum HANDLE_RESULT handle_signal(int fd, pid_t child_pid) {
  siginfo_t info;
  if (read(fd, &info, sizeof(info)) == -1) { ERRMSG("read() failed"); return KILL_CHILD; }
  if (info.si_signo != SIGCHLD) {
    if (kill_and_wait_sigchild(child_pid, SIGTERM, 1) == -1) {
      return KILL_CHILD;
    }
  }
  return WAIT_CHILD_EXIT;
}

enum HANDLE_RESULT handle_in_eof(int fd, pid_t child_pid) {
  if (kill_and_wait_sigchild(child_pid, SIGTERM, 1) == -1) {
    return KILL_CHILD;
  }
  return WAIT_CHILD_EXIT;
}

int parent_main(pid_t child_pid) {
  int epfd;
  int sigfd;
  int status;
  sigset_t sigs;

  if (sigfillset(&sigs)                           == -1) { ERRMSG("sigfillset() failed");  goto kill_child; }
  if (sigprocmask(SIG_SETMASK, &sigs, NULL)       == -1) { ERRMSG("sigprocmask() failed"); goto kill_child; }
  if ((sigfd = signalfd(-1, &sigs, SFD_NONBLOCK)) == -1) { ERRMSG("signalfd() failed");    goto kill_child; }

  if ((epfd = epoll_create(1))         == -1) { ERRMSG("epoll_create() failed"); goto kill_child; }
  if (epoll_add(epfd, sigfd, EPOLLIN)  == -1) { ERRMSG("epoll_add() failed"); goto kill_child; }
  if (epoll_add(epfd, STDIN_FILENO, 0) == -1) { ERRMSG("epoll_add() failed"); goto kill_child; }

  for (;;) {
    int i;
    enum HANDLE_RESULT handle_ret;
    struct epoll_event e[3];
    int nfd = epoll_wait(epfd, e, sizeof(e) / sizeof(struct epoll_event), 100);
    if (nfd == -1) { ERRMSG("epoll_wait() failed"); goto kill_child; }
    if (nfd == 0) {
      int ret = waitpid(child_pid, &status, WNOHANG);
      if (ret == -1) { ERRMSG("waitpid() failed");  goto kill_child; }
      if (ret != 0)  { goto child_exited; }
      continue;
    }

    for (i = 0; i < nfd; i++) {
      int fd = e[i].data.fd;
      if (fd == sigfd) { handle_ret = handle_signal(fd, child_pid); }
      else             { handle_ret = handle_in_eof(fd, child_pid); }

      switch(handle_ret) {
      case KILL_CHILD:      goto kill_child;
      case WAIT_CHILD_EXIT: goto wait_child_exit;
      }
    }
  }

 kill_child:
  if (kill(child_pid, SIGKILL) == -1) { ERR_EXIT("kill() failed"); }

 wait_child_exit:
  if (waitpid(child_pid, &status, 0) == -1) { ERR_EXIT("waitpid() failed"); }

 child_exited:
  if (WIFEXITED(status))   { return WEXITSTATUS(status); }
  if (WIFSIGNALED(status)) { return WTERMSIG(status) + 128; }
  return 1;
}
