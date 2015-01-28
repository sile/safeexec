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

#define ERRMSG(Message) fprintf(stderr, "[error:%d] " Message ": pid=%d, ppid=%d, error=%s(%d)\n", __LINE__, getpid(), getppid(), strerror(errno), errno)
#define ERR_EXIT(Message) {ERRMSG(Message); exit(1);}

enum HANDLE_RESULT {
  KILL_CHILD,
  WAIT_CHILD_EXIT
};

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
    if (ret == -1) { ERR_EXIT("fork() failed"); }

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
