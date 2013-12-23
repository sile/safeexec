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

#define BUF_SIZE 8092

struct PARENT_STATE {
  int epfd;
  pid_t child_pid;
  char buf[BUF_SIZE];
  uint32_t buf_start;
  uint32_t buf_end;
};

enum HANDLE_RESULT {
  KILL_CHILD,
  WAIT_CHILD_EXIT,
  CONTINUE
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
    int pipefd[2];
    if (pipe(pipefd) == -1) { ERR_EXIT("pipe() failed"); }

    ret = fork();
    if (ret == -1) { ERR_EXIT("fork() failed"); }

    if(ret > 0) {
      // parent
      if (prctl(PR_SET_PDEATHSIG, SIGTERM) == -1) { ERR_EXIT("parent prctl() failed"); }
      if (dup2(pipefd[1], STDOUT_FILENO) == -1)   { ERR_EXIT("parent dup2() failed"); }
      if (close(pipefd[1]) == -1)                 { ERR_EXIT("paretn close() failed"); }
      return parent_main(ret);
    } else {
      // child
      if (prctl(PR_SET_PDEATHSIG, SIGKILL) == -1) { ERR_EXIT("child prctl() failed"); }
      if (dup2(pipefd[0], STDIN_FILENO) == -1) { ERR_EXIT("child dup2() failed"); }
      if (close(pipefd[0]) == -1) { ERR_EXIT("child dup2() failed"); }

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

int epoll_mod(int epfd, int fd, uint32_t events) {
  struct epoll_event ev;
  ev.events  = events;
  ev.data.fd = fd;
  return epoll_ctl(epfd, EPOLL_CTL_MOD, fd, &ev);
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

enum HANDLE_RESULT handle_signal(int fd, uint32_t events, struct PARENT_STATE * state) {
  siginfo_t info;
  if (read(fd, &info, sizeof(info)) == -1) { ERRMSG("read() failed"); return KILL_CHILD; }
  if (info.si_signo != SIGCHLD) {
    if (kill_and_wait_sigchild(state->child_pid, SIGTERM, 1) == -1) {
      return KILL_CHILD;
    }
  }
  return WAIT_CHILD_EXIT;
}

enum HANDLE_RESULT handle_read(int fd, uint32_t events, struct PARENT_STATE * state) {
  if (events & EPOLLIN) {
    int ret = read(fd, state->buf + state->buf_end, BUF_SIZE - state->buf_end);
    if (ret == -1) { ERRMSG("read() failed"); return KILL_CHILD; }
    state->buf_end += ret;

    if (epoll_mod(state->epfd, STDIN_FILENO, 0)         == -1) { ERRMSG("epoll_mod() failed"); return KILL_CHILD; }
    if (epoll_mod(state->epfd, STDOUT_FILENO, EPOLLOUT) == -1) { ERRMSG("epoll_mod() failed"); return KILL_CHILD; }
  }
  if (events & (EPOLLERR | EPOLLHUP)) {
    if(state->buf_end == 0) {
      if (close(STDOUT_FILENO) == -1) { ERRMSG("close() failed"); return KILL_CHILD; }
      if (kill_and_wait_sigchild(state->child_pid, SIGPIPE, 1) == -1) {
        return KILL_CHILD;
      }
      return WAIT_CHILD_EXIT;
    }
  }
  return CONTINUE;
}

enum HANDLE_RESULT handle_write(int fd, uint32_t events, struct PARENT_STATE * state) {
  if (events & EPOLLOUT) {
    int ret = write(fd, state->buf + state->buf_start, state->buf_end - state->buf_start);
    if (ret == -1) { ERRMSG("write() failed"); return KILL_CHILD; }

    state->buf_start += ret;
    if (state->buf_start == state->buf_end) {
      state->buf_start = state->buf_end = 0;
      if (epoll_mod(state->epfd, STDOUT_FILENO, 0)      == -1) { ERRMSG("epoll_mod() failed"); return KILL_CHILD; }
      if (epoll_mod(state->epfd, STDIN_FILENO, EPOLLIN) == -1) { ERRMSG("epoll_mod() failed"); return KILL_CHILD; }
    }
  }
  return CONTINUE;
}

int parent_main(pid_t child_pid) {
  struct PARENT_STATE state = {0};
  int sigfd;
  int status;
  sigset_t sigs;

  if (sigfillset(&sigs)                     == -1) { ERRMSG("sigfillset() failed");  goto kill_child; }
  if (sigprocmask(SIG_SETMASK, &sigs, NULL) == -1) { ERRMSG("sigprocmask() failed"); goto kill_child; }
  if ((sigfd = signalfd(-1, &sigs, 0))      == -1) { ERRMSG("signalfd() failed");    goto kill_child; }

  if ((state.epfd = epoll_create(1))               == -1) { ERRMSG("epoll_create() failed"); goto kill_child; }
  if (epoll_add(state.epfd, sigfd, EPOLLIN)        == -1) { ERRMSG("epoll_add() failed"); goto kill_child; }
  if (epoll_add(state.epfd, STDIN_FILENO, EPOLLIN) == -1) { ERRMSG("epoll_add() failed"); goto kill_child; }
  if (epoll_add(state.epfd, STDOUT_FILENO, 0)      == -1) { ERRMSG("epoll_add() failed"); goto kill_child; }

  for (;;) {
    int i;
    enum HANDLE_RESULT handle_ret;
    struct epoll_event e[3];
    int nfd = epoll_wait(state.epfd, e, sizeof(e) / sizeof(struct epoll_event), -1);
    if (nfd == -1) { ERRMSG("epoll_wait() failed"); goto kill_child; }

    for (i = 0; i < nfd; i++) {
      int fd = e[i].data.fd;
      if      (fd == sigfd)        { handle_ret = handle_signal(fd, e[i].events, &state); }
      else if (fd == STDIN_FILENO) { handle_ret = handle_read  (fd, e[i].events, &state); }
      else                         { handle_ret = handle_write (fd, e[i].events, &state); }
      
      switch(handle_ret) {
      case KILL_CHILD:      goto kill_child;
      case WAIT_CHILD_EXIT: goto wait_child_exit;
      case CONTINUE:        break;
      }
    }
  }
  
 kill_child:
  if (kill(state.child_pid, SIGKILL) == -1) { ERR_EXIT("kill() failed"); }
  
 wait_child_exit:
  if (waitpid(state.child_pid, &status, 0) == -1) { ERR_EXIT("waitpid() failed"); }
  if (WIFEXITED(status))   { return WEXITSTATUS(status); }
  if (WIFSIGNALED(status)) { return WTERMSIG(status) + 128; }
  return 1;
}
