/**
 * Copyright 2015 Takeru Ohta <phjgt308@gmail.com>
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/event.h>
#include <stdarg.h>

#define ERRMSGF(P) errmsgf P
static void errmsgf(char *str, ...)
{
  va_list ap;
  va_start(ap, str);
  fprintf(stderr, "error[%s:%d] ", __FILE__, __LINE__);
  vfprintf(stderr, str, ap);
  fprintf(stderr, ": pid=%d, ppid=%d, error=%s(%d)\n", getpid(), getppid(), strerror(errno), errno);
  va_end(ap);
}
#define ERRMSG(Message) ERRMSGF((Message))
#define ERRF_EXIT(Param) {ERRMSGF(Param); exit(1);}
#define ERR_EXIT(Message) ERRF_EXIT((Message))

int sigisemptyset(const sigset_t * sigs) {
  // inefficient and incorrect
  const int minsig = 0;
  const int maxsig = 128;
  for (int i=minsig; i < maxsig; i++) {
    if (sigismember(sigs, i)) {
      return 0;
    }
  }
  return 1;
}

enum HANDLE_RESULT {
  KILL_CHILD,
  WAIT_CHILD_EXIT,
  IGNORE
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
      return parent_main(ret);
    } else {
      // child
      // XXX: When the parent process is killed by SIGKILL signal, the child process will be still alive
      if (execvp(command_path, command_args) == -1) { ERRF_EXIT(("execvp() failed(command=%s)", command_path)); };
    }
  }
  return 0;
}

int poll_add(int kq, int fd, int filter) {
  struct kevent kev;
  EV_SET(&kev, fd, filter, EV_ADD|EV_ONESHOT, 0, 0, NULL);
  return kevent(kq, &kev, 1, NULL, 0, NULL);
}

int kill_and_wait_sigchild(pid_t child_pid, int signal, uint32_t timeout_seconds) {
  if (kill(child_pid, signal) == -1)   { ERRMSG("kill() failed"); return -1; }

  int kq;
  if ((kq  = kqueue()) == -1) { ERRMSG("kqueue() failed"); return -1; }
  if (poll_add(kq, SIGCHLD, EVFILT_SIGNAL) == -1) { ERRMSG("poll_add() failed"); close(kq); return -1; }

  struct timespec timeout;
  timeout.tv_sec = timeout_seconds;
  timeout.tv_nsec = 0;
  if (kevent(kq, NULL, 0, NULL, 0, &timeout) == -1) { ERRMSG("kevent() failed"); close(kq); return -1; }
  close(kq);
  return 0;
}

enum HANDLE_RESULT handle_signal(const sigset_t * sigs, pid_t child_pid) {
  if (sigisemptyset(sigs)) {
    return IGNORE;
  }
  if (! sigismember(sigs, SIGCHLD)) {
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
  int kq;
  int status;
  sigset_t sigs;

  if (sigfillset(&sigs)                        == -1) { ERRMSG("sigfillset() failed");  goto kill_child; }
  if (sigprocmask(SIG_SETMASK, &sigs, NULL)    == -1) { ERRMSG("sigprocmask() failed"); goto kill_child; }
  if ((kq = kqueue())                          == -1) { ERRMSG("kqueue() failed"); goto kill_child; }
  if (poll_add(kq, STDIN_FILENO, EVFILT_READ)  == -1) { ERRMSG("poll_add() failed"); goto kill_child; }

  for (;;) {
    int i;
    enum HANDLE_RESULT handle_ret = IGNORE;
    struct kevent e[2];
    struct timespec timeout;
    timeout.tv_sec = 0;
    timeout.tv_nsec = 100 * 1000 * 1000; // 100ms

    int nfd = kevent(kq, NULL, 0, e, sizeof(e) / sizeof(struct kevent), &timeout);
    if (nfd == -1) { ERRMSG("kevent() failed"); goto kill_child; }
    if (nfd == 0) {
      int ret = waitpid(child_pid, &status, WNOHANG);
      if (ret == -1) { ERRMSG("waitpid() failed");  goto kill_child; }
      if (ret != 0)  { goto child_exited; }

      if (sigpending(&sigs) == -1) { ERRMSG("sigpending() failed"); goto kill_child; }
      handle_ret = handle_signal(&sigs, child_pid);
    }

    for (i = 0; i < nfd; i++) {
      int fd = e[i].ident;
      if (fd == STDIN_FILENO) {
        if (e[i].flags & EV_EOF) {
          handle_ret = handle_in_eof(fd, child_pid);
        }
      }
    }

    switch(handle_ret) {
    case KILL_CHILD:      goto kill_child;
    case WAIT_CHILD_EXIT: goto wait_child_exit;
    case IGNORE:          break;
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
