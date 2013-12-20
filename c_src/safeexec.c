/**
 * Copyright 2013 Takeru Ohta <phjgt308@gmail.com>
 */

#include <stdio.h>
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/prctl.h>

int main(int argc, char ** argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: safeexec COMMAND COMMAND_ARGS...\n");
    return 1;
  }
  return 0;
}

/*
int main(int argc, char ** argv) {
  int ret;
  const char * child_path = argv[1];
  char ** child_args = &argv[1];

  ret = fork();
  if(ret == -1) {
    std::cerr << "[error] fork() failed: " << strerror(errno) << "(" << errno << ")" << std::endl;
    return 1;
  } else if(ret > 0) {
    prctl( PR_SET_PDEATHSIG, SIGTERM );

    timespec timeout = {1, 0};

    sigset_t sigs;
    sigfillset(&sigs);

    sigprocmask(SIG_SETMASK, &sigs, NULL);
    siginfo_t siginfo;
    // if(sigwaitinfo(&sigs, &siginfo) == -1) {
    //     std::cerr << "[error] sigwaitinfo() failed: " << strerror(errno) << "(" << errno << ")" << std::endl;
    //     return -1;
    // }
    while(sigtimedwait(&sigs, &siginfo, &timeout) == -1 && errno == EAGAIN) {
       // std::string n;
       // std::cin >> n;
       // std::cerr << "read: " << n << "(" << n.size() << ")" << std::endl;
      std::cin.peek();
       std::cerr << "check" << std::endl;
      if(std::cin.eof()) {
        std::cerr << "stdin is closed!" << std::endl;
        break;
      }
      if(std::cerr.eof()) {
        std::cerr << "stdout is closed!" << std::endl;
        break;
      }
    }
    std::cerr << "[signal] " << siginfo.si_signo << std::endl;
    
    if(siginfo.si_signo != SIGCHLD) {
      kill(ret, SIGTERM);

      sigtimedwait(&sigs, &siginfo, &timeout);
      if(siginfo.si_signo !=SIGCHLD) {
        kill(ret, SIGKILL);
      }
    }
    int status;
    waitpid(ret, &status, 0);
    std::cerr << "[finish] " << status << std::endl;
    return status;
  } else {
    prctl( PR_SET_PDEATHSIG, SIGKILL );
    if(execvp(child_path, child_args) == -1) {
      std::cerr << "[error] execvp() failed: " << errno << std::endl;
      return 1;
    }
  }
  return 0;
}
*/
