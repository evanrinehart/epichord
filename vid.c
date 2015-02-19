#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(int argc, char* argv[]){

  printf("VID hello world\n");

  int fd[2];
  pid_t pid;
  int status;
  char* args[] = {"snd", NULL};
  
  pipe(fd);
  pid = fork();
  if(pid == -1){
    fprintf(stderr, "VID fork failed\n");
    exit(-1);
  }
  else if(pid == 0){ // child
    printf("VID' child rearranging file descriptors\n");
    close(0);
    close(fd[1]);
    dup(fd[0]);
    execve("cor", args, NULL);
    fprintf(stderr, "VID' exec failed! %s\n", strerror(errno));
    exit(-1);
  }
  else{ // parent
    close(fd[0]);
    printf("VID parent waiting\n");
    dprintf(fd[1], "yooooo\n");
    //close(fd[1]);
    wait(&status);
    printf("VID child terminated with code %d\n", status);
  }

  return 0;
}
