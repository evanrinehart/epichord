#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(int argc, char* argv[]){

  printf("VID hello world\n");

  int eventPipe[2];
  int paintPipe[2];
  pid_t pid;
  char* args[] = {"cor", NULL};
  
  // in both of these pipes, we are pipe 0
  pipe(eventPipe);
  pipe(paintPipe);

  pid = fork();

  if(pid == -1){
    fprintf(stderr, "VID fork failed\n");
    fprintf(stderr, "VID' exec failed! %s\n", strerror(errno));
    exit(-1);
  }
  else if(pid == 0){ // child
    printf("VID' child rearranging file descriptors\n");
    close(eventPipe[0]);
    close(paintPipe[0]);
    close(0);
    dup(eventPipe[1]); // we will read event commands from stdin
    close(1);
    dup(paintPipe[1]); // we will write paint commands to stdout
    execve("cor", args, NULL);
    fprintf(stderr, "VID' exec failed! %s\n", strerror(errno));
    exit(-1);
  }
  else{ // parent
    close(eventPipe[1]);
    close(paintPipe[1]);
    int status;
    wait(&status);
    printf("VID child terminated with code %d\n", status);
  }

  return 0;
}
