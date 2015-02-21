#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(int argc, char* argv[]){

  char buf[256];
  printf("SND hello world\n");
  fgets(buf, 256, stdin);
  printf("SND i heard %s (%zu)\n", buf, strlen(buf));
  if(feof(stdin)){
    printf("SND stdin is EOF\n");
  }
  else{
    sleep(5);
  }

  return 0;
}
