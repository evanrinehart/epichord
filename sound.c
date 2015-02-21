#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

/*
commands and requests we might get
LOAD path/to/data
PLAY
STOP
SEEK position
LOOP_POINT position1 position2
LOOP_ENABLE
LOOP_DISABLE 
SET_PARAMETERS
TELL (report current position in samples)
CAPTURE_ENABLE
CAPTURE_DISABLE
CAPTURE_DUMP
*/

/* input analysis buffer */

#define INBUFSIZE 256

int currentPosition;

void stdinWorker(){
  char buf[INBUFSIZE];
  char command[INBUFSIZE];
  char arg1[INBUFSIZE];
  char arg2[INBUFSIZE];

  fgets(buf, INBUFSIZE, stdin);
  if(ferror(stdin)){
    fprintf(stderr, "SOUND error while reading stdin. <%s>\n", strerror(errno));
    exit(-1);
  }
  if(feof(stdin)){
    fprintf(stderr, "SOUND stdin is EOF. Terminating\n");
    exit(0);
  }

  fprintf(stderr, "SOUND i heard %s (%zu)\n", buf, strlen(buf));

  sscanf(buf, "%s", command);
  if(strcmp(command, "LOAD")==0){
    sscanf("%s %s", command, arg1);
    fprintf(stderr, "loading %s\n", arg1);
    // load the sequence from the file and place it in a new buffer.
    // append the buffer to the global set of buffers.
    // if there there is no room, unload the oldest buffer which probably
    // isn't in use. this can be guaranteed by sleeping a short time before
    // taking the next request.
    // set the "killall flag" to silence active notes.
    // set the current sequence to the new sequence.
  }
  else if(strcmp(command, "PLAY")==0){
    fprintf(stderr, "play\n");
    // enable the playback flag
  }
  else if(strcmp(command, "STOP")==0){
    fprintf(stderr, "stop\n");
    // killall and disable playback
  }
  else if(strcmp(command, "SEEK")==0){
    // killall, set the seek flag and position
  }
  else if(strcmp(command, "LOOP_POINT")==0){
    // set the loop points
  }
  else if(strcmp(command, "LOOP_ENABLE")==0){
    fprintf(stderr, "loop enable\n");
    // enable the loop flag
  }
  else if(strcmp(command, "LOOP_DISABLE")==0){
    fprintf(stderr, "loop disable\n");
    // disable the loop flag
  }
  else if(strcmp(command, "SET_PARAMETERS")==0){
    // set the new params and set the parameter change flag 
  }
  else if(strcmp(command, "TELL")==0){
    fprintf(stderr, "something wants me to tell\n");
    printf("%d\n", currentPosition);
  }
  else if(strcmp(command, "CAPTURE_ENABLE")==0){
    fprintf(stderr, "capture enable\n");
  }
  else if(strcmp(command, "CAPTURE_DISABLE")==0){
    fprintf(stderr, "capture disable\n");
  }
  else if(strcmp(command, "CAPTURE_DUMP")==0){
    fprintf(stderr, "dump capture buffer\n");
    printf("nothing to see here!\n");
  }
  else{
    fprintf(stderr, "SOUND unrecognized request <%s>\n", buf);
  }
}

void audioCallback(){
  fprintf(stderr, "SOUND audio chunk requested\n");
}


/** the capture buffer **/

#define CAPTURE_SIZE 65536
char captureBuf[CAPTURE_SIZE];
int captureRead = 0;
int captureWrite = 0;
/* copy as much out of capture buffer as possible, return count copied */
int ringRead(char* dest){
  int ptr = captureWrite;
  int count;
  int count1;
  int count2;
  if(captureRead == ptr){ // buffer empty
    return 0;
  }
  else if(captureRead < ptr) { // no need to wrap
    count = ptr - captureRead;
    memcpy(dest, captureBuf + captureRead, count);
    captureRead = ptr;
    return count;
  }
  else { // need to copy twice to account for wrap
    count1 = CAPTURE_SIZE - captureRead;
    count2 = ptr;
    memcpy(dest, captureBuf + captureRead, count1);
    memcpy(dest+count1, captureBuf, count2);
    captureRead = ptr;
    return count1 + count2;
  }
}

/* dump bytes into capture buffer. 0 if successful, 1 if not enough room */
int ringWrite(char* src, int count){
  int wall = captureRead;
  int population = captureWrite >= wall
    ? captureWrite - wall
    : captureWrite + (CAPTURE_SIZE - wall);
  int count1;
  int count2;
  if(population + count > CAPTURE_SIZE - 1){ // not enough room
    return 1;
  }
  else if(captureWrite + count < CAPTURE_SIZE){ // normal copy
    memcpy(captureBuf + captureWrite, src, count);
    captureWrite += count;
    return 0;
  }
  else{ // need to copy twice to account for wrap
    count1 = CAPTURE_SIZE - captureWrite;
    count2 = count - count1;
    memcpy(captureBuf + captureWrite, src, count1);
    if(count2 > 0) memcpy(captureBuf, src + count1, count2);
    captureWrite = (captureWrite + count) % CAPTURE_SIZE;
    return 0;
  }
}

/* end capture buffer */

int setupCoreMidi(){
  return -1;
}

int main(int argc, char* argv[]){
  fprintf(stderr, "SOUND Hello World\n");
  if(setupCoreMidi()){
    fprintf(stderr, "SOUND CoreMidi setup failed.\n");
    exit(-1);
  }
  for(;;) stdinWorker();
  fprintf(stderr, "SOUND impossible -2\n");
  return -2;
}
