#include <CoreFoundation/CoreFoundation.h>
#include <CoreMidi/CoreMidi.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <inttypes.h>
#include <pthread.h>
#include <mach/mach.h>
#include <mach/mach_time.h>

#define FRAME_SIZE_NS 20000000
#define PLAYING_MAX 1024
#define INBUF_SIZE 1024
#define PACKET_LIST_SIZE 4096
#define DEFAULT_USPQ 500000 // 120 bpm

struct sequencerEvent {
  uint32_t tick;
  uint64_t atNs;
  uint8_t typeChan;
  uint8_t arg1;
  uint8_t arg2;
};

struct tempoChange {
  uint32_t tick;
  uint64_t atNs;
  uint32_t uspq; // microseconds per quarter note
};

struct playingNote {
  unsigned char playing : 1;
  unsigned char channel : 4;
  unsigned char note : 7;
};

MIDIClientRef client;
MIDIEndpointRef inputPort;
MIDIEndpointRef outputPort;
pthread_t dispatchThread;

int playFlag = 0;
uint64_t absolutePlayHeadNs;
uint64_t absoluteLeadingEdgeNs;
uint64_t absoluteSongStartNs;
uint64_t songNs = 0;

struct sequencerEvent* events;
int eventCount;

struct tempoChange* tempoChanges;
int tempoChangeCount = 0;

uint32_t ticksPerBeat = 384;

int onlineSeekFlag = 0;
uint64_t onlineSeekTargetNs;

double getCurrentBeat(){
  uint32_t uspq;
  uint32_t baseNs;
  uint64_t songNsSnap = songNs;
  int i;
  if(tempoChangeCount == 0 || songNsSnap < tempoChanges[i].atNs){
    uspq = DEFAULT_USPQ;
    baseNs = 0;
  }
  else{
    for(i=0; i<tempoChangeCount-1 && songNsSnap > tempoChanges[i].atNs; i++);
    uspq = tempoChanges[i].uspq;
    baseNs = tempoChanges[i].atNs;
  }
  return (songNsSnap - baseNs)/(1000.0 * uspq);
}

void executeSeek(int number, int numerator, int denominator){
  double beat = number + (double)numerator / denominator;
  uint32_t targetTick = beat * ticksPerBeat;
  uint64_t targetNs;
  uint32_t uspq;
  uint32_t baseTick;
  int i;
  printf("execute seek %d %d %d\n", number, numerator, denominator);
  if(tempoChangeCount == 0 || targetTick < tempoChanges[i].tick){
    uspq = DEFAULT_USPQ;
    baseTick = 0;
  }
  else{
    for(i=0; i<tempoChangeCount-1 && targetTick > tempoChanges[i].tick; i++);
    uspq = tempoChanges[i].uspq;
    baseTick = tempoChanges[i].tick;
  }
  targetNs = (targetTick-baseTick)*1000.0*uspq/ticksPerBeat;
  printf("targetNs = %llu\n", targetNs);
  if(playFlag == 0){
    songNs = targetNs;
  }
  else{
    onlineSeekFlag = 1;
    onlineSeekTargetNs = targetNs;
    usleep(FRAME_SIZE_NS/1000);
  }
}

struct playingNote playingNotes[PLAYING_MAX];
int playingCount = 0;

void initPlayingNotes(){
  int i;
  for(i=0; i<PLAYING_MAX; i++){
    playingNotes[i].playing = 0;
  }
}

void rememberNoteOn(int channel, int note){
  int i;
  for(i=0; i<PLAYING_MAX && playingNotes[i].playing==1; i++);
  if(i >= PLAYING_MAX){
    fprintf(stderr, "** SOUND remembering too many on-notes\n");
    exit(-1);
  }
  playingNotes[i].playing = 1;
  playingNotes[i].channel = channel;
  playingNotes[i].note = note;
  playingCount++;
}

void forgetNoteOn(int channel, int note){
  int i = 0;
  int count = 0;
  for(;;){
    if(i >= PLAYING_MAX) break;
    if(count >= playingCount) break;
    if(playingNotes[i].playing == 1){
      count++;
      if(playingNotes[i].channel == channel && playingNotes[i].note == note){
        playingNotes[i].playing = 0;
        playingCount--;
        break;
      }
    }
    i++;
  }
}

// cut all playing notes
void killAll(){
  unsigned char packetListStorage[PACKET_LIST_SIZE];
  MIDIPacketList* packetList = (MIDIPacketList*) packetListStorage;
  MIDIPacket* packet;
  unsigned char midi[3];
  uint64_t timeOfCut = absoluteLeadingEdgeNs;
  int count = 0;
  int i = 0;

  packet = MIDIPacketListInit(packetList);
  for(;;){
    if(count >= playingCount) break;
    if(i >= PLAYING_MAX) break;
    if(playingNotes[i].playing){
      midi[0] = 0x80 | playingNotes[i].channel;
      midi[1] = playingNotes[i].note;
      midi[2] = 0;
      packet = MIDIPacketListAdd(
        packetList,
        PACKET_LIST_SIZE,
        packet,
        timeOfCut,
        3,
        midi
      );
      if(packet == NULL){
        fprintf(stderr, "** SOUND unable to MIDIPacketListAdd (cut all)\n");
        exit(-1);
      }
      playingNotes[i].playing = 0;
      count++;
    }
    i++;
  }
  playingCount = 0;
  MIDIReceived(outputPort, packetList);
}





/** the capture buffer **/
/*
#define CAPTURE_SIZE 65536
char captureBuf[CAPTURE_SIZE];
int captureRead = 0;
int captureWrite = 0;
// copy as much out of capture buffer as possible, return count copied
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

//dump bytes into capture buffer. 0 if successful, 1 if not enough room
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
*/

void midiNotification(const MIDINotification* message, void* refCon){
  fprintf(stderr, "midiNotification\n");
}

void captureWorker(const MIDIPacketList* pktlist, void* refCon, void* srcConn){
  fprintf(stderr, "captureWorker\n");
}

int setupCoreMidi(){
  OSStatus status;

  /* creating a client */
  status = MIDIClientCreate(
    CFStringCreateWithCString(NULL, "My Midi Client", kCFStringEncodingASCII),
    midiNotification, NULL, &client
  );
  if(status != noErr){
    fprintf(stderr, "core midi client create error %d\n", status);
    exit(-1);
  }

  status = MIDIDestinationCreate(
    client,
    CFStringCreateWithCString(NULL,"Epichord Capture",kCFStringEncodingASCII),
    captureWorker,
    NULL,
    &inputPort
  );
  if(status != noErr){
    printf("error creating destination %d\n", status);
    exit(-1);
  }

  /* TO ADVERTISE AN INPUT PORT... CREATE A DESTINATION ENDPOINT */
  /* TO ADVERTISE AN OUTPUT PORT... CREATE A SOURCE ENDPOINT */
  /* TO SEND TO WHATEVER CONNECTED TO OUR OUTPUT USE MIDIRECEIVED */
  /* MIDI THAT COMES IN ON OUR OUTPUT TRIGGERS THE CAPTURE CALLBACK */

  status = MIDISourceCreate(
    client,
    CFStringCreateWithCString(NULL,"Epichord Output",kCFStringEncodingASCII),
    &outputPort
  );
  if(status != noErr){
    printf("error creating source %d\n", status);
    exit(-1);
  }

  return 0;
}

struct tempoChange* loadTempoChangeData(FILE* tempoFile, int* count){
  int tempoMax = 32;
  int tempoPtr = 0;
  int bytesRead;
  unsigned char seven[7];
  struct tempoChange* tempoBuf = malloc(tempoMax * sizeof(struct tempoChange));
  if(tempoBuf == NULL){
    fprintf(stderr, "** SOUND malloc of tempoBuf failed\n");
    exit(-1);
  }
  for(;;){
    bytesRead = fread(seven, 1, 7, tempoFile);
    if(bytesRead == 0) break;
    if(bytesRead != 7) {
      fprintf(stderr, "** SOUND tempo data file ends with %d bytes not 7\n", bytesRead);
      exit(-1);
    }
    tempoBuf[tempoPtr].tick = seven[0]<<24 | seven[1]<<16 | seven[2]<<8 | seven[3];
    tempoBuf[tempoPtr].uspq = seven[4]<<16 | seven[5]<<8  | seven[6];
    fprintf(stderr, "%u %u %llu\n",
      tempoBuf[tempoPtr].tick, tempoBuf[tempoPtr].uspq, tempoBuf[tempoPtr].atNs);
    tempoPtr++;
    if(tempoPtr == tempoMax){
      tempoBuf = realloc(tempoBuf, tempoMax*2*sizeof(struct tempoChange));
      if(tempoBuf == NULL){
        fprintf(stderr, "** SOUND failed to realloc tempo buffer\n");
        exit(-1);
      }
      tempoMax *= 2;
    }
  }
  *count = tempoPtr;
  return tempoBuf;
}

struct sequencerEvent* loadSequenceData(FILE* sequenceFile, int* count){
  int eventMax = 256;
  int eventPtr = 0;
  int bytesRead;
  unsigned char seven[7];
  struct sequencerEvent* eventBuf = malloc(eventMax * sizeof(struct sequencerEvent));
  if(eventBuf == NULL){
    fprintf(stderr, "** SOUND malloc of eventBuf failed\n");
    exit(-1);
  }
  for(;;){
    bytesRead = fread(seven, 1, 7, sequenceFile);
    if(bytesRead == 0) break;
    if(bytesRead != 7) {
      fprintf(stderr,
        "** SOUND sequence data file ends with %d bytes not 7\n", bytesRead);
      exit(-1);
    }
    eventBuf[eventPtr].tick = seven[0]<<24 | seven[1]<<16 | seven[2]<<8 | seven[3];
    eventBuf[eventPtr].typeChan = seven[4];
    eventBuf[eventPtr].arg1 = seven[5];
    eventBuf[eventPtr].arg2 = seven[6];
    fprintf(stderr, "%u %x %u %u\n",
      eventBuf[eventPtr].tick,
      eventBuf[eventPtr].typeChan,
      eventBuf[eventPtr].arg1,
      eventBuf[eventPtr].arg2
    );
    eventPtr++;
    if(eventPtr == eventMax){
      eventBuf = realloc(eventBuf, eventMax*2*sizeof(struct sequencerEvent));
      if(eventBuf == NULL){
        fprintf(stderr, "** SOUND failed to realloc event buffer\n");
        exit(-1);
      }
      eventMax *= 2;
    }
  }
  *count = eventPtr;
  return eventBuf;
}


void recomputeEventTimes(
  struct sequencerEvent* events,
  int eventCount,
  struct tempoChange* tempoChanges,
  int tempoCount,
  uint32_t ticksPerBeat
){
  int i, j;
  uint32_t default_uspq = DEFAULT_USPQ;
  uint32_t uspq = default_uspq;
  uint64_t prevNs = 0;
  uint32_t prevTick = 0;
  uint32_t deltaTicks;
  for(i=0, j=0; i < tempoCount; i++){
    printf("i=%d j=%d prevtick=%u uspq=%u\n", i, j, prevTick, uspq);
    deltaTicks = tempoChanges[i].tick - prevTick;
    tempoChanges[i].atNs = prevNs + deltaTicks*1000.0*uspq / ticksPerBeat;

    printf("tempo at ns %llu\n", tempoChanges[i].atNs);
    for(;;){
      if(j >= eventCount) break;
      deltaTicks = events[j].tick - prevTick;
      events[j].atNs = prevNs + deltaTicks*1000.0*uspq / ticksPerBeat;
      printf("event %d tick=%u at %llu / %llu\n", j, events[j].tick, events[j].atNs, tempoChanges[i].atNs);
      if(events[j].atNs <= tempoChanges[i].atNs) j++;
      else break;
    }

    prevTick = tempoChanges[i].tick;
    prevNs = tempoChanges[i].atNs;
    uspq = tempoChanges[i].uspq;
  }

  for(;;){
    if(j >= eventCount) break;
    deltaTicks = events[j].tick - prevTick;
    events[j].atNs = prevNs + deltaTicks*1000.0*uspq / ticksPerBeat;
    printf("event %d tick=%u at %llu / %llu\n", j, events[j].tick, events[j].atNs, tempoChanges[i].atNs);
    j++;
  }

}

// load raw sequence and tempo data from two files, then delete the files
void loadData(char* sequencePath, char* tempoPath){
  FILE* tempoFile;
  FILE* sequenceFile;
  tempoFile = fopen(tempoPath, "r");
  if(tempoFile == NULL){
    fprintf(stderr,
      "SOUND failed to open tempo file: %s %s\n", tempoPath, strerror(errno));
    exit(-1);
  }
  tempoChanges = loadTempoChangeData(tempoFile, &tempoChangeCount);
  fclose(tempoFile);

  sequenceFile = fopen(sequencePath, "r");
  if(sequenceFile == NULL){
    fprintf(stderr,
      "SOUND failed to open sequence file: %s %s\n", sequencePath, strerror(errno));
    exit(-1);
  }
  events = loadSequenceData(sequenceFile, &eventCount);
  fclose(sequenceFile);
  //printf("voice events = %d\n", eventCount);

  recomputeEventTimes(events, eventCount, tempoChanges, tempoChangeCount, ticksPerBeat);
}


// execute midi events within the range fromNs to toNs where 0 is the start
// of the song. should consider loop position to repeat parts indefinitely.
void dispatchFrame(uint64_t fromNs, uint64_t toNs){
  //fprintf(stderr, "[%llu, %llu)\n", fromNs, toNs);
  unsigned char packetListStorage[PACKET_LIST_SIZE];
  MIDIPacketList* packetList = (MIDIPacketList*) packetListStorage;
  MIDIPacket* packet;
  int packetSize = sizeof(uint64_t) + sizeof(uint16_t) + 3;
  unsigned char midi[3];
  int i, j;
  int midiSize;
  int c = 0;

  for(i=0; i<eventCount; i++){ // get to the first event in range
    if(events[i].atNs >= fromNs) break;
  }
  //printf("i = %d\n", i);

  packet = MIDIPacketListInit(packetList);

  for(j=0; i<eventCount; i++, j++){ // for each event in range
    if(events[i].atNs >= toNs) break;
    //printf("i=%d j=%d %u %x %u %u\n", i, j, events[i].tick, events[i].typeChan, events[i].arg1, events[i].arg2);

    c++;
    midi[0] = events[i].typeChan;
    midi[1] = events[i].arg1;
    midi[2] = events[i].arg2;
    midiSize = 3;
    if((midi[0] & 0xf0) == 0xc0 || (midi[0] & 0xf0) == 0xd0) midiSize = 2;
    if((midi[0] & 0xf0) != 0x80){
//      printf("%llu %02x %02x %02x\n", events[i].atNs, midi[0], midi[1], midi[2]);
    }
    if((midi[0] & 0xf0) == 0x90 && midi[2] > 0){
      rememberNoteOn(midi[0] & 0x0f, midi[1]);
    }
    if((midi[0] & 0xf0) == 0x80 || ((midi[0] & 0xf0) == 0x90 && midi[2] == 0)){
      forgetNoteOn(midi[0] & 0x0f, midi[1]);
    }
    packet = MIDIPacketListAdd(
      packetList,
      PACKET_LIST_SIZE,
      packet, 
      events[i].atNs + absoluteSongStartNs + FRAME_SIZE_NS,
      midiSize,
      midi
    );
    if(packet == NULL){
      fprintf(stderr, "** SOUND unable to MIDIPacketListAdd\n");
      exit(-1);
    }
    //printf("still ok\n");
  }

  //printf("output\n");
  MIDIReceived(outputPort, packetList);
  //printf("hmm\n");
}

// a frame is a 20ms chunk of time. we play 20ms ahead of time, sleep for
// 20ms, play 20ms of events, and sleep for ~20ms depending on overshot.
// ASSUMPTION mach_absolute_time returns nanoseconds, that is num=denom=1
void* sleepWakeAndDispatchFrame(){
  uint64_t sleepTargetNs;
  uint64_t currentNs;

  currentNs = mach_absolute_time();
  absolutePlayHeadNs = currentNs + FRAME_SIZE_NS;
  absoluteLeadingEdgeNs = absolutePlayHeadNs + FRAME_SIZE_NS;
  absoluteSongStartNs = currentNs - songNs;

  for(;;){
    if(playFlag == 0){
      onlineSeekFlag = 0;
      killAll();
      return NULL;
    }
    if(onlineSeekFlag == 1){
      killAll();
      songNs = onlineSeekTargetNs;
      absoluteSongStartNs = currentNs - songNs;
      onlineSeekFlag = 0;
    }
    printf("%lf\n", getCurrentBeat());
    dispatchFrame(songNs, songNs + FRAME_SIZE_NS);
    /*fprintf(stderr, "dispatch frame [%llu, %llu) [%llu, %llu)\n",
      songNs, songNs+FRAME_SIZE_NS,
      absolutePlayHeadNs, absoluteLeadingEdgeNs
    );*/
    sleepTargetNs = absolutePlayHeadNs - currentNs;
    absolutePlayHeadNs += FRAME_SIZE_NS;
    absoluteLeadingEdgeNs += FRAME_SIZE_NS;
    songNs += FRAME_SIZE_NS; // looping wrapping...
    usleep(sleepTargetNs / 1000);
    currentNs = mach_absolute_time();
    if(currentNs > absolutePlayHeadNs){ // over sleep
      fprintf(stderr, "SOUND over slept! game over man!\n");
      exit(-1);
    }
  }
}


void spawnDispatchThread(){
  int ret;
  ret = pthread_create(&dispatchThread, NULL, sleepWakeAndDispatchFrame, NULL);
  if(ret){
    fprintf(stderr,"SOUND dispatch thread failed to create: %s\n",strerror(errno));
    exit(-1);
  }
}

void joinDispatchThread(){
  int ret;
  ret = pthread_join(dispatchThread, NULL);
  if(ret){
    fprintf(
      stderr,
      "SOUND dispatch thread failed to join: %s (%d)\n",
      strerror(ret),
      ret
    );
    exit(-1);
  }
}

void stdinWorker(){
  char buf[INBUF_SIZE];
  char command[INBUF_SIZE];
  char arg1[INBUF_SIZE];
  char arg2[INBUF_SIZE];
  int number;
  int numerator;
  int denominator;
  int result;

  fgets(buf, INBUF_SIZE, stdin);
  if(ferror(stdin)){
    fprintf(stderr, "SOUND error while reading stdin. <%s>\n", strerror(errno));
    exit(-1);
  }
  if(feof(stdin)){
    fprintf(stderr, "SOUND stdin is EOF. Terminating\n");
    exit(0);
  }

  buf[strlen(buf)-1] = 0;

  result = sscanf(buf, "%s", command);
  if(result <= 0){
    fprintf(stderr, "SOUND unrecognized command\n");
  }
  else if(strcmp(command, "LOAD")==0){
    result = sscanf(buf, "%s %s %s", command, arg1, arg2);
    if(result < 3){
      fprintf(stderr, "** SOUND invalid LOAD command (%s)\n", buf);
      exit(-1);
    }
    loadData(arg1, arg2);
  }
  else if(strcmp(command, "PLAY")==0){
    if(playFlag == 0){
      playFlag = 1;
      spawnDispatchThread();
    }
    else{
      fprintf(stderr, "SOUND refusing to play, playFlag=%d\n", playFlag);
    }
  }
  else if(strcmp(command, "STOP")==0){
    if(playFlag == 1){
      playFlag = 0;
      joinDispatchThread();
    }
    else{
      fprintf(stderr, "SOUND stop ignored, playFlag=%d\n", playFlag);
    }
  }
  else if(strcmp(command, "SEEK")==0){
    result = sscanf(buf, "%s %d %d/%d", command, &number, &numerator, &denominator);
    if(result < 4){
      result = sscanf(buf, "%s %d", command, &number);
      if(result < 2){
        fprintf(stderr, "** SOUND invalid SEEK command (%s)\n", buf);
        return;
      }
      numerator = 0;
      denominator = 1;
    }
    executeSeek(number, numerator, denominator);
  }
  else if(strcmp(command, "CRASH")==0){
    fprintf(stderr, "%d\n", 0 / (int)NULL);
  }
  else if(strcmp(command, "EXIT")==0){
    playFlag = 0;
    joinDispatchThread();
    usleep(100000);
    //shutdownCoreMidi();
    exit(0);
  }
  else if(strcmp(command, "CUT_ALL")==0){
    killAll();
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
  else if(strcmp(command, "TICKS_PER_BEAT")==0){
    result = sscanf(buf, "%s %d", command, &number);
    if(result < 2){
      fprintf(stderr, "** SOUND invalid TICKS_PER_BEAT command (%s)\n", buf);
    }
    if(number <= 0){
      fprintf(stderr, "** SOUND ignoring setting ticks per beat to %d\n", number);
    }
    else{
      ticksPerBeat = number;
      fprintf(stderr, "SOUND ticksPerBeat=%d\n", number);
      // set a recompute time flag?
    }
  }
  else if(strcmp(command, "SET_PARAMETERS")==0){
    // set the new params and set the parameter change flag 
  }
  else if(strcmp(command, "TELL")==0){
    printf("%lf\n", getCurrentBeat());
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
    fprintf(stderr, "SOUND unrecognized command (%s)\n", buf);
  }
}

void interrupt(int unused){
  fprintf(stderr, "** SOUND interrupted by signal\n");
  playFlag = 0;
  joinDispatchThread();
  usleep(100000);
  exit(0);
}

int main(int argc, char* argv[]){
  fprintf(stderr, "SOUND Hello World\n");
  
  if(setupCoreMidi()){
    fprintf(stderr, "SOUND CoreMidi setup failed.\n");
    exit(-1);
  }

  initPlayingNotes();

  signal(SIGINT, interrupt);

  for(;;) stdinWorker();
  fprintf(stderr, "SOUND impossible -2\n");
  return -2;
}

