#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>
#import <stdio.h>
#import <unistd.h>
#import <stdlib.h>

NSWindow* mainWindow = NULL;

unsigned char* paintBuffer = NULL;
size_t paintBufferSize = 0;
int paintBufferPtr = 0;

void flushGraphics(){
  NSGraphicsContext* context = [NSGraphicsContext currentContext];
  //printf("flushing cocoa\n");
  [context flushGraphics];
}

int clamp(int lower, int x, int upper){
  if(x < lower) return lower;
  else if(x > upper) return upper;
  else return x;
}

void paintBox(int x, int y, int w, int h, int r, int g, int b){
}

void paintFilledBox(int x, int y, int w, int h, int r, int g, int b){
  NSSize size = [[mainWindow contentView] frame].size;
  NSGraphicsContext* context = [NSGraphicsContext currentContext];
  CGContextRef port = [context graphicsPort];
  int xx0 = clamp(0, x, size.width);
  int xx1 = clamp(0, x+w, size.width);
  int ww = xx1 - xx0;
  int yy0 = clamp(0, y, size.height);
  int yy1 = clamp(0, y+h, size.height);
  int hh = yy1 - yy0;
  CGContextSetRGBFillColor(port, r/255.0, g/255.0, b/255.0, 1);
  CGContextFillRect(port, CGRectMake (xx0, size.height-yy0, ww, -hh));
}

void executePaintCommand(){
  char command[32];
  int base;
  int results;
  int args[8];
  //printf("executing paint command\n");

  paintBuffer[paintBufferPtr] = 0;

  results = sscanf((char*)paintBuffer, "%31s", command);
  if(results < 1){
    fprintf(stderr,
      "** VIDEO failed to parse command (%31s)\n", (char*)paintBuffer);
    paintBufferPtr = 0;
    return;
  }
  base = strlen(command);

  //printf("buffer = %s\n", paintBuffer);
  //printf("command = %s\n", command);

  if(strcmp(command, "fill")==0){
    results = sscanf(
      (char*)paintBuffer+base, "%d %d %d %d %d %d %d",
      &args[0], &args[1], &args[2], &args[3], &args[4], &args[5], &args[6]
    );
    if(results < 7){
      fprintf(stderr, "** VIDEO invalid fill command\n");
      paintBufferPtr = 0;
      return;
    }
    else{
      paintFilledBox(
        args[0], args[1], args[2], args[3],
        args[4], args[5], args[6]
      );
    }
  }
  else if(strcmp(command, "flush")==0){
    flushGraphics();
  }
  else{
    fprintf(stderr, "VIDEO unknown paint command %s\n", command);
  }

  paintBufferPtr = 0;
}

void appendToPaintBuffer(size_t count, const unsigned char* bytes){
  // count <= 256, buffer >= 1024, only doubling is needed when no room
  //printf("append to buffer %lu: \n", count);
  //printf("\"");
  for(int i=0; i<count; i++){
    //printf("%c", bytes[i]);
  }
  //printf("\"");
    //printf("\n");
  if(paintBufferPtr + count >= paintBufferSize){
    printf("expanding paint buffer to %lu\n", paintBufferSize * 2);
    paintBuffer = realloc(paintBuffer, paintBufferSize * 2);
    if(paintBuffer == NULL){
      fprintf(stderr, "** VIDEO failed to expand paint buffer\n");
      exit(-2);
    }
    paintBufferSize *= 2;
  }

  memcpy(paintBuffer+paintBufferPtr, bytes, count);
  paintBufferPtr += count;
  //printf("paintBufferPtr = %d\n", paintBufferPtr);
}

void paintIn(size_t count, const unsigned char* bytes){

  int i = 0;
  int j = 0;

  if(count == 0){
    fprintf(stderr, "** VIDEO paintIn zero bug\n");
    exit(-1);
  }

  //printf("paintIn %lu\n", count);

  for(;;){
    if(bytes[i] == '\n'){
      //printf("newline found at i=%d (j=%d)\n", i, j);
      if(i-j > 0){
        appendToPaintBuffer(i-j, bytes+j);
        executePaintCommand();
      }
      i++;
      j=i;

      if(i==count) return;
    }
    else if(i == count - 1){
      //printf("no newline found i=%d j=%d\n", i, j);
      appendToPaintBuffer(i-j+1, bytes+j);
      return;
    }
    else{
      i++;
    }
  }
}


void initializePaintBuffer(){
  paintBuffer = malloc(1024);
  paintBufferSize = 1024;
}


/*
void showGraphics(){
  CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
  CGContextSetRGBFillColor(context, 1, 0, 0, 1);
  CGContextFillRect(context, CGRectMake (0, 0, 200, 100 ));
  CGContextSetRGBFillColor(context, 0, 0, 1, .5);
  CGContextFillRect(context, CGRectMake (0, 0, 100, 200));
}
*/


int notForCharacters(int keycode){
  switch(keycode){
    case kVK_F1: return 1;
    case kVK_F2: return 1;
    case kVK_F3: return 1;
    case kVK_F4: return 1;
    case kVK_F5: return 1;
    case kVK_F6: return 1;
    case kVK_F7: return 1;
    case kVK_F8: return 1;
    case kVK_F9: return 1;
    case kVK_F10: return 1;
    case kVK_F11: return 1;
    case kVK_F12: return 1;
    case kVK_LeftArrow: return 1;
    case kVK_RightArrow: return 1;
    case kVK_UpArrow: return 1;
    case kVK_DownArrow: return 1;
    default: return 0;
  }
}

int modifierDown(int code, uint64_t flags){
  switch(code){
    case kVK_Shift:        return (flags & NX_DEVICELSHIFTKEYMASK) ? 1 : 0;
    case kVK_RightShift:   return (flags & NX_DEVICERSHIFTKEYMASK) ? 1 : 0;
    case kVK_Command:      return (flags & NX_DEVICELCMDKEYMASK)   ? 1 : 0;
    case 54:               return (flags & NX_DEVICERCMDKEYMASK)   ? 1 : 0;
    case kVK_Control:      return (flags & NX_DEVICELCTLKEYMASK)   ? 1 : 0;
    case kVK_RightControl: return (flags & NX_DEVICERCTLKEYMASK)   ? 1 : 0;
    case kVK_Option:       return (flags & NX_DEVICELALTKEYMASK)   ? 1 : 0;
    case kVK_RightOption:  return (flags & NX_DEVICERALTKEYMASK)   ? 1 : 0;
    case kVK_CapsLock:     return (flags & NX_ALPHASHIFTMASK)      ? 1 : 0;
    case kVK_Function:     return (flags & NX_SECONDARYFNMASK)     ? 1 : 0;
    default:               return -1;
  }
  /*
  #define	NX_ALPHASHIFTMASK	0x00010000
  #define	NX_SHIFTMASK		0x00020000
  #define	NX_CONTROLMASK		0x00040000
  #define	NX_ALTERNATEMASK	0x00080000
  #define	NX_COMMANDMASK		0x00100000
  #define	NX_NUMERICPADMASK	0x00200000
  #define	NX_HELPMASK		0x00400000
  #define	NX_SECONDARYFNMASK	0x00800000
  */

  /* device-dependent (really?) */
  /*
  #define	NX_DEVICELCTLKEYMASK	0x00000001
  #define	NX_DEVICELSHIFTKEYMASK	0x00000002
  #define	NX_DEVICERSHIFTKEYMASK	0x00000004
  #define	NX_DEVICELCMDKEYMASK	0x00000008
  #define	NX_DEVICERCMDKEYMASK	0x00000010
  #define	NX_DEVICELALTKEYMASK	0x00000020
  #define	NX_DEVICERALTKEYMASK	0x00000040
  #define NX_DEVICERCTLKEYMASK	0x00002000
  */
}

const char* keycodeToString(int code){
  switch(code){
    case kVK_Escape: return "escape";
    case kVK_F1: return "f1";
    case kVK_F2: return "f2";
    case kVK_F3: return "f3";
    case kVK_F4: return "f4";
    case kVK_F5: return "f5";
    case kVK_F6: return "f6";
    case kVK_F7: return "f7";
    case kVK_F8: return "f8";
    case kVK_F9: return "f9";
    case kVK_F10: return "f10";
    case kVK_F11: return "f11";
    case kVK_F12: return "f12";
    //case kVK_: return "power" ;
    case kVK_ANSI_Grave: return "grave-accent";
    case kVK_ANSI_1: return "1";
    case kVK_ANSI_2: return "2";
    case kVK_ANSI_3: return "3";
    case kVK_ANSI_4: return "4";
    case kVK_ANSI_5: return "5";
    case kVK_ANSI_6: return "6";
    case kVK_ANSI_7: return "7";
    case kVK_ANSI_8: return "8";
    case kVK_ANSI_9: return "9";
    case kVK_ANSI_0: return "0";
    case kVK_ANSI_Minus: return "minus";
    case kVK_ANSI_Equal: return "equals";
    case kVK_Delete: return "backspace";
    case kVK_Tab: return "tab";
    case kVK_ANSI_Q: return "q";
    case kVK_ANSI_W: return "w";
    case kVK_ANSI_E: return "e";
    case kVK_ANSI_R: return "r";
    case kVK_ANSI_T: return "t";
    case kVK_ANSI_Y: return "y";
    case kVK_ANSI_U: return "u";
    case kVK_ANSI_I: return "i";
    case kVK_ANSI_O: return "o";
    case kVK_ANSI_P: return "p";
    case kVK_ANSI_LeftBracket: return "left-bracket";
    case kVK_ANSI_RightBracket: return "right-bracket";
    case kVK_ANSI_Backslash: return "backslash";
    case kVK_CapsLock: return "caps-lock";
    case kVK_ANSI_A: return "a";
    case kVK_ANSI_S: return "s";
    case kVK_ANSI_D: return "d";
    case kVK_ANSI_F: return "f";
    case kVK_ANSI_G: return "g";
    case kVK_ANSI_H: return "h";
    case kVK_ANSI_J: return "j";
    case kVK_ANSI_K: return "k";
    case kVK_ANSI_L: return "l";
    case kVK_ANSI_Semicolon: return "semicolon";
    case kVK_ANSI_Quote: return "quote";
    case kVK_Return: return "return";
    case kVK_Shift: return "left-shift";
    case kVK_ANSI_Z: return "z";
    case kVK_ANSI_X: return "x";
    case kVK_ANSI_C: return "c";
    case kVK_ANSI_V: return "v";
    case kVK_ANSI_B: return "b";
    case kVK_ANSI_N: return "n";
    case kVK_ANSI_M: return "m";
    case kVK_ANSI_Comma: return "comma";
    case kVK_ANSI_Period: return "period";
    case kVK_ANSI_Slash: return "slash";
    case kVK_RightShift: return "right-shift";
    case kVK_Function: return "fn";
    case kVK_Control: return "left-control";
    case kVK_Option: return "left-option";
    case kVK_Command: return "left-command";
    case kVK_Space: return "spacebar";
    case 54: return "right-command";
    case kVK_RightOption: return "right-option";
    case kVK_LeftArrow: return "left-arrow";
    case kVK_RightArrow: return "right-arrow";
    case kVK_UpArrow: return "up-arrow";
    case kVK_DownArrow: return "down-arrow";
    default: return NULL;
  }
  /*
  kVK_ANSI_KeypadDecimal        = 0x41,
  kVK_ANSI_KeypadMultiply       = 0x43,
  kVK_ANSI_KeypadPlus           = 0x45,
  kVK_ANSI_KeypadClear          = 0x47,
  kVK_ANSI_KeypadDivide         = 0x4B,
  kVK_ANSI_KeypadEnter          = 0x4C,
  kVK_ANSI_KeypadMinus          = 0x4E,
  kVK_ANSI_KeypadEquals         = 0x51,
  kVK_ANSI_Keypad0              = 0x52,
  kVK_ANSI_Keypad1              = 0x53,
  kVK_ANSI_Keypad2              = 0x54,
  kVK_ANSI_Keypad3              = 0x55,
  kVK_ANSI_Keypad4              = 0x56,
  kVK_ANSI_Keypad5              = 0x57,
  kVK_ANSI_Keypad6              = 0x58,
  kVK_ANSI_Keypad7              = 0x59,
  kVK_ANSI_Keypad8              = 0x5B,
  kVK_ANSI_Keypad9              = 0x5C
  */
};


void spawnCore(NSFileHandle** paintIn, FILE** eventOut){
  int paintPipe[2]; //we read from 0
  int eventPipe[2]; //we write to 1
  char buf1[10];
  char buf2[10];
  char* args[] = {"core", "-p", NULL, "-e", NULL, "-w (640,480)", NULL};
  pid_t pid;

  if(pipe(paintPipe) < 0){
    fprintf(stderr, "VIDEO paint pipe failed (%s)\n", strerror(errno));
    exit(-1);
  }
  if(pipe(eventPipe) < 0){
    fprintf(stderr, "VIDEO event pipe failed (%s)\n", strerror(errno));
    exit(-1);
  }

  snprintf(buf1, 10, "%d", paintPipe[1]);
  snprintf(buf2, 10, "%d", eventPipe[0]);
  args[2] = buf1;
  args[4] = buf2;

  pid = fork();
  if(pid == -1){
    fprintf(stderr, "VIDEO fork failed! %s\n", strerror(errno));
    exit(-1);
  }
  else if(pid == 0){ // child 
    close(eventPipe[1]);
    close(paintPipe[0]);
    execve("./core", args, NULL);
    fprintf(stderr, "LIMINAL VIDEO->CORE exec failed! %s\n", strerror(errno));
    exit(-1);
  }
  else{ // parent
    close(eventPipe[0]);
    close(paintPipe[1]);

    *eventOut = fdopen(eventPipe[1], "w");
    if(*eventOut == NULL){
      fprintf(stderr, "VIDEO can't fdopen event stream %s\n", strerror(errno));
      exit(-1);
    }
    if(setvbuf(*eventOut, NULL, _IOLBF, 0) < 0){
      fprintf(stderr, "VIDEO setvbuf failed! %s\n", strerror(errno));
      exit(-1);
    }


    *paintIn = [[NSFileHandle alloc] initWithFileDescriptor:paintPipe[0]];
  }
}
  
@interface MyWindow : NSWindow

@property FILE* eventOut;

@end

@implementation MyWindow

- (bool)windowShouldClose {
  return NO;
}

- (void)mouseDown:theEvent {
  fprintf(self.eventOut, "click %d\n", 0);
  //printf("mouse down cocoa\n");
}

- (void)mouseUp:theEvent {
  fprintf(self.eventOut, "release %d\n", 0);
  //printf("mouse up cocoa\n");
}

- (void)rightMouseDown:theEvent {
  fprintf(self.eventOut, "click %d\n", 1);
}

- (void)rightMouseUp:theEvent {
  fprintf(self.eventOut, "release %d\n", 1);
}

- (void)otherMouseDown:theEvent {
  int button = [theEvent buttonNumber];
  fprintf(self.eventOut, "click %d\n", button);
}

- (void)otherMouseUp:theEvent {
  int button = [theEvent buttonNumber];
  fprintf(self.eventOut, "release %d\n", button);
}

- (void)commonMouseMoved:theEvent {
  id v = [self contentView];
  NSSize size = [v frame].size;
  NSPoint p = [v convertPoint:[theEvent locationInWindow] fromView:nil];
  double mouseX = p.x;
  double mouseY = size.height - p.y;
  fprintf(self.eventOut, "mouse %lf %lf\n", mouseX, mouseY);
  //printf("mouse moved cocoa\n");
}

- (void)mouseMoved:theEvent {
  [self commonMouseMoved:theEvent];
}

- (void)mouseDragged:theEvent {
  [self commonMouseMoved:theEvent];
}

- (void)rightMouseDragged:theEvent {
  [self commonMouseMoved:theEvent];
}

- (void)otherMouseDragged:theEvent {
  [self commonMouseMoved:theEvent];
}

- (void)keyDown:theEvent {
  CGKeyCode k = [theEvent keyCode];
  const char* name = keycodeToString(k);
  char c[16];

  if(![theEvent isARepeat]){
    if(name) fprintf(self.eventOut, "keydown %s\n", name);
    else     fprintf(self.eventOut, "keydown unknown cocoa %u\n", k);
  }

  if([[theEvent characters] getCString:c maxLength:16 encoding:NSUTF8StringEncoding] == NO){
    fprintf(stderr, "** VIDEO unable to encode character ");
    NSLog(@"%@", [theEvent characters]);
  }
  else{
    if(!notForCharacters(k) && strlen(c) > 0){
      if(k == kVK_Return) fprintf(self.eventOut, "character \\n\n");
      else if(k == kVK_Delete) fprintf(self.eventOut, "character \\b\n");
      else fprintf(self.eventOut, "character %s\n", c);
    }
  }
}

- (void)keyUp:theEvent {
  CGKeyCode k = [theEvent keyCode];
  const char* name = keycodeToString(k);
  if(name) fprintf(self.eventOut, "keyup %s\n", name);
  else     fprintf(self.eventOut, "keyup unknown cocoa %u\n", k);
}

- (void)flagsChanged:theEvent {
  CGKeyCode k = [theEvent keyCode];
  const char* name = keycodeToString(k);
  int down = modifierDown(k, [theEvent modifierFlags]);
  if(name != NULL){
    if(down == 1) fprintf(self.eventOut, "keydown %s\n", name);
    if(down == 0) fprintf(self.eventOut, "keyup %s\n", name);
  }
  //NSLog(@"%@", theEvent);
}

- (void)scrollWheel:(NSEvent*)theEvent {
  double dy = theEvent.deltaY;
  fprintf(self.eventOut, "wheel %lf\n", dy);
}

- (void)display {
  fprintf(stdout, "WINDOW DISPLAY\n");
}

/* this would go in a window delegate which i didnt set up
- (NSSize)windowWillResize:(NSWindow*)sender toSize:(NSSize)newSize {
  printf("%lf %lf\n", newSize.width, newSize.height);
  return newSize;
}
*/


@end

@interface MyWindowDelegate : NSObject <NSWindowDelegate>
@property FILE* eventOut;
@end

@implementation MyWindowDelegate

- (BOOL)windowShouldClose:(id)sender {
  fprintf(self.eventOut, "quit\n");
  return NO;
}

- (void)windowDidBecomeMain:(NSNotification*)notif {
  NSPoint loc = [NSEvent mouseLocation];
  NSRect winframe = [mainWindow frame];
  loc.x -= winframe.origin.x;
  loc.y -= winframe.origin.y;
  id v = [mainWindow contentView];
  NSSize size = [v frame].size;
  NSPoint p = [v convertPoint:loc fromView:nil];
  double mouseX = p.x;
  double mouseY = size.height - p.y;
  fprintf(self.eventOut, "mouse %lf %lf\n", mouseX, mouseY);
}

- (void)windowDidEndLiveResize:(NSNotification*)notif {
  NSWindow* win = [notif object];
  NSSize size = [[win contentView] frame].size;
  fprintf(self.eventOut, "resize %d %d\n", (int)size.width, (int)size.height);

  //printf("resize cocoa\n");
}


@end

@interface MyDelegate : NSObject <NSApplicationDelegate>
@property FILE* eventOut;
- (id)initWithEventOut:(FILE*)eventOut;
@end

@implementation MyDelegate

- (id)initWithEventOut:(FILE*)eventOut {
  self.eventOut = eventOut;
  return self;
}

- (NSApplicationTerminateReply)applicationShouldTerminate:sender {
  fprintf(self.eventOut, "quit\n");
  return NO;
}

@end

@interface MyMenuHandler : NSObject
- (void)quit:(id)sender;
- (void)about:(id)sender;
- (void)booya:(id)sender;
@end

@implementation MyMenuHandler
- (void)quit:(id)sender {
  printf("QUIT\n");
}

- (void)about:(id)sender {
  printf("ABOUT\n");
}

- (void)booya:sender {
  printf("BOOYA\n");
}
@end


@interface MyNotificationHandler : NSObject
@property (assign) NSFileHandle* paintIn;
@property (assign) NSNotificationCenter* center;
@property FILE* eventOut;

- (id)initWithEventOut:(FILE*)eventOut;

- (void)windowClosing:NSNotification;
- (void)stdinReadable:NSNotification;
- (void)windowUnminimize:NSNotification;
//- (void)windowResized:NSNotification;

- (void)registerWindowClosing;
- (void)registerWindowUnminimize;
@end

void dumpBytes(int n, const unsigned char* bytes){
  int i;
  for(i=0; i<n; i++){
    printf("%02x ", bytes[i]);
  }
  printf("\n");
}

@implementation MyNotificationHandler

- (id)initWithEventOut:(FILE*)eventOut {
  self.eventOut = eventOut;
  return self;
}

- (void)windowClosing:notif {
  NSLog(@"%@", notif);
  printf("window is about to close\n");
  printf("VIDEO \"GAME OVER MAN\"\n");
  //exit(0);
}

- (void)windowUnminimize:notif {
  NSLog(@"%@", notif);
  printf("window exposed\n");
}

/*
- (void)windowResized:notif {
  NSWindow* win = [notif object];
  NSSize size = [[win contentView] frame].size;
  fprintf(self.eventOut, "resize %d %d\n", (int)size.width, (int)size.height);

  //printf("resize cocoa\n");
}
*/

- (void)stdinReadable:(NSNotification*)notif {
  int i=0;
  NSFileHandle* fileHandle = [notif object];
  //printf("stdin readable ... \n");
  NSData* data = fileHandle.availableData;
  //dumpBytes(data.length, data.bytes);
  if(data.length == 0){
    fprintf(stderr, "VIDEO stdin stream has ended. Terminating.\n");
    exit(-1);
  }
  for(;;){
    if(data.length - i > 256){
      paintIn(256, data.bytes+i);
      i += 256;
    }
    else{
      paintIn(data.length - i, data.bytes+i);
      break;
    }
  }
  [self.paintIn waitForDataInBackgroundAndNotify];
}


- (void)registerWindowClosing {
  [self.center
    addObserver:self
    selector:@selector(windowClosing:)
    name:@"NSWindowWillCloseNotification"
    object:nil ];
}

- (void)registerWindowUnminimize {
  [self.center
    addObserver:self
    selector:@selector(windowUnminimize:)
    name:@"NSWindowDidDeminiaturizeNotification"
    object:nil ];
}

/*
- (void)registerWindowResized {
  [self.center
    addObserver:self
    selector:@selector(windowResized:)
    name:@"NSWindowDidResizeNotification"
    object:nil ];
}
*/

@end


int main (int argc, const char * argv[])
{
  NSFileHandle* paintIn = [NSFileHandle fileHandleWithStandardInput];
  FILE* eventOut = stdout;

  fprintf(stderr, "VIDEO Hello World\n");

  initializePaintBuffer();

  if(argc < 2){ // by default, spawn the core application
    spawnCore(&paintIn, &eventOut);
    //close(0);
    //dup(fileno(paintIn));
    //close(1);
    //dup(fileno(eventOut));
  }

  [NSAutoreleasePool new];
  [NSApplication sharedApplication];
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];


  //install delegate
  MyDelegate* delegate = [MyDelegate new];
  delegate.eventOut = eventOut;
  ((NSApplication*)NSApp).delegate = delegate;

  //install notification handlers
  MyNotificationHandler* handler =
    [[MyNotificationHandler alloc] initWithEventOut:eventOut];
  handler.paintIn = paintIn;
  handler.center = [NSNotificationCenter defaultCenter];
  handler.eventOut = eventOut;
  [paintIn waitForDataInBackgroundAndNotify];
  [handler.center
    addObserver:handler
    selector:@selector(stdinReadable:) 
    name:@"NSFileHandleDataAvailableNotification"
    object:paintIn ];
  [handler registerWindowClosing];
  [handler registerWindowUnminimize];
  //[handler registerWindowResized];
  MyMenuHandler* menuHandler = [MyMenuHandler new];

  id menubar = [[NSMenu new] autorelease];
  id appMenuItem = [[NSMenuItem new] autorelease];
  [menubar addItem:appMenuItem];
  [NSApp setMainMenu:menubar];
  id appMenu = [[NSMenu new] autorelease];
  id appName = [[NSProcessInfo processInfo] processName];

  id quitTitle = [@"Quit " stringByAppendingString:appName];
  id quitMenuItem = [[[NSMenuItem alloc]
    initWithTitle:quitTitle
    action:@selector(terminate:)
    keyEquivalent:@"q"
  ] autorelease];
  [appMenu addItem:quitMenuItem];
  [appMenuItem setSubmenu:appMenu];

  NSMenuItem* otherMenuItem = [[NSMenuItem alloc]
    initWithTitle:@"Booya"
    action:@selector(booya:)
    keyEquivalent:@"123" ];
  [otherMenuItem setTarget:menuHandler];
  [appMenu addItem:otherMenuItem];
  [otherMenuItem setMenu:appMenu];
    

  MyWindowDelegate* windel = [MyWindowDelegate new];
  windel.eventOut = eventOut;
  MyWindow* window = [[[MyWindow alloc] 
    initWithContentRect:NSMakeRect(0,0,640,480)
    styleMask:NSTitledWindowMask|NSClosableWindowMask|NSMiniaturizableWindowMask|NSResizableWindowMask
    backing:NSBackingStoreBuffered
    defer:NO
  ] autorelease];
  mainWindow = window;
  window.eventOut = eventOut;
  [window setAcceptsMouseMovedEvents:YES];
  [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
  [window setTitle:appName];
  [window makeKeyAndOrderFront:nil];
  [window setDelegate:windel];
  [NSApp activateIgnoringOtherApps:YES];
  [NSApp run];
  return 0;
   //NSLog (@"Hello world. Arguments: %@", [[NSProcessInfo processInfo] arguments]);
   //NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

   //[pool drain];
}
