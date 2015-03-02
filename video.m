// First program example

#import <Cocoa/Cocoa.h>
#import <Carbon/Carbon.h>
#import <stdio.h>
#import <unistd.h>
#import <stdlib.h>

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


void spawnCore(FILE** paintIn, FILE** eventOut){
  int eventPipe[2]; //we write to 1
  int paintPipe[2]; //we read from 0
  char* args[] = {"core", NULL};
  pid_t pid;

  if(pipe(eventPipe) < 0){
    fprintf(stderr, "VIDEO event pipe failed (%s)\n", strerror(errno));
    exit(-1);
  }
  if(pipe(paintPipe) < 0){
    fprintf(stderr, "VIDEO paint pipe failed (%s)\n", strerror(errno));
    exit(-1);
  }

/*
  write(eventPipe[1], "BOOYA\n", 6);
  char buf[10];
  buf[9] = 0;
  if(read(eventPipe[0], buf, 6) < 0){
    printf("WROGNGG %s\n", strerror(errno));
    exit(-1);
  }
  else{
    printf("READ WORKED! %s\n", buf);
    exit(-1);
  }
*/

  pid = fork();
  if(pid == -1){
    fprintf(stderr, "VIDEO fork failed! %s\n", strerror(errno));
    exit(-1);
  }
  else if(pid == 0){ // child 
    close(eventPipe[1]);
    close(paintPipe[0]);
    close(0);
    dup(eventPipe[0]); // we will read event commands from stdin
    close(1);
    dup(paintPipe[1]); // we will write paint commands to stdout
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

    *paintIn = fdopen(paintPipe[0], "r");
    if(*paintIn == NULL){
      fprintf(stderr, "VIDEO can't fdopen event stream %s\n", strerror(errno));
      exit(-1);
    }
  }
}
  

void showGraphics(){
  CGContextRef context = [[NSGraphicsContext currentContext] graphicsPort];
  CGContextSetRGBFillColor(context, 1, 0, 0, 1);
  CGContextFillRect(context, CGRectMake (0, 0, 200, 100 ));
  CGContextSetRGBFillColor(context, 0, 0, 1, .5);
  CGContextFillRect(context, CGRectMake (0, 0, 100, 200));
}

@interface MyWindow : NSWindow

@property FILE* eventOut;

@end

@implementation MyWindow

- (void)mouseDown:theEvent {
  NSLog(@"%@", theEvent);
}

- (void)mouseUp:theEvent {
  NSLog(@"%@", theEvent);
}

- (void)rightMouseDown:theEvent {
  NSLog(@"%@", theEvent);
}

- (void)rightMouseUp:theEvent {
  NSLog(@"%@", theEvent);
}

- (void)mouseMoved:theEvent {
  NSLog(@"%@", theEvent);
}

- (void)keyDown:theEvent {
  CGKeyCode k = [theEvent keyCode];
  //write(self.eventPipe, buf, strlen(buf));
  //write(self.eventPipe, "BOOYA\n", 6);
  printf("event out %p\n", self.eventOut);
  fprintf(self.eventOut, "click left\n");
  //[self flushWindow];
}

- (void)keyUp:theEvent {
//  NSLog(@"%@", theEvent);
}

- (void)flagsChanged:theEvent {
  NSLog(@"%@", theEvent);
}

- (void)scrollWheel:theEvent {
  NSLog(@"%@", theEvent);
}

- (void)display {
  fprintf(stdout, "WINDOW DISPLAY\n");
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
  printf("application should terminate\n");
  return YES;
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
  exit(0);
}

- (void)about:(id)sender {
  printf("ABOUT\n");
}

- (void)booya:sender {
  printf("BOOYA\n");
}
@end


@interface MyNotificationHandler : NSObject
@property (assign) NSFileHandle* standardIn;
@property (assign) NSNotificationCenter* center;
@property FILE* eventOut;

- (id)initWithEventOut:(FILE*)eventOut;

- (void)windowClosing:NSNotification;
- (void)stdinReadable:NSNotification;
- (void)windowUnminimize:NSNotification;
- (void)windowResized:NSNotification;

- (void)registerStdinReadable;
- (void)registerWindowClosing;
- (void)registerWindowUnminimize;
@end

@implementation MyNotificationHandler

- (id)initWithEventOut:(FILE*)eventOut {
  self.eventOut = eventOut;
  return self;
}

- (void)windowClosing:notif {
  NSLog(@"%@", notif);
  printf("window is about to close\n");
  printf("VIDEO \"GAME OVER MAN\"\n");
  exit(0);
}

- (void)windowUnminimize:notif {
  NSLog(@"%@", notif);
  printf("window exposed\n");
}

- (void)windowResized:notif {
  NSLog(@"%@", notif);
  printf("window resized\n");
}

- (void)stdinReadable:notif {
  NSLog(@"%@", notif);
  printf("stdin readable\n");
  NSData* data = self.standardIn.availableData;
  if(data.length == 0){
    fprintf(stderr, "VIDEO stdin stream has ended. Terminating.\n");
    exit(-1);
  }
  NSLog(@"contents: %@", data.description);
  [self registerStdinReadable];
}

- (void)registerStdinReadable {
  [self.standardIn waitForDataInBackgroundAndNotify];
  [self.center
    addObserver:self
    selector:@selector(stdinReadable:) 
    name:@"NSFileHandleDataAvailableNotification"
    object:nil ];
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

- (void)registerWindowResized {
  [self.center
    addObserver:self
    selector:@selector(windowResized:)
    name:@"NSWindowDidResizeNotification"
    object:nil ];
}

@end


int main (int argc, const char * argv[])
{
  FILE* paintIn = stdin;
  FILE* eventOut = stdout;

  fprintf(stderr, "VIDEO Hello World\n");

  if(argc < 2){ // by default, spawn the core application
    spawnCore(&paintIn, &eventOut);
    close(0);
    dup(fileno(paintIn));
    //close(1);
    //dup(fileno(eventOut));
  }

  [NSAutoreleasePool new];
  [NSApplication sharedApplication];
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

  //install delegate
  id delegate = [[MyDelegate alloc] initWithEventOut:eventOut];
  ((NSApplication*)NSApp).delegate = [MyDelegate new];

  //install notification handlers
  id standardIn = [NSFileHandle fileHandleWithStandardInput];
  MyNotificationHandler* handler =
    [[MyNotificationHandler alloc] initWithEventOut:eventOut];
  handler.standardIn = standardIn;
  handler.center = [NSNotificationCenter defaultCenter];
  handler.eventOut = eventOut;
  [handler registerStdinReadable];
  [handler registerWindowClosing];
  [handler registerWindowUnminimize];
  [handler registerWindowResized];

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
    

  MyWindow* window = [[[MyWindow alloc] 
    initWithContentRect:NSMakeRect(0,0,640,480)
    styleMask:NSTitledWindowMask|NSClosableWindowMask|NSMiniaturizableWindowMask|NSResizableWindowMask
    backing:NSBackingStoreBuffered
    defer:NO
  ] autorelease];
  window.eventOut = eventOut;
  [window setAcceptsMouseMovedEvents:YES];
  [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
  [window setTitle:appName];
  [window makeKeyAndOrderFront:nil];
  [NSApp activateIgnoringOtherApps:YES];
  showGraphics();
  [NSApp run];
  return 0;
   //NSLog (@"Hello world. Arguments: %@", [[NSProcessInfo processInfo] arguments]);
   //NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

   //[pool drain];
}
