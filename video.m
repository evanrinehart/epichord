// First program example

#import <Cocoa/Cocoa.h>
#import <stdio.h>
#import <unistd.h>
#import <stdlib.h>

void spawnCore(int* paintIn, int* eventOut){
  int eventPipe[2];
  int paintPipe[2];
  char* args[] = {"core", NULL};
  pid_t pid;

  // in both of these pipes, by convention, we will use pipe 0
  pipe(eventPipe);
  pipe(paintPipe);

  pid = fork();

  if(pid == -1){
    fprintf(stderr, "VIDEO fork failed! %s\n", strerror(errno));
    exit(-1);
  }
  else if(pid == 0){ // child 
    fprintf(stderr, "LIMINAL VIDEO->CORE preparing to exec\n");
    close(eventPipe[0]);
    close(paintPipe[0]);
    close(0);
    dup(eventPipe[1]); // we will read event commands from stdin
    close(1);
    dup(paintPipe[1]); // we will write paint commands to stdout
    execve("./core", args, NULL);
    fprintf(stderr, "LIMINAL VIDEO->CORE exec failed! %s\n", strerror(errno));
    exit(-1);
  }
  else{ // parent
    close(eventPipe[1]);
    close(paintPipe[1]);
    *eventOut = eventPipe[0];
    *paintIn = paintPipe[0];
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

@property int eventPipe;

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
  NSLog(@"%@", theEvent);
  [self flushWindow];
}

- (void)keyUp:theEvent {
  NSLog(@"%@", theEvent);
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
@property int eventOut;
- (id)initWithEventOut:(int)fd;
@end

@implementation MyDelegate

- (id)initWithEventOut:(int)fd {
  self.eventOut = fd;
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
@property int eventOut;

- (id)initWithEventOut:(int)fd;

- (void)windowClosing:NSNotification;
- (void)stdinReadable:NSNotification;
- (void)windowUnminimize:NSNotification;
- (void)windowResized:NSNotification;

- (void)registerStdinReadable;
- (void)registerWindowClosing;
- (void)registerWindowUnminimize;
@end

@implementation MyNotificationHandler

- (id)initWithEventOut:(int)fd {
  self.eventOut = fd;
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
  printf("registering\n");
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
  int paintIn = 0;  //stdin
  int eventOut = 1; //stdout

  fprintf(stderr, "VIDEO Hello World\n");

  if(argc < 2){ // by default, spawn the core application
    spawnCore(&paintIn, &eventOut);
    close(0);
    dup(paintIn);
    close(1);
    dup(eventOut);
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
    

  id window = [[[MyWindow alloc] 
    initWithContentRect:NSMakeRect(0,0,640,480)
    styleMask:NSTitledWindowMask|NSClosableWindowMask|NSMiniaturizableWindowMask|NSResizableWindowMask
    backing:NSBackingStoreBuffered
    defer:NO
  ] autorelease];
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
