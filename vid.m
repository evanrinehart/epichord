// First program example

#import <Cocoa/Cocoa.h>
#import <stdio.h>
#import <unistd.h>
#import <stdlib.h>

void spawnCore(){
  puts("FORKING\n"); 
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
@end

@implementation MyDelegate

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
@property int eventPipe;

- (void)windowClosing:NSNotification;
- (void)stdinReadable:NSNotification;
- (void)windowUnminimize:NSNotification;
- (void)windowResized:NSNotification;

- (void)registerStdinReadable;
- (void)registerWindowClosing;
- (void)registerWindowUnminimize;
@end

@implementation MyNotificationHandler

- (void)windowClosing:notif {
  NSLog(@"%@", notif);
  printf("window is about to close\n");
  printf("VID GAME OVER MAN\n");
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
    fprintf(stderr, "VID stdin stream has ended. Terminating.\n");
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
  //spawnCore();
  [NSAutoreleasePool new];
  [NSApplication sharedApplication];
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

  //install delegate
  ((NSApplication*)NSApp).delegate = [MyDelegate new];

  //install notification handlers
  id standardIn = [NSFileHandle fileHandleWithStandardInput];
  MyNotificationHandler* handler = [MyNotificationHandler new];
  handler.standardIn = standardIn;
  handler.center = [NSNotificationCenter defaultCenter];
  handler.eventPipe = 3;
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
