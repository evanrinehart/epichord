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
  printf("mouse down, talk to event pipe\n");
}

- (void)mouseUp:theEvent {
  printf("mouse up, talk to event pipe\n");
}

- (void)rightMouseDown:theEvent {
  printf("right mouse down, talk to event pipe\n");
}

- (void)rightMouseUp:theEvent {
  printf("right mouse up, talk to event pipe\n");
}

- (void)mouseMoved:theEvent {
  printf("mouse moved, talk to event pipe\n");
}

- (void)keyDown:theEvent {
  printf("key down, talk to event pipe\n");
}

- (void)keyUp:theEvent {
  printf("key up, talk to event pipe\n");
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

int main (int argc, const char * argv[])
{
  //spawnCore();
  [NSAutoreleasePool new];
  [NSApplication sharedApplication];
  [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
  ((NSApplication*)NSApp).delegate = [MyDelegate new];
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
