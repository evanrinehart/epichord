# Epichord

The plan: a piano-roll sequencer and UI toolkit experiment. The idea is there
are three parts which do three things well. The sound server written in C
responds to the audio system in a lock-free manner, feeding it sound or
midi data. The window itself is a native program with only enough functionality
to draw primitive graphics and notify the core application of user input.
The rest is the most complex part, but contains nothing platform specific.
The core program is a clean workspace for experiments in UI programming in
Haskell.

![Design Diagram]
(http://i.imgur.com/sbZ015b.png)



