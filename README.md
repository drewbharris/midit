MidiT - play standard MIDI files
================================

**midit** is a command-line utility that plays the specified MIDI file(s) to
one or more ALSA sequencer ports. It is derived from *aplaymidi(1)* and is
capable of pausing, accelerating and slowing down tempo, seeking backwards
and forwards, displaying information such as position in track in ticks or
in 'minutes:seconds' format. It can also display the midi events being read.

Apologies for poor code and bugs; I wrote this very early in my programming
career. Feel free to fix any or all things in need of fixing.

Dependencies
------------
Requires alsa (Advanced Linux Sound Architecture - www.alsa-project.org).

Installing
----------
To build, run:
```
make
```
To install, run:
```
make install
```

Help
----
```
man ./midit.1
```
