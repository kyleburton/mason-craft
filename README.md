# Overview

The goal is for Mason to learn about Software Development, aka Programming, aka Software Engineering.  This is a HUGE domain.

I (Kyle) learned programming outside of an academic context (iow, I didn't study any of it at school/college/univeristy).

The most impactful advice I can offer is that your success will most impacted by whether or not you pick yourself up and try again.  You're going to face failure (not knowing things) over and over.  If you can learn even one small thing from each time something you try doesn't work, if you do that enough times, you'll learn how to do this.

There are lots of different programming languages.  They are [legion](http://www.99-bottles-of-beer.net/).  The first ones were created in the 1940's and 1950's during and just after World War II (see (Admiral Grace Murray Hopper)[https://en.wikipedia.org/wiki/Grace_Hopper] who helped shape and define Software Engineering), and new programming languages are created on a regular basis ((Go)[https://golang.org/] and (Rust)[https://www.rust-lang.org/] are two very recent and very prominent languages that were creatd relatively recently, 2009 and 2010).

If you're going to learn programming, there are some languages you should be aware of based on how widely used they are:

* JavaScript, which is built into every Web Browser, which you can start programming with right now by opening your browser's DeveloperTools.
* C (yes the single letter) is a lower level langauge, nearly every operating system you'll ever use is written in C. The code that runs at a low level in your phone, your laptop, the code in your TV, your network router, etc.  C is frequently used when software has to do its job in as little time as possible and with as little memory as possible.
* Assembly, this is even lower level than C
* Machine Code - this is what runs on the CPUs, all programming languages end up executed as machine code
* Java, not related to JavaScript in any way.  This is what the MineCraft server is implemented in, though I suspect MicroSoft will change this in the future.  This programming language is typically used to create Internet Based Services and other servers, it can be used to create GUI (Grahical User Inerface) programs, though isn't as good at that.
* C Sharp (C#) this is like MicroSoft's version of Java, it is great at creating both GUI applications and services, though works best on Windows and not as well on the Mac or Linux


Lets see if we can extend or otherwise automate some Minecraft.  I've played around with that in the past using CanaryMod (from the book "Learn to Program with Minecraft Plugins - Create Flaming Cows in Java Using CanaryMod") though after MicroSoft bought Mojang, they've shut down many of the freely available libraries like CanaryMod and Bukkit.


```
CanaryMod: https://github.com/CanaryModTeam/CanaryMod
Status:    Inactive since 2015, does not currently build

Bukkit:    https://getbukkit.org/
Download:  Must Be Built

  now called CraftBukkit

Spigot:    https://getbukkit.org/
Download:  Must Be Built

It looks like Spigot is a MineCraft Server with CraftBukkit integrated into it:

  https://www.spigotmc.org/wiki/what-is-spigot-craftbukkit-bukkit-vanilla-forg/

Lets see where we can get with Spigot: https://www.spigotmc.org/wiki/buildtools/

```

Things you will need to install or setup:

* An Editor or IDE (Integrated Development Environment)
  * Programming is complex, editors for writing code are complex, IMO, there is no escaping this
  * I use [Emacs](https://www.spigotmc.org/wiki/buildtools/) or [Vim](https://www.spigotmc.org/wiki/buildtools/), though these are hideously, diabolically, difficult to learn, they have served me well my entire life.
  * [Atom](https://atom.io/) might be a simpler starting point, though I don't know it so I won't be able to advise you on how to best use it.
  * There are lots of free Editors and lots of editors you have to pay for.  I have very little experience with the ones you have to pay for.
  * [IntelliJ IDEA](https://atom.io/) Lots of people I've worked with like this IDE.  The Community Edition is free.  This is a very complex tool as well and in many ways does more than Emacs or VIM.  I have very little expeirence with this IDE though.
* [Home Brew](https://www.spigotmc.org/wiki/buildtools/)
  * Brew will let us install lots of freely available programmer's tools, like `bake`
* [bash](https://linuxconfig.org/bash-scripting-tutorial-for-beginners) is also known as the "Unix Shell" or "Terminal" that you'll frequently see.  Your Mac has one of these, called `Terminal`.  This is where you'll type many of the commands you'll see related to programming.  It's worthwhile learning more about this.  Interstingly enough `bash` is a programming language!
* [Spigot](https://www.spigotmc.org/wiki/buildtools/)


### `brew`

```
brew install mvn vim git emacs
brew install kyleburton/kyleburton/bake
```
