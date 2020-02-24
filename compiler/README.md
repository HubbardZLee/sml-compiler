# xMips - a very basic emulator of MIPS to use in a compiler construction course

## Purpose
The simulator here has been designed to go with the homework sequence of Andrew Appel's 
course [Compiling Techniques](https://www.cs.princeton.edu/courses/archive/spring12/cos320/schedule.html "Course Schedule").

The code generation stage, 
[Assignment 7](https://www.cs.princeton.edu/courses/archive/spring12/cos320/homeworks/hw7/),
involves generating code in an IR created by extending MIPS architecture with an arbitrary
number of virtual registers (hence xMips).

Problem: You can't test the generated code because there is no simulator for xMips. Students need to
perform liveness analysis, 
[Assignment 8](https://www.cs.princeton.edu/courses/archive/spring12/cos320/homeworks/hw8/), 
graph coloring and register allocation 
[Assignment 9](https://www.cs.princeton.edu/courses/archive/spring12/cos320/homeworks/hw9/),
and assemble it all together 
[Assignment 10](https://www.cs.princeton.edu/courses/archive/spring12/cos320/homeworks/hw10/),
to finally be able to test their generated code.

This repo provides a simulator that can be used to run the xMips programs. The simulator is pretty
basic, providing only signed arithmetic operations and does not limit to 32 bit numbers. It is
not aimed to be a complete and true emulator. It is aimed at helping test a first compiler
written for a course.

## Setting up

Assuming you have followed Appel's assignment sequence, add the files from the repo. Be aware.
You'd be overwriting 'compile.sml', so you should do this step AFTER you have gotten
[as7.zip](https://www.cs.princeton.edu/courses/archive/spring12/cos320/homeworks/hw7/as7.zip).

You will also need to modify 'sources.cm'. Add the following at the bottom:

```
    emulator.sml 
    words.sml
    xmips.sml
```

## Using

First, develop codegen.sml. Once you are ready to test. Load the program code, as you would normally.

```
- CM.make "sources.cm";
```
Now activate the xMips simulator using the following command.

```
- xMips.run();
```
You'd see a new command prompt.

**xMips>**

Now you will give xMips commands, listed below.

But first, to get back to the SML prompt, do the following.

**xMips>** exit

## xMips commands

### To compile a file, say sample1.fun

**xMips>** compile sample1.fun 

This will compile the file. If there is no compile error, it will also load the program in xmips memory.

### To see the program loaded in memory

**xMips>** list

The command lists 10 instructions (by default), starting from address 0. Successive list commands starts
printing from where the previous left.

If you want to print from a specific label, give the label as an argument, as follows.

**xMips>** list \_main

This will print 10 instructions (by default), starting from label \_main.

You may wish a different number of instructions. In such case, use the version with two arguments.

**xMips>** list \_main 5

This also changes the default count to 5. Subsequent "list" with not argument or with label argument uses this count.


### To run the program loaded in memory

Once a program is loaded in memory, you run it as follow.

**xMips>** run

The run command starts the program from the entry point, which is the label main. Emphsize: the
run command always starts from the entry point, including if you do so after a breakpoint (see below).

To restart after a breakpoint, use 'continue'.

### To set a breakpoint at a label

**xMips>** break \_main

Now when you run the program end execution reaches a break point, the emulator will stop.
You may continue execution using the continue command or step command.

### To set a breakpoint at an address

You may wish to break an instruction that has no label. In such case, do the following.

**xMips>** breaki 500

Notice the trailing 'i' after break. It stands for 'immediate'. The number 500 represents
the address of the instruction address to break at. The address is a decimal number, and is the same as that given by the list command.

### To continue execution after a breakpoint

**xMips>** continue

Remember, 'run' always starts (or restarts) the program from the beginning. 
In contrast, continue starts after a breakpoint.

### To single step the execution one instruction at a time

**xMips>** step

You would typically use this after reaching a breakpoint.

### To remove all breakpoints

**xMips>** clear

There is no way at this time to remove a specific break point.

### To see the content of a register

**xMips>** display $s5

The above will display content of register $s5. You can give more than one register, separated by space. Such as.

**xMips>** display $s5 $t0 $x21

In the above $x21 is a 'virtual' register provided by xmips. A virtual register must be active to be able to see its value.

### To display the content of memory location

**xMips>** displayi 100

The above will display the content of memory location 100. 
You can give a sequence of addresses to see the result of multiple addresses, such as:

**xMips>** displayi 100 104 108

The addresses are mapped to the lower word boundary. This, 101, 102, and 103 are mapped to 100.

The content of memory is typed. If you display the value in a location containing instruction
you'll see the instruction. If the location has a string, you'll get the string.

### To turn execution trace on and off

When the emulator is executing, if the trace is turned on it will print each instruction as it executes.
This can be useful when coupled with breakpoint. The execution will be displayed
until a breakpoint is reached.

**xMips>** trace on

Traces can be long and exhausting. You can turn the trace off as follows.

**xMips>** trace off

## Command shorthand

CLI's are pain. And having to type long commands makes them more painful. For convenience, all
of the commands have a short form as well.

|command | shorthand |
|--------|----------|
| break | b |
| breaki | bi |
| compile | c |
| continue | c |
| display | d |
| displayi | di |
| list | l |
| run | r |
| step | s |
| trace | t |

Yes, the shorthand 'c' is for both compile and continue.
The ambiguity is resolved by the parameter. When accompanied with
a parameter it is treated as the compile command.
