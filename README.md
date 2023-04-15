# Welcome!

This directory contains the single file source code for running tkadle. Currently, this program is only written for Linux installation.


# Installation

Download "tkadle.tcl" to any location, then ensure the file permissions allow for execution.


## Dependencies

Installation and running of tkadle requires TCL and TK. The installation of these packages is outside the scope of this README. Refer to package installation notes for guidance.

| Package                  | Download URL                                   |
| -                        | -                                              |
| **Tcl** (8.6.x versions) | http://sourceforge.net/projects/tcl/files/Tcl/ |
| **Tk** (8.6.x versions)  | http://sourceforge.net/projects/tcl/files/Tcl/ |


## Installing for MacOS and Windows

tkadle has not been ported to MacOS or Windows. No interest yet exists for porting this program to other operating systems.


# Running

**tkadle** is intended to be run from the command line where the file format is specified. For example, given a markdown formatted list in "todo.txt" the command line to edit with this application would be:

> tkadle.tcl -syn markdown todo.txt
