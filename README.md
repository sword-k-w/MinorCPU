# minor-cpu

## How to Get Verilog Version of Our Code

The main part of our cpu is written in Chisel. To get verilog version, run `sbt "runMain CPUMain --target-dir generated"` at root directory. `CPU.v` will generated in folder `generated`.

**Always use big endian in README!** 

eg: 0010 means 2th-low bit is 1.

## Something need to do

The assignment of entry in RoB and LSQ is incorrect. (multi-assignment)

Use the way in RS to handle it.