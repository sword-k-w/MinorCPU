# minor-cpu

## How to Get Verilog Version of Our Code

The main part of our cpu is written in Chisel. To get verilog version, run `sbt "runMain CPUMain --target-dir generated"` at root directory. `CPU.v` will generated in folder `generated`.

**Always use big endian in README!** 

eg: 0010 means 2th-low bit is 1.

## 5-staged pipeline

### ALU

#### Input

* in1  : UInt(32.W)
* in2  : UInt(32.W)
* type : Byte(4.W) {func7[1] | func3} 
  eg : sll -> 0001; sra -> 1101

#### Output

* out  : result(32.W)