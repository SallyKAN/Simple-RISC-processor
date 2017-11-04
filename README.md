# Simple-RISC-processor
This is my project for computer architecture subject. 
This aims to implement a complete single-cycle processor which can execute a real program, also, a 2-stage pipelined processor is then developed based on that, in which the first stage is instruction fetch and the second stage implements the register fetch, execute, memory and writeback parts.

Hope it can be helpful 
# Download 
`git clone https://github.com/username/Simple-RISC-processor.git`
## part1
Implement a single cycle RISCV processor which can execute the program below. The program calculates the first 30 Fibonacci numbers and writes the last one to a fixed memory address.
## part2
The single cycle processor is modified to implement a 2-stage pipelined processor where the first stage is instruction fetch and the second stage implements the register fetch, execute, memory and writeback parts. Branch delay slot is used in machine code. 
## part3
Implement a speculate and kill scheme for branches to execute the original machine code.
