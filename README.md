# Syntroids
A [game](https://www.react.uni-saarland.de/publications/GHKF19.pdf) synthesized for FPGAs from [Temporal Stream Logic](https://www.react.uni-saarland.de/publications/FKPS19a.pdf)

## Requirements

### TSL-tools
You need the TSL-toolchain to convert TSL specifications to the Temporal Logic Synthesis Format (TLSF) and to convert the control flow model to code of a [FRP-framework](https://www.react.uni-saarland.de/publications/FKPS19b.pdf).
- https://github.com/reactive-systems/tsltools

### Synthesis tool
You need a TLSF-compatible LTL-synthesis tool. We used the following ones
- [Strix](https://strix.model.in.tum.de/)
- [Bosy](https://github.com/reactive-systems/bosy)

Note that different synthesis tools might vary in recource usage and the size of the outputed control flow model.

### Clash
You need the Clash HDL used as the FRP-Framework, generating verilog code:
- https://github.com/clash-lang/clash-compiler

Due to active development of Clash, we recommend to
`` git checkout fff460634d80db6f4add2b887cea22c2d937fc35``
before building the compiler.

### Hardware
You may not need the following tools depending on whether you want work on generated verilog code or rebuild the whole physical system.

#### Yosys
You need the Yosys Open SYnthesis Suite to synthesize the generated verilog code.
- https://github.com/YosysHQ/yosys

#### Nextpnr
You need nextpnr a vendor neutral, timing driven, FOSS FPGA place and route tool.
- https://github.com/YosysHQ/nextpnr

#### Icestorm
You need the IceStorm tools.
- https://github.com/cliffordwolf/icestorm

#### Icotools
You need the icoprog programming tool for IcoBoards.
- https://github.com/cliffordwolf/icotools

## Usage
We include a [Makefile](src/specifications/Makefile), be sure to configure a [build.cfg](src/build.cfg.sample). It features the following functionality:
- `make tlsf` converts all TSL specifications to TLSF
- `make check` executes `tslcheck` on all TSL specifications 
- `make <filename>.aag` synthsizes the specification `<filename>.tsl` using strix
- `make strix/<filename>.hs` generates corresponding Clash code, plus the above if not already synthesized
- `make <filename>.bosy` synthsizes the specification `<filename>.tsl` using bosy
- `make bosy/<filename>.hs` generates corresponding Clash code, plus the above if not already synthesized

After acquiring a `.hs` file for every specification, copy them to the [clash](src/clash) folder. You can then compile the game to Verilog code using a simple `make`.
