include ../build.cfg

HSFILE   = TopEntity
NAME     = TopEntity
CLASHCMD = stack --stack-yaml $(CLASH)/stack.yaml exec clash -- --verilog -fclash-inline-limit=200
VFILE = verilog/$(HSFILE)/$(NAME)/*.v

build:
	@if [ "TopEntity.json" ]; then rm -f TopEntity.json; fi 

	$(CLASHCMD) $(HSFILE).hs
	$(YOSYS) -q -p "synth_ice40 -top $(NAME) -json $(NAME).json" $(VFILE)
	@# $(ARACHNE) -d 8k -P ct256 -p $(NAME).pcf $(NAME).blif -o $(NAME).asc
	$(NEXTPNR) --hx8k --json $(NAME).json --pcf $(NAME).pcf --asc $(NAME).asc
	$(ICEPACK) $(NAME).asc $(NAME).bin

clean:
	@rm -f -r verilog
	@rm -f *.blif
	@rm -f *.dyn_hi
	@rm -f *.dyn_o
	@rm -f *.o
	@rm -f *.hi
	@rm -f *.json
	@rm -f *.bin
	@rm -f *.asc
	@rm -f LEDMatrixInitial.txt
	@rm -f SinusTableInit_.txt

exec:
	$(ICOPROG) -p < $(NAME).bin

flash:
	$(ICOPROG) -f < $(NAME).bin


compl: clean build exec
