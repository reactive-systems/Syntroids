include ../build.cfg

TSL2TLSF    = $(TSLTOOLS)tsl2tlsf
AAG2CLASH   = $(TSLTOOLS)cfm2code
TSLCHECK    = $(TSLTOOLS)tslcheck

SOURCES     := $(wildcard *.tsl)
TLSFFILES   := $(patsubst %.tsl,%.tlsf,$(SOURCES))
BOSYFILES   := $(patsubst %.tsl,%.bosy,$(SOURCES))
AIGERFILES  := $(patsubst %.tsl,%.aag,$(SOURCES))
CLASHFILES  := $(patsubst %.tsl,%.hs,$(SOURCES))
BCLASHFILES := $(addprefix bosy/,$(CLASHFILES))
SCLASHFILES := $(addprefix strix/,$(CLASHFILES))

##### tsltools #####

check:
	@ $(TSLCHECK) $(SOURCES)

%.tlsf : %.tsl
	$(TSL2TLSF) $< > $@

tlsf: $(TLSFFILES)

##### strix #####

strix: strixdir $(TLSFFILES) $(AIGERFILES) $(SCLASHFILES)
	@echo $(SCLASHFILES)

%.aag : %.tlsf
	$(STRIX) $< -o $@

strix/%.hs : %.aag
	$(AAG2CLASH) clash $< -o $@
strixdir:
	if [ ! -d "strix" ]; then mkdir -p strix; fi 

##### bosy #####

bosy:bosydir $(TLSFFILES) $(BOSYFILES) $(BCLASHFILES)
	@echo $(BCLASHFILES)

%.bosy : %.tlsf
	$(BOSY) --synthesize $< > $@

bosy/%.hs : %.bosy
	$(AAG2CLASH) clash $< -o $@

bosydir:
	if [ ! -d "bosy" ]; then mkdir -p bosy; fi 

##### md5sum #####

md5:
	@md5sum $(SOURCES)

##### clean #####

clean-bosy:
	@rm -f *.tlsf
	@rm -f *.bosy
	@rm -f $(SCLASHFILES)
	@rm -f -r bosy

clean-strix:
	@rm -f *.tlsf
	@rm -f *.aag
	@rm -f $(SCLASHFILES)
	@rm -f -r strix

clean: clean-bosy clean-strix
