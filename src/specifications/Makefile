include ../build.cfg

TSL2TLSF    = $(TSLTOOLS)tsl2tlsf
AAG2CLASH   = $(TSLTOOLS)cfm2code
TSLCHECK    = $(TSLTOOLS)tslcheck

SOURCES     := $(wildcard *.tsl)
TLSFFILES   := $(patsubst %.tsl,%.tlsf,$(SOURCES))

##### tsltools #####

check:
	@ $(TSLCHECK) $(SOURCES)

%.tlsf : %.tsl
	$(TSL2TLSF) $< > $@

tlsf: $(TLSFFILES)

##### strix #####

%.aag : %.tlsf
	$(STRIX) $< -o $@

strix/%.hs : %.aag
	@if [ ! -d "strix" ]; then mkdir -p strix; fi 
	$(AAG2CLASH) clash $< -o $@

##### bosy #####

%.bosy : %.tlsf
	$(BOSY) --synthesize $< > $@

bosy/%.hs : %.bosy
	@if [ ! -d "bosy" ]; then mkdir -p bosy; fi 
	$(AAG2CLASH) clash $< -o $@

##### md5sum #####

md5:
	@md5sum $(SOURCES)

##### clean #####

clean-bosy:
	@rm -f *.tlsf
	@rm -f *.bosy
	@rm -f -r bosy

clean-strix:
	@rm -f *.tlsf
	@rm -f *.aag
	@rm -f -r strix

clean: clean-bosy clean-strix
