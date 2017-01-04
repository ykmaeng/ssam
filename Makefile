#REBAR = $(shell pwd)/rebar
REBAR = rebar

.PHONY: deps rel stagedevrel

all: deps compile

fall: fcompile

compile: 
	$(REBAR) compile

fcompile: 
	$(REBAR) skip_deps=true compile

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

distclean: clean devclean relclean
	$(REBAR) delete-deps

test:
	$(REBAR) skip_deps=true eunit

rel: all
	$(REBAR) generate

relclean:
	rm -rf rel/ssam

proxystart:
	  @/usr/local/Cellar/haproxy/1.4.21/sbin/haproxy -f dev.haproxy.conf

frel: fcompile
	$(REBAR) generate

###
### Docs
###
docs:
	#$(REBAR) skip_deps=true doc
	@$(foreach svc, $(wildcard apps/ssam*), cd $(svc); $(REBAR) skip_deps=true doc; cd ../..;)

##
## Developer targets
##

stage : rel
	$(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf rel/ssam/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/ssam/lib;)


stagedevrel: dev1 dev2 dev3
	$(foreach dev,$^,\
	  $(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf dev/$(dev)/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) dev/$(dev)/lib;))

devrel: dev1 dev2 dev3

dev1rel: dev1 

devclean:
	rm -rf dev

dev1 dev2 dev3: fall
	mkdir -p dev
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@.config)

fdevrel: fall
	@$(foreach dev, $(wildcard dev/*), cp -R apps/ssam/ebin/*.beam $(dev)/lib/ssam-0.1.0/ebin/;)
	@$(foreach dev, $(wildcard dev/*), cp -R apps/ssam_account/ $(dev)/lib/ssam_account-0.1.0/;)
	@$(foreach dev, $(wildcard dev/*), cp -R apps/ssam_message/ $(dev)/lib/ssam_message-0.1.0/;)
	@$(foreach dev, $(wildcard dev/*), cp -R apps/ssam_telephony/ $(dev)/lib/ssam_telephony-0.1.0/;)
	@$(foreach dev, $(wildcard dev/*), cp -R apps/ssam_storage/ $(dev)/lib/ssam_storage-0.1.0/;)
	@$(foreach dev, $(wildcard dev/*), cp -R apps/ssam_monitor/ $(dev)/lib/ssam_monitor-0.1.0/;)

depreciated:
	@$(foreach svc, $(wildcard apps/ssam_*), cp -R $(svc)/priv/ apps/ssam/priv/;)
	@$(foreach svc, $(wildcard apps/ssam_*), cp -R $(svc)/include/ apps/ssam/include/;)

	@$(foreach dev, $(wildcard dev/*), cp apps/ssam*/ebin/*.beam $(dev)/lib/ssam-proto/ebin/;)
	@$(foreach dev, $(wildcard dev/*), cp -R apps/ssam/priv/ $(dev)/lib/ssam-proto/priv/;)
	@$(foreach dev, $(wildcard dev/*), cp -R apps/ssam/include/ $(dev)/lib/ssam-proto/include/;)

##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.ssam_combo_dialyzer_plt

check_plt: deps compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

build_plt: deps compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

dialyzer: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin


cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)
