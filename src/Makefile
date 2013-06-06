EXTRA_DIST=README
SUBDIRS=compartments mediators drivers

.PHONY: default components mediators drivers

default: components mediators drivers

compartments:
	$(MAKE) -C components

mediators:
	$(MAKE) -C mediators

drivers:
	$(MAKE) -C drivers
