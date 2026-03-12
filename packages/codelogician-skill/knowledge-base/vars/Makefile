SOURCES := $(filter-out merged.yaml,$(wildcard *.yaml))

merged.yaml: $(SOURCES)
	yq eval-all '. as $$item ireduce ({}; . * $$item)' $(SOURCES) > $@
