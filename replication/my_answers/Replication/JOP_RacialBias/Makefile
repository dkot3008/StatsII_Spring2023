RDIR= ./analysis
DATADIR= ./data
FIGDIR= ./figs

# list R files
RFILES := $(wildcard $(RDIR)/*.R)

# Indicator files to show R file has run
OUT_FILES := $(RFILES:.R=.Rout)

all:	$(OUT_FILES)

# May need to add something here if some R files depend on others.

# RUN EVERY R FILE
$(RDIR)/%.Rout:	$(RDIR)/%.R $(RDIR)/sensitivity-functions.R
	R CMD BATCH --no-save $< 

# Run R files
R:	$(OUT_FILES)

# Clean up stray files
clean:
	rm -fv $(OUT_FILES)

.PHONY: all clean
