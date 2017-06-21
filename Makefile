# A simple makefile for creating the CCSI Pulverization distribution
VERSION := 2014.10.00
PRODUCT := Pulverization
LICENSE := CCSI_TE_LICENSE_$(PRODUCT).txt
PKG_DIR := CCSI_$(PRODUCT)_$(VERSION)
PACKAGE := $(PKG_DIR).tgz

# Where Jenkins should checkout ^/projects/common/trunk/
COMMON := .ccsi_common

LEGAL_DOCS := LEGAL \
           CCSI_TE_LICENSE.txt

PAYLOAD := model \
     results \
     test \
     LEGAL \
     $(LICENSE)

# Get just the top part (not dirname) of each entry so cp -r does the right thing
PAYLOAD_TOPS := $(foreach v,$(PAYLOAD),$(shell echo $v | cut -d'/' -f1))
# And the payload with the PKG_DIR prepended
PKG_PAYLOAD := $(addprefix $(PKG_DIR)/, $(PAYLOAD))

# OS detection & changes
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  MD5BIN=md5sum
endif
ifeq ($(UNAME), Darwin)
  MD5BIN=md5
endif
ifeq ($(UNAME), FreeBSD)
  MD5BIN=md5
endif

.PHONY: all clean

all: $(PACKAGE)

$(PACKAGE): $(PAYLOAD)
	@mkdir $(PKG_DIR)
	@cp -r $(PAYLOAD_TOPS) $(PKG_DIR)
	@tar -cf - $(PKG_PAYLOAD) | gzip -n > $(PACKAGE)
	@$(MD5BIN) $(PACKAGE)
	@rm -rf $(PKG_DIR) $(LICENSE) $(LEGAL_DOCS)

$(LICENSE): CCSI_TE_LICENSE.txt
	@sed "s/\[SOFTWARE NAME \& VERSION\]/$(PRODUCT) v.$(VERSION)/" < CCSI_TE_LICENSE.txt > $(LICENSE)

$(LEGAL_DOCS):
	@if [ -d $(COMMON) ]; then \
	  cp $(COMMON)/$@ .; \
	else \
	  svn -q export ^/projects/common/trunk/$@; \
	fi

clean:
	@rm -rf $(PACKAGE) $(PKG_DIR) $(LICENSE) $(LEGAL_DOCS)
