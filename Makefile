LOADLIBES=-lm
CUR_DATE := $(shell date +"%Y_%m_%d")
DEPLOY_DIR=$(PWD)/plancknull_generate_html_$(CUR_DATE)
DEPLOY_FILE=$(PWD)/plancknull_generate_html_$(CUR_DATE).zip
ADDITIONAL_LIBRARIES=-lz -lpthread

INPUT_FILES=generator.scm \
	user-settings.scm \
	json-utils.scm \
	file-utils.scm \
	html-gen-utils.scm \
	fitsio.scm \
	healpix.scm

CHICKEN_INSTALL=chicken-install
CHICKEN_CSC=csc
CHICKEN_EGGS_TO_DEPLOY=json packrat html-tags html-utils matchable \
	list-utils check-errors stack shell filepath \
	directory-utils numbers record-variants srfi-29 locale \
	regex lookup-table posix-utils condition-utils \
	variable-item srfi-19
CHICKEN_EGGS_TO_INSTALL=$(CHICKEN_EGGS_TO_DEPLOY) schematic

.phony: all deploy install_eggs help documentation

all: generator documentation

generator: $(INPUT_FILES)
	$(CHICKEN_CSC) $< -o $@ -lcfitsio $(ADDITIONAL_LIBRARIES)

deploy: $(DEPLOY_DIR)/generator

install_eggs:
	@for chicken_module in $(CHICKEN_EGGS_TO_INSTALL); do \
		$(CHICKEN_INSTALL) $$chicken_module; \
	done

$(DEPLOY_DIR)/generator: $(INPUT_FILES)
	mkdir -p $(DEPLOY_DIR)
	csc -deploy -o $(DEPLOY_DIR) $<
	@for chicken_module in $(CHICKEN_EGGS_TO_DEPLOY); do \
		$(CHICKEN_INSTALL) -deploy -p $(DEPLOY_DIR) $$chicken_module; \
	done
	cp -rf css $(DEPLOY_DIR) # Copy this directory as well
	zip -r $(DEPLOY_FILE) $(DEPLOY_DIR)

documentation: \
	docs/generator.scm.html \
	docs/user-settings.scm.html \
	docs/json-utils.scm.html \
	docs/file-utils.scm.html \
	docs/html-gen-utils.scm.html

docs/%.scm.html: %.scm
	schematic -f markdown --directory docs $<

help:
	@echo "Available targets:"
	@echo ""
	@echo "   generator       Build the executable in the current directory"
	@echo "   deploy          Build a standalone executable in"
	@echo "                  " $(DEPLOY_DIR)
	@echo "                   (this takes some time)"
	@echo "   install_eggs    Install any egg required by Chicken to"
	@echo "                   compile/deploy the source code and to produce"
	@echo "                   the documentation (you might need to use"
	@echo "                   'sudo', i.e. 'sudo make install_eggs')"
	@echo "   documentation   Create the documentation in ./docs"
