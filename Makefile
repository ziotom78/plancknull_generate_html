LOADLIBES=-lm
DEPLOY_DIR=$(PWD)/standalone_generator

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

generator: generator.scm
	$(CHICKEN_CSC) $< -o $@

deploy: $(DEPLOY_DIR)/generator

install_eggs:
	@for chicken_module in $(CHICKEN_EGGS_TO_INSTALL); do \
		$(CHICKEN_INSTALL) $$chicken_module; \
	done

$(DEPLOY_DIR)/generator: generator.scm
	mkdir -p $(DEPLOY_DIR)
	csc -deploy -o $(DEPLOY_DIR) $<
	@for chicken_module in $(CHICKEN_EGGS_TO_DEPLOY); do \
		$(CHICKEN_INSTALL) -deploy -p $(DEPLOY_DIR) $$chicken_module; \
	done
	cp -rf css $(DEPLOY_DIR) # Copy this directory as well

documentation: docs/generator.scm.html

docs/generator.scm.html: generator.scm json-utils.scm scheme-help.scm
	schematic -f markdown --directory docs $^

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