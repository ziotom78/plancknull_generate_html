LOADLIBES=-lm
DEPLOY_DIR=$(PWD)/standalone_generator

CHICKEN_INSTALL=chicken-install
CHICKEN_CSC=csc
CHICKEN_EGGS=json packrat html-tags html-utils matchable \
	list-utils check-errors stack shell filepath \
	directory-utils

.phony: all deploy install_eggs help

all: generator

generator: generator.scm
	$(CHICKEN_CSC) $< -o $@

deploy: $(DEPLOY_DIR)/generator

install_eggs:
	@for chicken_module in $(CHICKEN_EGGS); do \
		$(CHICKEN_INSTALL) $$chicken_module; \
	done

$(DEPLOY_DIR)/generator: generator.scm
	mkdir -p $(DEPLOY_DIR)
	csc -deploy -o $(DEPLOY_DIR) $<
	@for chicken_module in $(CHICKEN_EGGS); do \
		$(CHICKEN_INSTALL) -deploy -p $(DEPLOY_DIR) $$chicken_module; \
	done

help:
	@echo "Available targets:"
	@echo ""
	@echo "   generator       Build the executable in the current directory"
	@echo "   deploy          Build a standalone executable in"
	@echo "                  " $(DEPLOY_DIR)
	@echo "                   (this takes some time)"
	@echo "   install_eggs    Install any egg required by Chicken to"
	@echo "                   compile/deploy the source code (you might"
	@echo "                   need to invoke 'sudo make' instead of 'make')"
