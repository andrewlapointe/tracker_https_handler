# Makefile for Cowboy Handler Application

# Project paths
REBAR := rebar3

# SSL certificate and key paths (adjust if necessary)
CERT_FILE := /path/to/cert.pem
KEY_FILE := /path/to/key.pem

.PHONY: all compile run clean

all: clean compile

compile:
	@echo "Compiling the application..."
	$(REBAR) compile

run: compile
	@echo "Starting the Cowboy application..."
	$(REBAR) compile

clean:
	@echo "Cleaning up compiled files..."
	$(REBAR) clean
	rm -rf _build

release:
	@echo "Building a release..."
	$(REBAR) release

deploy: release
	@echo "Deploying the release..."
	# Add deployment commands here (e.g., copy files to your server)

test:
	@echo "Running tests..."
	$(REBAR) eunit
