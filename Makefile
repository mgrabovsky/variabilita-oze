CONTAINER_MANAGER=podman
IMAGE_NAME=shiny-variabilita-oze
SHINY_PORT=8080

default:
	@echo 'Available targets:'
	@echo '    build-container'
	@echo '    run-container'

build-container:
	$(CONTAINER_MANAGER) build --no-cache \
		--tag $(IMAGE_NAME):latest .

run-container:
	$(CONTAINER_MANAGER) run --rm --interactive --tty \
		--publish $(SHINY_PORT):8080 \
		$(IMAGE_NAME):latest

.PHONY: build-container default run-container

