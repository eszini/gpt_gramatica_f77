# Definimos el objetivo "all" para construir el ejecutable
all: tgram

COMPILER  = gcc
CFLAGS    = -w
COPY     ?= 1  # Por defecto, copiar archivos (1 = copiar, 0 = no copiar)
VERSION   =  10001

BIN_DIR := $(HOME)/bin
SRC_BCK := $(HOME)/bck1
SRC_DIR := $(HOME)/wrk/tgram/A_src

# Objetivo setup para crear directorios si no existen
setup:
	@echo "Checking and creating directories if they do not exist..."
	@[ -d $(BIN_DIR) ] || (mkdir -p $(BIN_DIR) && echo "Created directory: $(BIN_DIR)")
	@[ -d $(SRC_DIR) ] || (mkdir -p $(SRC_DIR) && echo "Created directory: $(SRC_DIR)")

# Ver version del makefile
version:
	@echo "Makefile - version $(VERSION)"

# Regla para construir el ejecutable "tgram"
tgram: tgram.c
	@echo "Compilando tgram.c..."
	@${COMPILER} ${CFLAGS} -o tgram tgram.c 2> errors.log
	@if [ ! -s errors.log ]; then \
		echo "Compilacion ok."; \
		if [ "$(COPY)" -eq "1" ]; then \
			echo "Copiando tgram.c a src..."; \
			cp tgram.c $(SRC_DIR); \
			echo "Copiando Makefile a src..."; \
			cp tgram.c $(SRC_BCK); \
			echo "Copiando Makefile a backup..."; \
			cp Makefile $(SRC_DIR); \
			echo "Copiando ejecutables a bin..."; \
			if [ -e tgram ]; then \
				cp tgram $(BIN_DIR); \
				echo "Executable tgram copied to $(BIN_DIR)"; \
			else \
				echo "Executable tgram not found."; \
			fi; \
			if [ -e tgram.exe ]; then \
				cp tgram.exe $(BIN_DIR); \
				echo "Executable tgram.exe copied to $(BIN_DIR)"; \
			else \
				echo "Executable tgram.exe not found."; \
			fi; \
		else \
			echo "Skipping file copy"; \
		fi; \
	else \
		echo "Errores encontrados durante la compilación:"; \
		cat errors.log; \
	fi
	@rm -f errors.log

# Regla para copiar archivos desde A_src
get:
	@if [ "$(file)" ]; then \
		echo "Copiando $(file) desde $(SRC_DIR) a $(CURDIR)..."; \
		cp $(SRC_DIR)/$(file) .; \
	else \
		echo "Por favor, especifica el archivo a copiar. Ejemplo: make get file=tgram.c"; \
	fi

# Regla para editar tgram.c
vi: tgram.c
	vi tgram.c

# Regla para hacer limpieza
clean:
	@echo "Haciendo limpieza ... "
	@rm -f *.exe *.log log *.err log? *.chr tool.sta tgram 

