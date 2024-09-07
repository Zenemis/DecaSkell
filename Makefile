# Nom du compilateur Haskell
HC = ghc

# Options de compilation
HC_OPTS = -Wall -O2 -odir $(OBJ_DIR) -hidir $(OBJ_DIR) -v

# Répertoire des sources
SRC_DIR = src

# Répertoire des objets
OBJ_DIR = obj

# Assignation des commandes shell
ifeq ($(OS), Windows_NT)
	MKDIR = mkdir 
else
	MKDIR = mkdir -p
endif

# Création du répertoire obj
$(shell $(MKDIR) "$(OBJ_DIR)")

# Répertoire générique pour NKHelp
NKHELP_DIR = $(SRC_DIR)/Lexer/NKHelp

# Fichiers sources
SOURCES = 	$(SRC_DIR)/Lexer/Header.hs \
			$(wildcard $(NKHELP_DIR)/*.hs) \
			$(SRC_DIR)/Lexer/NonKeyword.hs \
			$(SRC_DIR)/Lexer/Lexer.hs \
			$(SRC_DIR)/DecaSkell.hs 

# Fichiers objets correspondants
OBJECTS = $(SOURCES:$(SRC_DIR)/%.hs=$(OBJ_DIR)/%.o)

# Fichier exécutable
EXECUTABLE = DecaSkell

# Règle par défaut
all: $(EXECUTABLE)

# Compilation de l'exécutable
$(EXECUTABLE): $(OBJ_DIR) $(OBJECTS)
	$(HC) $(HC_OPTS) -o $(EXECUTABLE) $(OBJECTS)

# Compilation des fichiers objets
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.hs 
	$(info $(dir $@))
	$(shell $(MKDIR) "$(dir $@)")
	$(HC) $(HC_OPTS) -c $< -o $@

# Nettoyage des fichiers générés
clean:
	rm -rf $(OBJ_DIR) $(EXECUTABLE)

# Nettoyage complet (y compris l'exécutable)
dist-clean: clean

.PHONY: all clean dist-clean
