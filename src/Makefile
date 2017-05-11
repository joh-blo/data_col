include ../vsn.mk

# ----------------------------------------------------
# VARIABLES
# ----------------------------------------------------

APP_NAME = data_col

MODULES = \
	data_col \
	dc_app \
	dc_sup \
	dc_manager \
	dc_handler \
	dc_db \
	dc_lib

INCLUDES= \
	../include


TARGET_FILES = $(APP_TARGET) $(EMAKE_TARGET)

include ../../meadow/priv/Makefile.vars


# ----------------------------------------------------
# BUILD TARGETS
# ----------------------------------------------------
all: $(TARGET_FILES) 
	erl -make

clean:
	rm -f $(APP_TARGET) $(EMAKE_TARGET) ../ebin/*.beam


doc: edoc


include ../../meadow/priv/Makefile.rules


