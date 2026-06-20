include ../../icons.mk

PRIV := ../../../priv
DEST_PATH := $(PRIV)/icons/32/generic

.PHONY: directories

all: \
		directories \
    $(DEST_PATH)/beacon.rgba \
    $(DEST_PATH)/go_back.rgba \
    $(DEST_PATH)/info.rgba \
    $(DEST_PATH)/misc_box.rgba \
    $(DEST_PATH)/new_mail.rgba \
    $(DEST_PATH)/open_mail.rgba \
    $(DEST_PATH)/mail_doc.rgba

directories: $(DEST_PATH)

MKDIR_P := mkdir -p

$(DEST_PATH):
	$(MKDIR_P) $(DEST_PATH)

$(DEST_PATH)/%.rgba: %.png
	$(MINRGB) rgba8888 $< $@
