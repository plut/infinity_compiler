BG=$(HOME)/jeux/bg/game
TARGET=./infinity_compiler
DB=./game.sqlite
RUN=RUST_BACKTRACE=1 $(TARGET) -G $(BG) -L 5
all: run

.PHONY: all run doc watch check test

doc:
	mdbook build guide

r: run
run:
	cargo b
	$(MAKE) test

c: check
check:
	cargo c --color=always 2>&1 |head -30

w: watch
watch:
	cargo watch -s 'clear;cargo c --color=always 2>&1 | head -30'

t: test
test:
	rm -rf $(BG)/simod_out
	$(RUN) -O init.log init -B
	echo "update \"items\" set price=5,name='A new name for Albruin' where itemref='sw1h34';" | sqlite3 $(DB)
	$(RUN) -O show.log show sw1h34.itm
	$(RUN) -O add.log add target
	$(RUN) -O save.log save

x:
	echo "delete from add_items" | sqlite3 game.sqlite
	echo "delete from add_item_abilities" | sqlite3 game.sqlite
	echo "delete from add_item_effects" | sqlite3 game.sqlite
	echo "delete from edit_items" | sqlite3 game.sqlite
	echo "delete from edit_item_abilities" | sqlite3 game.sqlite
	echo "delete from edit_item_effects" | sqlite3 game.sqlite
	$(RUN) -O add.log add target
