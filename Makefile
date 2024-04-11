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

b: build
build:
	cargo b

init: build
	$(RUN) -O init.log init -B

save: build
	$(RUN) -O save.log save

c: clippy
clippy:
	cargo clippy --color=always 2>&1 | less -R

w: watch
watch:
	cargo watch -s 'clear;cargo c --color=always 2>&1 | head -30'

# low-level (SQL) test
sqltest:
	rm -rf $(BG)/simod_out
#  	$(RUN) -O init.log init -B
	echo "update \"items\" set price=5,name='A new name for Albruin' where id='sw1h34';" | sqlite3 $(DB)
	echo "select * from save_items where id='sw1h34'"|sqlite3 game.sqlite
	echo "select * from save_items_abilities where root='sw1h34'"|sqlite3 game.sqlite
	echo "select * from strref_dict"|sqlite3 game.sqlite
#  	$(RUN) -O show.log show sw1h34.itm
#  	$(RUN) -O add.log add target
	$(RUN) -O save.log save

# high-level (Lua) test
t: test
test:
	cargo build
	@-echo "delete from add_items" | sqlite3 game.sqlite
	@-echo "delete from add_items_abilities" | sqlite3 game.sqlite
	@-echo "delete from add_items_effects" | sqlite3 game.sqlite
	@-echo "delete from edit_items" | sqlite3 game.sqlite
	@-echo "delete from edit_items_abilities" | sqlite3 game.sqlite
	@-echo "delete from edit_items_effects" | sqlite3 game.sqlite
#  	$(RUN) -O init.log init -B
	$(RUN) -O add.log add phony_target
	@-echo "select * from add_items" | sqlite3 game.sqlite
	@-echo "select parent,id from items_abilities where parent in (select id from add_items)" |sqlite3 game.sqlite
	@-echo "select parent,root,id,opcode from items_effects where root in (select id from add_items)" |sqlite3 game.sqlite

restore:
	$(RUN) restore
