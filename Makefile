BG=$(HOME)/jeux/bg/game
TARGET=./infinity_compiler
DB=./game.sqlite
RUN=RUST_BACKTRACE=1 $(TARGET) -G $(BG) -L 5
all: run

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
	$(RUN) -O init.log init
	echo "update \"items\" set price=5,name='A new name for Albruin' where itemref='sw1h34';" | sqlite3 $(DB)
	$(RUN) -O show.log show sw1h03.itm
	$(RUN) -O save.log save
