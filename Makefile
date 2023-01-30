BG=$(HOME)/jeux/bg/game
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
	rm -rf $(BG)/sim_out
	RUST_BACKTRACE=1 ./infinity_compiler -G $(BG) -L 5 -O a.log show sw1h03.itm
	RUST_BACKTRACE=1 ./infinity_compiler -G $(BG) -L 5 -O a.log compile
