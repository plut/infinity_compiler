BG=$(HOME)/jeux/bg/game
all: run

r: run
run:
	rm -rf $(BG)/sim_out
	cargo r -- -g -G $(BG)

c: check
check:
	cargo c --color=always 2>&1 |head -30

w: watch
watch:
	cargo watch -s 'clear;cargo c --color=always 2>&1 | head -30'
