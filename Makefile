build:
	stack build --fast

clean:
	stack clean

until:
	until stack build; do echo eek; done

make tests:
	stack test --fast
