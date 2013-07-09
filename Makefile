all:
	erl -make

app:
	cp src/enigma.app.src ebin/enigma.app

clean:
	rm -rf ebin/*.beam erl_crash.dump
