fungsi: src
	cabal build
	fyne-cross windows -arch=386
	cp fyne-cross/bin/windows-386/fungsi.exe fungsi.exe
	rm -rf fyne-cross
	rm Icon.png
