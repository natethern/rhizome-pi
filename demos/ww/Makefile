STATIC=
#STATIC=-static -aux -lrhzww
#STATIC=-static -aux rhzww.lib

WWOPTION0=-windows -xm debugger $(STATIC) rp_wwutl: rp_wwapi: rp_wwbas:
WWOPTION1=$(WWOPTION0) rp_wwui:
WWOPTION2=$(WWOPTION1) rp_wwdlg:
WWOPTION3=$(WWOPTION2) rp_wwtrm:

all: minimal.exe hello1.exe hello2.exe hello3.exe scrib.exe calcpad.exe life.exe

minimal.exe: minimal.scm
	pisc minimal.scm
	pisl -o minimal $(WWOPTION0) minimal

hello1.exe: hello1.scm
	pisc hello1.scm
	pisl -o hello1 $(WWOPTION0) hello1

hello2.exe: hello2.scm
	pisc hello2.scm
	pisl -o hello2 $(WWOPTION0) hello2

hello3.exe: hello3.scm
	pisc hello3.scm
	pisl -o hello3 $(WWOPTION1) hello3

scrib.exe: scrib.scm
	pisc scrib.scm
	pisl -o scrib $(WWOPTION2) scrib

calcpad.exe: calcpad.scm
	pisc calcpad.scm
	pisl -o calcpad $(WWOPTION3) calcpad

life.exe: life.scm
	pisc life.scm
	pisl -o life $(WWOPTION2) life

clean:
	rm -f *.[co] *.obj *.exe
