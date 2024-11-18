.asm.obj:
	$(ASM)asm /ml $(MASM) $*;

vz.com: main.obj alias.obj char.obj core.obj disp.obj dos.obj expr.obj\
	filer.obj gets.obj harderr.obj inst.obj key.obj ledit.obj macro.obj\
	memo.obj menu.obj misc.obj open.obj printf.obj scrn.obj smooth.obj\
	string.obj text.obj view.obj wind.obj swap.obj ems.obj xscr.obj msg.obj

main.obj: vz.inc main.asm

alias.obj: vz.inc alias.asm

char.obj: vz.inc char.asm

core.obj: vz.inc core.asm

disp.obj: vz.inc disp.asm

dos.obj: vz.inc dos.asm

expr.obj: vz.inc expr.asm

filer.obj: vz.inc filer.asm

gets.obj: vz.inc gets.asm

harderr.obj: vz.inc harderr.asm

inst.obj: vz.inc inst.asm

key.obj: vz.inc key.asm key98.asm keyibm.asm dummy

ledit.obj: vz.inc ledit.asm

macro.obj: vz.inc macro.asm

memo.obj: vz.inc memo.asm

menu.obj: vz.inc menu.asm

misc.obj: vz.inc misc.asm

open.obj: vz.inc open.asm

printf.obj: sprintf.inc printf.asm

scrn.obj: vz.inc scrn.asm scrn98.asm scrnibm.asm dummy

smooth.obj: vz.inc smooth.asm

string.obj: vz.inc string.asm

text.obj: vz.inc text.asm

view.obj: vz.inc view.asm

wind.obj: vz.inc wind.asm

xscr.obj: vz.inc xscr.asm

swap.obj: vz.inc swap.asm

ems.obj: vz.inc ems.asm

msg.obj: vz.inc msg.asm dummy
