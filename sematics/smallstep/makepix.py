import re
from subprocess import call

f = open('bits.tex','r')
s = f.read()
f.close()

p = re.compile (r"\\bit{(.*?)}\s*%beginbit(.*?)%endbit", re.DOTALL)
k = 0
for (name,bit) in p.findall (s):
	bitfile = open ('outputs/bit.tex','w')
	bitfile.write(bit)
	bitfile.close()
	call (['latex', 'presentationbits.tex'])
	call (['dvipng', '-T', 'tight', '-D', '480', '-Q', '7', '-o', 'temp.png', 'presentationbits.dvi'])
	call (['mv', 'temp.png', 'outputs/'+name+'.png'])
