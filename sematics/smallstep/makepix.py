import re
import bisect
from subprocess import call

f = open('presentationbits.tex','r')
s = f.read()
f.close()

lits = re.finditer (r"%beginlit(.*?)%endlit", s, re.DOTALL)
bits = re.finditer (r"%beginbit(.*?)%endbit", s, re.DOTALL)

litdict = {l.start() : l.group(1) for l in lits}

k = 0
for bit in bits:
	name = str(k)
	k = k + 1
	fullfile = litdict.copy()
	fullfile[bit.start()] = bit.group(1)

	tempname = 'temp'
	temptexfile = tempname+'.tex'
	tempdvifile = tempname+'.dvi'
	temppngfile = tempname+'.png'

	bitfile = open (temptexfile,'w')
	for (loc,text) in iter(sorted(fullfile.items())):
		bitfile.write(text)	
	bitfile.close()
	
	call (['latex', temptexfile])
	call (['dvipng', '-T', 'tight', '-D', '480', '-Q', '7', '-bg', 'Transparent', '-o', temppngfile, tempdvifile])
	call (['mv', temppngfile, 'outputs/'+name+'.png'])
	call (['rm', tempdvifile, temptexfile])
