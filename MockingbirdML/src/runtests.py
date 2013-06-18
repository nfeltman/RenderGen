import os

# Runtime.runExperiment DacrtRenderer.render ("",["../models/teapot.obj","500","../outputs/testrenderD.ppm"]);

testrange = [
#(1,30),
#(2,10),
#(3,50),
#(5,30),
(6,20),
]

print "COMPILING ALL RENDERERS"
for (i,_) in testrange :
	s = str(i)
	outfile = "gen"+s+".sml"
	makefile = "tests/make"+s+".cm"
	
	os.system("sml @SMLload=bin/compiler-image.x86-win32 "+s+" tests/"+outfile)
	
	f = open(makefile, 'w')
	cmContents = "Group is \n\t"+outfile+"\n\t../runtime/runtime.cm\n"
	f.write(cmContents)
	f.close()

print "LINKING ALL RENDERERS"
os.chdir("tests") # safer to avoid path seperators
os.environ["CM_VERBOSE"] = "false"
for (i,_) in testrange :
	print "Linking renderer "+str(i)+"..."
	makefile = "make"+str(i)+".cm"
	executable = "heap"+str(i)
	cmd = "ml-build.bat "+makefile+" GeneratedRenderer.render "+executable
	os.system(cmd)

print "RUNNING ALL RENDERERS"
for (i,p) in testrange :
	print "Running renderer "+str(i)+"..."
	s = str(i)
	outfile = "pic"+s+".ppm"
	cmd = "sml @SMLload=heap"+s+".x86-win32 ../../models/teapot.obj "+str(p)+" "+outfile
	os.system(cmd)