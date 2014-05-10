#!env tclsh

set I [glob train2???.rda.???.???.in]
set nrun 0
foreach outType { Derece Hiz S } {
foreach i $I {
    foreach j $I {
	set train $i
	set test $j
	regsub {\.in} $train .$outType trainout
	regsub {\.in} $test .$outType testout
	regsub {\.in} $i .col  colNames
	incr nrun
	set ndir xrun$nrun
#	if {[file isdirectory $ndir]==1} continue
	file mkdir $ndir
	puts "$ndir"
	cd $ndir
	set f [open README w]; 
	puts $f "output $outType"
	puts $f "train  $train"
	puts $f "test   $test"
	close $f

	set cmd "R --no-save < ../paretoCrossEval.R "
	puts $cmd
	flush stdout
	set f [open cmd w];     puts $f $cmd;    close $f
	exec chmod 755 cmd
	catch { 
	    eval "exec ./cmd | tee log.$nrun "
	}

	cd ..
}
    
    }
}
