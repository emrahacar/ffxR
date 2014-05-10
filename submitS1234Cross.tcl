#!env tclsh

set I [glob train2???.rda.S1234.???.???.in]
set J [glob trainAll.???.???.in]
set nrun 0
foreach outType { Derece Hiz S } {
foreach i $I {
    set ilist [split $i .]
    set j [join [list trainAll [lindex $ilist 3] [lindex $ilist 4] [lindex $ilist 5]] .]
	set train $i
	set test $j
	regsub {\.in} $train .$outType trainout
	regsub {\.in} $test .$outType testout
	regsub {\.in} $i .col  colNames
	incr nrun
	set ndir s1234run$nrun
	if {[file isdirectory $ndir]==1} continue
	file mkdir $ndir
	puts "$ndir"
        puts "$train $trainout $test $testout"
	cd $ndir
	set f [open README w]; 
	puts $f "output $outType"
	puts $f "train  $train"
	puts $f "test   $test"
	close $f
        exec ln -sf ../runffx.py
        exec ln -sf  ../FFX.py
        set cmd "python2.7 ./runffx.py test ../$train ../$trainout ../$test ../$testout ../$colNames"
        puts $cmd
        flush stdout
	set f [open cmd w];     puts $f $cmd;    close $f
	exec chmod 755 cmd
	catch { 
	    eval "exec ./cmd | tee log.$nrun "
	}
	set cmd2 "R --no-save < ../paretoCrossEval.R "
	puts $cmd2
	flush stdout
	set f [open cmd2 w];     puts $f $cmd2;    close $f
	exec chmod 755 cmd2
	catch { 
	    eval "exec ./cmd2 | tee log.paretoCrossEval.$nrun "
	}
	cd ..
}
}


