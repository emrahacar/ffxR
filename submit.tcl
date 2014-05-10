#!env tclsh

set I [glob *.in]
set nrun 0
foreach i $I j $I {
    foreach outType { S Hiz Derece } {
    set train $i
    set test $j
    regsub {\.in} $train .$outType trainout
    regsub {\.in} $test .$outType testout
    regsub {\.in} $i .col  colNames
    incr nrun
    file mkdir run$nrun
    puts "run$nrun"
    cd run$nrun
    exec ln -sf ../runffx.py
    exec ln -sf  ../FFX.py
    set cmd "python2.7 ./runffx.py test ../$train ../$trainout ../$test ../$testout ../$colNames"
    puts $cmd
    flush stdout
    set f [open cmd w];     puts $f $cmd;    close $f
    exec chmod 755 cmd
    catch { 
	eval "exec ./cmd | tee log.$nrun"
    }
    cd ..
}
}

set I [lsort -decreasing [glob run*]]
foreach i $I {
    if {[file isdirectory $i]==0} {continue}
    if {[file exists [file join $i tahmin.rda]]} {continue}
    puts "evaluating run directory $i"
    cd $i
    catch { 
	eval "exec R --no-save < ../pareto.R"
    }
    cd ..
}
