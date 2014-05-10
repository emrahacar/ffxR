#!/bin/env python2.7
"""
runffx.py v1.3 (Sept 16, 2011)

This module is a toolkit for command-line testing of the Fast Function Extraction (FFX) algorithm.

Reference: Trent McConaghy, FFX: Fast, Scalable, Deterministic Symbolic Regression Technology, Genetic Programming Theory and Practice IX, Edited by R. Riolo, E. Vladislavleva, and J. Moore, Springer, 2011.  http://www.trent.st/ffx
"""

"""
FFX Software Licence Agreement (like BSD, but adapted for non-commercial gain only)

Copyright (c) 2011, Solido Design Automation Inc.  Authored by Trent McConaghy.
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
    * Usage does not involve commercial gain. 
    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    * Neither the name of the associated institutions nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

For permissions beyond the scope of this license, please contact Trent McConaghy (trentmc@solidodesign.com).

THIS SOFTWARE IS PROVIDED BY THE DEVELOPERS ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE DEVELOPERS OR THEIR INSTITUTIONS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

Patent pending.
"""

import csv, os, sys, time, types

import numpy

import FFX

USAGE = """
Fast Function Extraction (FFX) toolkit.

Tools:
  runffx test -- given x/y training data and x/y test data, build model then calculate its test rmse 
  runffx splitdata -- split x/y datafiles into separate datafiles for training and testing
  runffx aboutdata -- gives the number of variables and samples for a given x 
  runffx help -- shows this string
 
Type 'runffx TOOLNAME' with no arguments to get help for that tool, e.g. 'runffx test'.
"""


def runmain(args):
    """This is the main routine that is called from a lightweight command-line application."""

    #case: no extra args, so return help
    if len(args) == 0:
        print USAGE
        return

    toolname = args[1]
    if toolname == 'help':
        print USAGE
        return

    elif toolname == 'splitdata':
        splitdata(args)
        return
    
    elif toolname == 'aboutdata':
        aboutdata(args)
        return

    elif toolname == 'test':
        testffx(args)
        return
    
    else:
        print "There is no toolname of '%s'." % toolname
        print USAGE
        return



def splitdata(args):
    help = """
Usage: runffx splitdata INPUTS_FILE.csv OUTPUTS_FILE.csv
   
Given csv-formatted inputs and outputs files, splits them into training and testing data files
of the form INPUTS_FILE_train.csv, OUTPUTS_FILE_train.csv, INPUTS_FILE_test.csv, OUTPUTS_FILE_test.csv.

Sorts the data in ascending y.  Assigns every fourth value to test data; and rest to train data.

In the csv files, there is one column for each sample point.  The inputs files have one 
row for each input variable.  The outputs files have just one row total, because the 
output is scalar.  Values in a given row are separated by spaces.
"""
        
    #got the right number of args?  If not, output help
    args = args[1:] #remove 'runffx.py'
    num_args = len(args)
    if num_args == 1 or (num_args == 2 and args[1] == 'help'):
        print help; return
    if num_args not in [3]:
        print help
        print 'Got %d args, need 3.' % num_args
        return

    #yank out the args
    X_file = args[1]
    if not (X_file.endswith('.csv') or X_file.endswith('.txt')):
        print "SAMPLES_IN file '%s' needs to end with .csv or .txt.  Early exit." % X_file
        return
    if not os.path.exists(X_file):
        print "SAMPLES_IN file '%s' does not exist.  Early exit." % X_file
        return
    
    y_file = args[2]
    if not (y_file.endswith('.csv') or y_file.endswith('.txt')):
        print "SAMPLES_OUT file '%s' needs to end with .csv or .txt.  Early exit." % y_file
        return
    if not os.path.exists(y_file):
        print "SAMPLES_OUT file '%s' does not exist.  Early exit." % y_file
        return

    #create the target output filenames, and ensure they don't exist
    train_X_file = addPathPrefix('train_', X_file)
    train_y_file = addPathPrefix('train_', y_file)
    test_X_file = addPathPrefix('test_', X_file)
    test_y_file = addPathPrefix('test_', y_file)
    for newfile in [train_X_file, train_y_file, test_X_file, test_y_file]:
        if os.path.exists(newfile):
            print "New file '%s' exists, and should not.  Early exit." % newfile
            return

    #report what we're working with
    print "Begin runffx splitdata.  INPUTS_FILE.csv=%s, OUTPUTS_FILE.csv=%s" % (X_file, y_file)
    
    #create X, y
    X = csvToArray(X_file) #[var_i][sample_i] : float
    Y = csvToArray(y_file) # [sample_i] : float
    if Y.shape[0] != 1 and Y.shape[1] != 1:
        print "The y data needs to be one row or one column.  Exiting". sys.exit(0)
    elif Y.shape[0] == 1:
        y = Y[0,:]
    else:
        y = Y[:,0]

    if X.shape[1] != y.shape[0]:
        X = X.T
    assert X.shape[1] == y.shape[0]

    #create train/test data from X,y
    I = numpy.argsort(y)
    test_I, train_I = [], []
    for (loc, i) in enumerate(I):
        if loc % 4 == 0: test_I.append(i)
        else:            train_I.append(i)
    
    train_X = numpy.take(X, train_I, 1)
    train_y = numpy.take(y, train_I)
    test_X = numpy.take(X, test_I, 1)
    test_y = numpy.take(y, test_I)

    print "There will be %d samples in training data, and %d samples in test data" % (len(train_y), len(test_y))
    
    #dump to file
    arrayToCsv(train_X, train_X_file)
    vecToCsv(train_y, train_y_file)
    
    arrayToCsv(test_X, test_X_file)
    vecToCsv(test_y, test_y_file)

    #done
    print "Created these files:"
    print "  Training inputs:  %s" % train_X_file
    print "  Training outputs: %s" % train_y_file
    print "  Testing inputs:   %s" % test_X_file
    print "  Testing outputs:  %s" % test_y_file
    print ""
    print "Done runffx splitdata."

def aboutdata(args):
    help = """
Usage: runffx aboutdata SAMPLES_IN.csv

Simply prints the number of variables and number of samples for the given ascii database.

"""    
    #got the right number of args?  If not, output help
    args = args[1:] #remove 'runffx.py'
    num_args = len(args)
    if num_args == 1 or (num_args == 2 and args[1] == 'help'):
        print help; return
    if num_args not in [2]:
        print help
        print "Got %d arguments; need 2." % num_args
        return

    #yank out the args        
    X_file = args[1]
    
    #get raw data
    (num_vars, num_samples) = xFileInfo(X_file)

    #print info
    print "Data file: %s" % X_file
    print "Number of input variables: %d" % num_vars
    print "Number of input samples: %d" % num_samples


def testffx(args):
    help = """
Usage: runffx test TRAIN_IN.csv TRAIN_OUT.csv TEST_IN.csv TEST_OUT.csv [VARNAMES.csv]

-Builds a model from training data TRAIN_IN.csv and TRAIN_OUT.csv.
-Computes & prints test nmse using test data TEST_IN.csv TEST_OUT.csv.
  -Also outputs the whole pareto optimal set of # bases vs. error in a .csv

Arguments:
  TRAIN_IN.csv -- model input values for training data
  TRAIN_OUT.csv -- model output values for training data
  TEST_IN.csv -- model input values for testing data
  TEST_OUT.csv -- model output values for testing data
  VARNAMES.csv (optional) -- variable names.  One string for each variable name.

In the training and test files, there is one column for each sample point.  The inputs 
files have one row for each input variable.  The outputs files have just one row total, 
because the output is scalar.  Values in a given row are separated by spaces.
"""
    
    #got the right number of args?  If not, output help
    args = args[1:] #remove 'runffx.py'
    num_args = len(args)
    if num_args == 1 or (num_args == 2 and args[1] == 'help'):
        print help; return
    if num_args not in [5, 6]:
        print help
        print '\nGot %d args.  Need 5 or 6.' % num_args
        return
    
    #yank out the args
    train_X_file, train_y_file, test_X_file, test_y_file = args[1:5]
    if num_args == 6:
        varnames_file = args[5]
    else:
        varnames_file = None

    #report what we're working with
    print "Begin ffx test."

    #get X/y
    train_X = csvToArray(train_X_file) #[var_i][sample_i] : float
    test_X = csvToArray(test_X_file)   #[var_i][sample_i] : float
    train_y = csvToVec(train_y_file) #[sample_i] : float
    test_y = csvToVec(test_y_file)   #[sample_i] : float
    min_y = min(min(train_y), min(test_y))
    max_y = max(max(train_y), max(test_y))

    #get varnames
    if varnames_file:
        varnames = csvRowToStrings(varnames_file)
    else:                                
        varnames = ['x%d' % i for i in xrange(train_X.shape[0])]

    #build models
    start_time = time.time()
    models = FFX.MultiFFXModelFactory().build(train_X, train_y, test_X, test_y, varnames)

    #output to uniquely-named csv
    time_s = str(time.time()).replace('.','')
    output_csv = 'pareto_front_%s.csv' % time_s
    f = open(output_csv, 'w')
    f.write('%10s, %13s, %s\n' % ('Num bases', 'Test error (%)', 'Model'))
    for model in models:
        f.write('%10s, %13s, %s\n' % 
                ('%d' % model.numBases(), '%.4f' % (model.test_nmse * 100.0), model))
    f.close()

    end_time = time.time()
    elapsed_time = end_time - start_time
    print "Done.  Runtime: %.1f seconds.  Results are in: %s" % \
        (elapsed_time, output_csv)


#=================================================================================
#utility functions
def addPathPrefix(prefix, pathname):
    """Given a filename like 'dirA/dirB/filename.ext', returns 'dirA/dirB/$prefixfilename.ext'
    """
    new_pathname = os.path.join(os.path.dirname(pathname), prefix + os.path.basename(pathname))
    return new_pathname

def xFileInfo(filename):
    """Returns the number of rows and columns in the file."""
    delim = getDelimiter(filename)
    f = open(filename, 'r')
    reader = csv.reader(f, delimiter=delim)
    num_rows = 0
    for (row_i, row) in enumerate(reader):
        if row_i == 0: #ignore empty strings (e.g. at end of row)
            num_cols = len([val for val in row if val])
        num_rows += 1
    f.close()
    return (num_rows, num_cols)

def csvToArray(filename):
    """Return a 2d array version of the input csv file."""
    (num_rows, num_cols) = xFileInfo(filename)
    X = numpy.zeros((num_rows, num_cols), dtype=float) #[row_i][col_i] : float
    delim = getDelimiter(filename)
    f = open(filename, 'r')
    reader = csv.reader(f, delimiter=delim)
    for (row_i, row) in enumerate(reader):
        col_i = 0
        for val in row:
            if val: #ignore empty strings (e.g. at end of row)
                X[row_i, col_i] = float(val)
                col_i += 1
    f.close()
    return X

def csvToVec(filename):
    """Return a 1d array version of the input csv file.  Input file must have just 1 row."""
    X = csvToArray(filename)
    assert X.shape[0] == 1, 'file %s must have 1 row' % filename
    y = X[0,:]
    return y

def arrayToCsv(X, filename):
    """Create an ascii file, where each row of 2d array X is a different line."""
    assert len(X.shape) == 2
    assert isinstance(filename, types.StringType), filename.__class__
    
    try:
        f = open(filename, 'w')
        writer = csv.writer(f) # default to Excel CSV dialect
        for row_i in xrange(X.shape[0]):
            writer.writerow(X[row_i,:])
        f.close()
    except (IOError, OSError), o:
        raise ValueError('Error while writing ASCII file %s: %s'
                         % (filename, o))


def getDelimiter(filename):
    """Auto-determines whether the entry delimiter is ',' or ' '"""
    f = open(filename, 'r')
    line = f.readline()
    f.close()
    if ',' in line:
        return ','
    else:
        return ' '

def vecToCsv(y, filename):
    """Create an ascii file, where each row of 1d array y is a different line."""
    assert len(y.shape) == 1, "v needs to be a 1d array"
    X = numpy.reshape(y, (1, len(y)))
    arrayToCsv(X, filename)


def csvRowToStrings(filename):
    """Extracts and returns a list of strings from the first row of the file."""
    f = open(filename, 'r')
    line = f.readline()
    f.close()
    strings = line.split()
    return strings

#=================================================================================
if __name__ == '__main__':
    if len(sys.argv) == 1:
        print USAGE
        sys.exit(0)

    runmain(sys.argv) #main call

