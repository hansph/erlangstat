# erlangstat

A simple collection of statistical algorithms, functions to work with tables and a simple csv reader and writer.
Input is a list of numbers.

There are no external dependencies, just pure Erlang.

## stat.erl
Basic statistics:

* minmax/1 - basic statistics put into a record of type #stats
* print/1 - pretty print a #stats record
* avg/1 - average
* variance/1
* variance_welford/1 - variance using the welford algorithm
* median/1
* median_s/1 - median of a sample
* stddev/1 - standard deviation
* stddev_sample/1 - standard deviation of a sample
* sample_error/1 - standard error
* covar/2 - covariance
* covar_sample/2 - covariance of a sample
* correlation/2

## csv.erl
Read/write a csv file into a list of lists (a "table")

* read/1, read/2, - read a csv file
* write_list/2, write_list/3 - write to a file

## table.erl
Functions for easier handling of lists of lists as created by the csv reader.

exports type table().

* get_col/2, get_col_if/3 - extract a column from a table
* print/1
* append_row/2 - appends a row
* get_row/1 - return the nth row
* subtable/2 - extract rows into a new table

## exampl1.erl
An example on how to use the library
