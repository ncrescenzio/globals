#!/bin/bash
#
# compute realtive lp-errors between timedata   
#

#
# usage ./lp_error.sh approx exact [pnorm] [weight] 
#
# where:
# exact  [in     ] :: exact solution path
# approx [in     ] :: approximate solution path
# pnorm  [in, opt] :: pnorm_use (default=2.0) 
# weight [in, opt] :: weight
# result : time evolution of absolute errors.
#

#
# read inputs
# 
approx=$1
exact=$2
p_norm=$3
weight=$4

#
# compute difference
# 

dir=`dirname "$0"`
repo=`eval "cd $dir/../../;pwd;cd - > /dev/null"`

$repo/globals/axpy_timedata/axpy.out -1.0 $approx $exact err.dat 

#
# check data dimension
# 
dim_head=$(head -1 $exact)
array=( $dim_head)
dim=${array[0]}
#
if [ "$dim" == 1 ];
then   
    $repo/globals/lp_norm_timedata/lp_norm_timedata.out err.dat ${p_norm} ${weight}
    rm err.dat
else
    $repo/globals/vector2magnitude_timedata/vector2magnitude_timedata.out err.dat err_magn.dat
    $repo/globals/lp_norm_timedata/lp_norm_timedata.out err_magn.dat ${p_norm} ${weight}
    rm err.dat
    rm err_magn.dat
fi


