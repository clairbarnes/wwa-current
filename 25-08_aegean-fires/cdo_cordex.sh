#!/bin/bash
# module load cdo

# update domain & variable name manually
varnm=$1
xn=$2
xx=$3
yn=$4
yx=$5
fpath=$6     # where are the files coming from (root directory)
outpath=$7   # where should the output be saved


# loop over files and extract subset
while read m; do
  echo $m
  gcm=${m%%/*}
  
  mkdir -p $outpath
   
  # loop over historical runs; only subset if starting in 1950 or earlier 
  fl_hist=`ls $fpath/$m/$varnm/*.nc`
  for fnm in $fl_hist; do
    # if (( ${fnm: (-20):4} > 1949 )); then
      new_fnm=$outpath/$varnm${fnm##*$varnm}
      # make sure the target file doesn't already exist
        if [ ! -f $new_fnm ]; then
          cdo sellonlatbox,$xn,$xx,$yn,$yx $fnm $new_fnm
        # fi          
    fi
  done

#   # loop over scenario runs; only subset if ending in 2030 or later 
#   fl_rcp=`ls $fpath/$m/$varnm/*.nc`
#   for fnm in $fl_rcp; do
#     # if (( ${fnm: (-11):4} < 2031 )); then
#       new_fnm=$outpath/$varnm${fnm##*$varnm}
#         # make sure the target file doesn't already exist
#         if [ ! -f $new_fnm ]; then
#           cdo sellonlatbox,$xn,$xx,$yn,$yx $fnm $new_fnm
#         fi 
#     # fi
#   done
done <cordex-models.txt