m_list_mask=("ACCESS-CM2,r1i1p1f1" "ACCESS-ESM1-5,r1i1p1f1" "CanESM5,r1i1p1f1" "CMCC-ESM2,r1i1p1f1" "CNRM-CM6-1-HR,r1i1p1f2" "CNRM-CM6-1,r1i1p1f2" "EC-Earth3,r1i1p1f1" "EC-Earth3-Veg,r1i1p1f1" "EC-Earth3-Veg-LR,r1i1p1f1" "FGOALS-g3,r1i1p1f1" "INM-CM4-8,r1i1p1f1" "INM-CM5-0,r1i1p1f1" "IPSL-CM6A-LR,r1i1p1f1" "MIROC6,r1i1p1f1" "MPI-ESM1-2-HR,r1i1p1f1" "MPI-ESM1-2-LR,r1i1p1f1" "MRI-ESM2-0,r1i1p1f1" "NorESM2-LM,r1i1p1f1" "NorESM2-MM,r1i1p1f1" "TaiESM1,r1i1p1f1")

var="tasmax"
scen="ssp585"
for gcm in "${m_list_mask[@]}"; do
    IFS=, read gcm iter <<<"${gcm}"
   # printf "%s %s\n" "${name}" "${url}"
    echo "${gcm}" "${iter}"
    #ls   /bdd/CMIP6/ScenarioMIP/*/${gcm}/${scen}/${iter}/day/${var}/*/latest/${var}*
    find /bdd/CMIP6/ScenarioMIP/*/${gcm}/${scen}/${iter}/day/${var}/*/latest/${var}*  -name "${var}_*" > ${var}_${scen}_${gcm}.txt
    
    cat  ${var}_${scen}_${gcm}.txt | while read path_file
    do
        echo "The path_file is : $path_file"
        indir=`dirname $path_file`
        file_name="$(basename -- $path_file)"
        echo $file_name
        new_file=`echo $file_name | awk -F"_" '{print $1 "_mask_" $2 "_"  $3 "_" $4 "_" $5 "_" $6 "_" $7 }'`
        new_file_ts=`echo $file_name | awk -F"_" '{print $1 "_ts_" $2 "_"  $3 "_" $4 "_" $5 "_" $6 "_" $7 }'`
        tx7x_file_ts=`echo $file_name | awk -F"_" '{print "tx7x" "_ts_EU_" $3 "_" $4 "_" $5 "_" $6 "_" $7 }'`

        echo $new_file
        echo $new_file_ts
        echo $tx7x_file_ts

        out1="/scratchx/cbarnes/WWW_data/NH_heat_202307/europe/tmp"
        outdir=`echo $new_file | awk -F"_" '{print   "/scratchx/cbarnes/WWW_data/NH_heat_202307/europe/" $4}'`

        echo "The destination folder is : $outdir"
        if [ ! -d "$outdir" ]; then
        # Control will enter here if $DIRECTORY doesn't exist.
            echo "The directory is not there"
            mkdir -p ${outdir}
        fi
        cdo mul $path_file /scratchx/cbarnes/WWW_data/mediterranean_heat_20230428/mask/ocean_mask_fx_${gcm}.nc ${out1}/${new_file}
        cdo -subc,273.15 -fldmean -sellonlatbox,-5,25,36,45 ${out1}/${new_file}  ${outdir}/${new_file_ts}
        cdo yearmax -runmean,7 ${outdir}/${new_file_ts} ${outdir}/${tx7x_file_ts} 
    done
done
