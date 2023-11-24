m_list_mask=("ACCESS-CM2,r1i1p1f1" "ACCESS-ESM1-5,r1i1p1f1" "CanESM5,r1i1p1f1" "CMCC-ESM2,r1i1p1f1" "CNRM-CM6-1-HR,r1i1p1f2" "CNRM-CM6-1,r1i1p1f2" "EC-Earth3,r1i1p1f1" "EC-Earth3-Veg,r1i1p1f1" "EC-Earth3-Veg-LR,r1i1p1f1" "FGOALS-g3,r1i1p1f1" "INM-CM4-8,r1i1p1f1" "INM-CM5-0,r1i1p1f1" "IPSL-CM6A-LR,r1i1p1f1" "MIROC6,r1i1p1f1" "MPI-ESM1-2-HR,r1i1p1f1" "MPI-ESM1-2-LR,r1i1p1f1" "MRI-ESM2-0,r1i1p1f1" "NorESM2-LM,r1i1p1f1" "NorESM2-MM,r1i1p1f1" "TaiESM1,r1i1p1f1")

#vars="tasmax tx3x"
vars="tx3x"
for var in $vars; do
    for gcm in "${m_list_mask[@]}"; do
        IFS=, read gcm iter <<<"${gcm}"
   # printf "%s %s\n" "${name}" "${url}"
        echo "${gcm}" "${iter}"
         ff="/scratchx/cbarnes/WWW_data/mediterranean_heat_20230428/${gcm}/${var}*"
    #echo $ff
   
        old_path=`echo $ff | awk '{print $2}'`
        file_name="$(basename -- $old_path)"
        last_file_ts=`echo $file_name | awk -F"_" '{print $1 "_" $2 "_"  $3 "_" $4 "_ssp585_" $6 "_" "19400101-20991231.nc" }'`
        echo $last_file_ts

        cdo mergetime $ff /scratchx/cbarnes/WWW_data/mediterranean_heat_20230428/tmp/${var}_ts_${gcm}_${iter}.nc
        cdo selyear,1940/2099 /scratchx/cbarnes/WWW_data/mediterranean_heat_20230428/tmp/${var}_ts_${gcm}_${iter}.nc /scratchx/cbarnes/WWW_data/mediterranean_heat_20230428/TS/${var}/${last_file_ts}
    done
done 
