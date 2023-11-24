cd waw#!/bin/bash
event=europe                 # Event name
beginobs=1980                 # Start for evaluation period
eyear=2023                    # Event year
ensemble=CMIP6                # Ensemble name
iname=tx7x                     # Index name
retup=10.                      # Return period of the event
changesign=""                 # To study low extremes
model="gev"                 # Extremes model
mode="shift"                  # scale/shift


# Directory including SGSAT covariates
cdir=/projsu/cmip-work/rvautard/DATA/SGSAT/$ensemble

# Directory including the event indicators

edir=/scratchx/ipinto/WWW_data/NH_heat_202307/europe/TS/tx7x

m_list_mask=("ACCESS-CM2,r1i1p1f1" "ACCESS-ESM1-5,r1i1p1f1" "CanESM5,r1i1p1f1" "CMCC-ESM2,r1i1p1f1" "CNRM-CM6-1-HR,r1i1p1f2" "CNRM-CM6-1,r1i1p1f2" "EC-Earth3,r1i1p1f1" "EC-Earth3-Veg,r1i1p1f1" "EC-Earth3-Veg-LR,r1i1p1f1" "FGOALS-g3,r1i1p1f1" "INM-CM4-8,r1i1p1f1" "INM-CM5-0,r1i1p1f1" "IPSL-CM6A-LR,r1i1p1f1" "MIROC6,r1i1p1f1" "MPI-ESM1-2-HR,r1i1p1f1" "MPI-ESM1-2-LR,r1i1p1f1" "MRI-ESM2-0,r1i1p1f1" "NorESM2-LM,r1i1p1f1" "NorESM2-MM,r1i1p1f1" "TaiESM1,r1i1p1f1")

# Evaluation period
q=EVAL
rm $event.RESULTS.$iname.$ensemble
#for m in $mlist ; do
for gcm in "${m_list_mask[@]}"; do
    IFS=, read m iter <<<"${gcm}"
    echo "${m}" "${iter}"
    echo MODEL ${m}  
    ls ${edir}/${iname}_ts_EU_${m}_ssp585_${iter}*_18500101-20991231.nc
    ls $cdir/SGSAT${eyear}.${m}_${iter}.hssp585.dat 
  ./attributop ${edir}/${iname}_ts_EU_${m}_ssp585_${iter}*_18500101-20991231.nc $cdir/SGSAT${eyear}.${m}_${iter}.hssp585.dat  $model assume $mode restrain 0.4 cov1 -1.2 confidenceinterval 95 blockyr 1 end2 $eyear begin $beginobs end $eyear biasrt $retup 
  cat ATTRIB-RESULTS | gawk '{printf "%-40s%-40s\n","'$m' ",$0}' >> $event.RESULTS.$iname.$ensemble
done

gawk '$2=="FIT-PARAMETERS" {print}' $event.RESULTS.$iname.$ensemble > $event.FIT-PARAMETERS.$q.$iname.$ensemble
gawk '$2=="RP-VALUE" {print}' $event.RESULTS.$iname.$ensemble > $event.RP-VALUE.$q.$iname.$ensemble
gawk '$2=="PROBABILITY-RATIO" {print}' $event.RESULTS.$iname.$ensemble > $event.PROBABILITY-RATIO.$q.$iname.$ensemble
gawk '$2=="INTENSITY-CHANGE" {print}' $event.RESULTS.$iname.$ensemble > $event.INTENSITY-CHANGE.$q.$iname.$ensemble
rm ATTRIB-RESULTS
mv $event.RESULTS.$iname.$ensemble $event.RESULTS.$q.$iname.$ensemble

# Attribution period
q=ATT
rm $event.RESULTS.$iname.$ensemble
for gcm in "${m_list_mask[@]}"; do
    IFS=, read m iter <<<"${gcm}"
    echo "${m}" "${iter}"
  echo MODEL $m
  ./attributop ${edir}/${iname}_ts_EU_${m}_ssp585_${iter}*_18500101-20991231.nc $cdir/SGSAT${eyear}.${m}_${iter}.hssp585.dat $model assume $mode restrain 0.4 cov1 -1.2 confidenceinterval 95 blockyr 1 end2 $eyear begin 1900 end $eyear biasrt $retup
  cat ATTRIB-RESULTS | gawk '{printf "%-40s%-40s\n","'$m' ",$0}' >> $event.RESULTS.$iname.$ensemble
done

gawk '$2=="FIT-PARAMETERS" {print}' $event.RESULTS.$iname.$ensemble > $event.FIT-PARAMETERS.$q.$iname.$ensemble
gawk '$2=="RP-VALUE" {print}' $event.RESULTS.$iname.$ensemble > $event.RP-VALUE.$q.$iname.$ensemble
gawk '$2=="PROBABILITY-RATIO" {print}' $event.RESULTS.$iname.$ensemble > $event.PROBABILITY-RATIO.$q.$iname.$ensemble
gawk '$2=="INTENSITY-CHANGE" {print}' $event.RESULTS.$iname.$ensemble > $event.INTENSITY-CHANGE.$q.$iname.$ensemble
rm ATTRIB-RESULTS
mv $event.RESULTS.$iname.$ensemble $event.RESULTS.$q.$iname.$ensemble

# Projection 2deg period
q=FUT2
rm $event.RESULTS.$iname.$ensemble
for gcm in "${m_list_mask[@]}"; do
    IFS=, read m iter <<<"${gcm}"
    echo "${m}" "${iter}"
    echo MODEL ${m}
  ./attributop ${edir}/${iname}_ts_EU_${m}_ssp585_${iter}*_18500101-20991231.nc $cdir/SGSAT${eyear}.${m}_${iter}.hssp585.dat $model assume $mode restrain 0.4 cov1 0.8 confidenceinterval 95 blockyr 1 end2 $eyear begin 1900 end 2050 biasrt $retup
  cat ATTRIB-RESULTS | gawk '{printf "%-40s%-40s\n","'$m' ",$0}' >> $event.RESULTS.$iname.$ensemble
done

gawk '$2=="FIT-PARAMETERS" {print}' $event.RESULTS.$iname.$ensemble > $event.FIT-PARAMETERS.$q.$iname.$ensemble
gawk '$2=="RP-VALUE" {print}' $event.RESULTS.$iname.$ensemble > $event.RP-VALUE.$q.$iname.$ensemble
gawk '$2=="PROBABILITY-RATIO" {print}' $event.RESULTS.$iname.$ensemble > $event.PROBABILITY-RATIO.$q.$iname.$ensemble
gawk '$2=="INTENSITY-CHANGE" {print}' $event.RESULTS.$iname.$ensemble > $event.INTENSITY-CHANGE.$q.$iname.$ensemble
rm ATTRIB-RESULTS
mv $event.RESULTS.$iname.$ensemble $event.RESULTS.$q.$iname.$ensemble
