Part 1 DATA prep
We need to prepare the time series for the attribution and for that we are going to use spiritx1
Login to spirtx1 (change ipinto to your username)
> ssh cbarnes@spiritx1.ipsl.fr
Create a folder to work on 
> mkdir -p WWA/NH_heat_202307_tutorial/prepare_data
# all the data we are going to create we will store it at /scratchx/ipinto
# /scratchx/login (2To and 300000 files max per user) : NO BACKUP
> mkdir -p /scratchx/ipinto/WWW_data/NH_heat_202307_tutorial/tmp
> cd WWA/NH_heat_202307_tutorial/prepare_data

# example for one model
# eg. below are command lines to prepare time series of a single model 
#""" 
ls /bdd/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/day/tasmax/gn/latest/tasmax*

#mask the ocean/add missing values in the ocean  
# the surface elevevation data is in /bdd/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/fx/sftlf/gn/latest/sftlf_fx_CanESM5_historical_r1i1p1f1_gn.nc
# we can mask the ocean by setting the sftlf values 100  to missing value and the sftlf data on land areas to 1.
#  cdo -expr,'sftlf = ((sftlf>=100)) ? 1.0 : sftlf/0.0' /bdd/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/fx/sftlf/gn/latest/sftlf_fx_CanESM5_historical_r1i1p1f1_gn.nc /scratchx/ipinto/WWW_data/mediterranean_heat_20230428/mask/ocean_mask_fx_CanESM5.nc

# mask the data 
cdo mul /bdd/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/day/tasmax/gn/latest/tasmax_day_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc /scratchx/ipinto/WWW_data/mediterranean_heat_20230428/mask/ocean_mask_fx_CanESM5.nc /scratchx/cbarnes/WWW_data/NH_heat_202307_tutorial/tmp/tasmax_mask_day_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc

#select a region, calculate area average, convert to celcius 
cdo -subc,273.15 -fldmean -sellonlatbox,-5,25,36,45 /scratchx/cbarnes/WWW_data/NH_heat_202307_tutorial/tmp/tasmax_mask_day_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc   /scratchx/cbarnes/WWW_data/NH_heat_202307_tutorial/tasmax_ts_day_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc
# Get the running mean of 7 days, get the annual maxima
cdo yearmax -runmean,7 /scratchx/cbarnes/WWW_data/NH_heat_202307_tutorial/tasmax_ts_day_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc /scratchx/cbarnes/WWW_data/NH_heat_202307_tutorial/tx7x_ts_EU_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc

#or 

find /bdd/CMIP6/CMIP/CCCma/CanESM5/historical/r1i1p1f1/day/tasmax/gn/latest/tasmax* -name "tasmax_*" > tasmax_hist_CanESM5.txt
cat tasmax_hist_CanESM5.txt | while read path_file
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
    
    out1="/scratchx/cbarnes/WWW_data/NH_heat_202307_tutorial/tmp"
    outdir=`echo $new_file | awk -F"_" '{print   "/scratchx/cbarnes/WWW_data/NH_heat_202307_tutorial/" $4}'`

    echo "The destination folder is : $outdir"
    if [ ! -d "$outdir" ]; then
       # Control will enter here if $DIRECTORY doesn't exist.
          echo "The directory is not there"
          mkdir -p ${outdir}
    fi
        cdo mul $path_file /scratchx/ipinto/WWW_data/mediterranean_heat_20230428/mask/ocean_mask_fx_${gcm}.nc ${out1}/${new_file}
        cdo -subc,273.15 -fldmean -sellonlatbox,-5,25,36,45 ${out1}/${new_file}  ${outdir}/${new_file_ts}
        cdo yearmax -runmean,7 ${outdir}/${new_file_ts} ${outdir}/${tx7x_file_ts} 
done 
#""""
# end of data preparation 

(or run the scripts....)


Part 2 Attribution part
# Once we have all the timeseries (/scratchx/ipinto/WWW_data/NH_heat_202307_tutorial/tx7x_ts_EU_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc) we are going to run the attribution part using Climate Explorer code 
Login to spirit1 (change ipinto to your username)
> ssh cbarnes@spirit1.ipsl.fr
you might need add the line below to your .bashrc (on spirit1), only once!
export LD_LIBRARY_PATH=.:/projsu/cmip-work/rvautard/CLIMEXP/gsl/lib

Create a folder to work on 
> mkdir -p WWA/NH_heat_202307_tutorial/attribution
> cd WWA/NH_heat_202307_tutorial/attribution
> ln -s /projsu/cmip-work/rvautard/CEXP/attributop
try 
> ./attributop
a message like this is supposed to show on your terminal: 
usage: attribute series covariate_series|none GEV|Gumbel|GPD|Gauss assume shift|scale mon n [sel m] [ave N] [log|sqrt] begin2 past_climate_year end2 year_under_study plot FAR_plot_file [dgt threshold%] [includelast]
 note that n and m are in months even if the series is daily.
 N is always in the same units as the series.cd
 the covariate series is averaged to the same time scale.

#if you run the below code an error about memory will appear. We are supposed to use SLURM to submit the job (https://documentations.ipsl.fr/spirit/spirit_clusters/slurm.html)
> ./attributop /scratchx/ipinto/WWW_data/NH_heat_202307_tutorial/tx7x_ts_EU_CanESM5_historical_r1i1p1f1_gn_18500101-20141231.nc /projets/WWA/DATA/SGSAT/CMIP6/SGSAT2023.CanESM5_r1i1p1f1.hssp585.dat gev assume shift restrain 0.4 cov1 -1.2 confidenceinterval 95 blockyr 1 end2 2023 begin 1900 end 2023 biasrt 10

see script run_att_1model.slm.
submit the job
> sbatch run_att_1model.slm
see jobs running 
> squeue 
see only my job 
> squeue | grep ipinto

# all of the above was an example to perform the calculations for one model. In reality we need to repeat this for many models/experiments. For that I use the scripts 
# script to prepare historical data 
1.sel_time_series_gcms_tmax_historical.sh 
# slurm script to run the above 
1.sel_time_series_gcms_tmax_historical.slm 
# script to prepare scenario data 
1.sel_time_series_gcms_tmax_scen.sh 
# slurm script to run the above 
1.sel_time_series_gcms_tmax_scen.slm 
# script to merge the historical data and future data for the annual block maxima timeseries
2.merge_ts_gcms_tx7x.sh
# slurm script to run the above  
2.merge_ts_gcms_tx7x.slm 
# script to merge the historical data and future data for the daily timeseries 
3.merge_ts_gcms_tmax.sh 
# slurm script to run the above 
3.merge_ts_gcms_tmax.slm

#script to run the attribution part 
attribute_heat_europe.sh
# slurm script to run the above 
attribute_heat_europe.slm

PS. Folder paths might not be right, you'll need to adjust! We will adapt as we go through the scripts!
