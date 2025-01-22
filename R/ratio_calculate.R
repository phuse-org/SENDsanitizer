#!/usr/bin/env Rscript

# This code is for creating
# -----------------------------------------------------------------------------
# Date                     Programmer
#----------   --------------------------------------------------------------
# Jan-21-2025    Md Yousuf Ali (MdYousuf.Ali@fda.hhs.gov)



      ## if(ratio=='OWBR'){


calculate_ratio <- function(ratio,organ,sub,om_df,om_lat){
  # ratio OWBR, organ BRAIN
## om_df <- dt
      brain_u <- om_df[USUBJID==sub & OMTESTCD=='WEIGHT' &  OMSPEC==organ, OMSTRESU]
        brain_u <- unique(brain_u)

        brain <- om_df[USUBJID==sub & OMSPEC==organ & OMTESTCD=='WEIGHT', OMSTRESN_new]
      omsp_mg <- om_df[USUBJID==sub & OMTESTCD=='WEIGHT' & OMLAT==om_lat & OMSTRESU=='mg',OMSPEC]
      omsp_g <- om_df[USUBJID==sub & OMTESTCD=='WEIGHT' & OMLAT==om_lat & OMSTRESU=='g',OMSPEC]
        if(brain_u=='g'){
          if(length(omsp_g) > 0){
            for(one_omspec in omsp_g){

              organ_weight_g <- om_df[USUBJID==sub & OMTESTCD=='WEIGHT' & OMLAT==om_lat &
                                      OMSPEC==one_omspec, OMSTRESN_new]

      om_df[USUBJID==sub & OMTESTCD==ratio & OMLAT==om_lat & OMSPEC==one_omspec,
            `:=`(new_c=(organ_weight_g/brain)*100)]
            }


      }
      if(length(omsp_mg) > 0){
        brain_mg <- brain*1000
        for(one_omspec in omsp_mg){

          organ_weight_mg <- om_df[USUBJID==sub & OMTESTCD=='WEIGHT' & OMLAT==om_lat &
                                  OMSPEC==one_omspec, OMSTRESN_new]

      om_df[USUBJID==sub & OMTESTCD==ratio & OMLAT==om_lat & OMSPEC==one_omspec,
            `:=`(new_c=(organ_weight_mg/brain_mg)*100)]
        }
      }
        }else if(brain_u=='mg'){

          if(length(omsp_g) > 0){
            brain_g <- brain/1000
            for(one_omspec in omsp_g){
              organ_weight_g <- om_df[USUBJID==sub & OMTESTCD=='WEIGHT' & OMLAT==om_lat &
                                      OMSPEC==one_omspec, OMSTRESN_new]

      om_df[USUBJID==sub & OMTESTCD==ratio & OMLAT==om_lat & OMSPEC==one_omspec,
            `:=`(new_c=(organ_weight_g/brain_g*100))]
            }

      }
          if(length(omsp_mg) > 0){
            for(one_omspec in omsp_mg){

              organ_weight_mg <- om_df[USUBJID==sub & OMTESTCD=='WEIGHT' & OMLAT==om_lat &
                                      OMSPEC==one_omspec, OMSTRESN_new]


              om_df[USUBJID==sub & OMTESTCD==ratio & OMLAT==om_lat & OMSPEC==one_omspec,
                    `:=`(new_c=(organ_weight_mg/brain)*100)]

            }

        ## sub_bw_mg <- sub_bw*1000
      }

        }



        ## om_df[USUBJID==sub & OMTESTCD==ratio & OMLAT==om_lat, `:=`(new_c=(OMSTRESN_new/brain)*100)]
#ratio2
om_df
}
