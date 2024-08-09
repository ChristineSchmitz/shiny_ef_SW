#Yield model tp4####
yield_prediction_tp_4_function<-function(fruit_drop_natural_4,
                                         fruit_drop_managed_4,
                                         storm_risk_4,
                                         drop_rate_storm_4,
                                         add_fruit_losses_4,
                                         fruits_per_tree_4,
                                         timespan_4,
                                         risk_drought_4,
                                         risk_drought_irrigation_4,
                                         percentage_tree_losses_4,
                                         risk_tree_losses_4,
                                         trees_per_ha_4,
                                         reducing_factor_diameter_increase_drought_4,
                                         fruit_diameter_4,
                                         weekly_diameter_increase_4,
                                         fruit_density_at_harvest){
  #number of fruits per tree##
  #fruit drop before harvest
  if(subset(Management_values,Management_values$management_measure=="spray_against_pre_harvest_fruit_drop")$value==0){
    pre_harvest_fruit_drop<-fruit_drop_natural_4}else{
      pre_harvest_fruit_drop<-fruit_drop_managed_4
    }
  #risk of stormy weather or heavy rain (increasing fruit drop)
  pre_harvest_fruit_drop_storm_rain<-chance_event(storm_risk_4, 
                                                  value_if = drop_rate_storm_4,
                                                  value_if_not = 0)
  
  n_fruits_per_tree_harvest_p4<-max(round(fruits_per_tree_4*
                                            (1-pre_harvest_fruit_drop)*
                                            (1-pre_harvest_fruit_drop_storm_rain))-add_fruit_losses_4,0)#Ensure that the value can't be lower than 0
  
  #tree losses (due to mice and other reasons (any time in the year))
  tree_losses_tp4_harvest<-chance_event(risk_tree_losses_4,
                                        value_if = percentage_tree_losses_4,
                                        value_if_not = 0)
  #trees per ha
  n_trees_harvest_p4<-max(round(trees_per_ha_4*(1-tree_losses_tp4_harvest)),0)
  
  #fruit weight diameter ##
  #drought
  ifelse(subset(Management_values,Management_values$management_measure=="irrigation")$value==1, drought_risk<-risk_drought_irrigation_4, drought_risk<-risk_drought_4)
  
  drought_influence<-chance_event(drought_risk,
                                  value_if = reducing_factor_diameter_increase_drought_4,
                                  value_if_not = 0,
                                  n=1) 
  
  #fruit diameter at harvest
  mean_fruit_diameter_harvest_p4<-(fruit_diameter_4+
                                     weekly_diameter_increase_4*
                                     (1-drought_influence)*
                                     (timespan_4/7))
  #fruit weight at harvest
  mean_fruit_volume_harvest_p4<-(4/3)*(mean_fruit_diameter_harvest_p4/2)^3*pi#Assuming an apple is a ball
  mean_fruit_weight_diameter_harvest_p4<-mean_fruit_volume_harvest_p4*fruit_density_at_harvest/1000 #in kg
  
  #total yield
  total_apple_yield_diameter_p4<-n_trees_harvest_p4*
    n_fruits_per_tree_harvest_p4*
    mean_fruit_weight_diameter_harvest_p4 
  
  #return 
  return(list(number_of_apple_harvested_per_tree = n_fruits_per_tree_harvest_p4,
              yield_at_harvest_diameter = total_apple_yield_diameter_p4,
              mean_fruit_weight_at_harvest_diameter = mean_fruit_weight_diameter_harvest_p4,
              mean_fruit_diameter_at_harvest = mean_fruit_diameter_harvest_p4))
}


#Yield model tp3####
yield_prediction_tp_3_function<-function(add_fruit_losses_3,
                                         lower_opt_n_fruits_3,
                                         range_opt_n_fruits_3,
                                         timespan_3,
                                         total_timespan,
                                         timespan_4,
                                         risk_drought_3,
                                         risk_drought_irrigation_3,
                                         n_fruits_per_tree_beginning_tp3,
                                         weekly_diameter_increase_3,
                                         change_diameter_increase_overbearing_3,
                                         change_diameter_increase_underbearing_3,
                                         reducing_factor_diameter_increase_drought_3,
                                         fruit_diameter_3,
                                         percentage_tree_losses_3,
                                         risk_tree_losses_3,
                                         trees_per_ha_3){
  #number of fruits per tree##
  
  #manual thinning is considered it the quality part!
  #n_fruits_per_tree_beginning_tp3 is the number of fruits after manual thinning
  n_fruits_per_tree_tp4_p3<-max(round(n_fruits_per_tree_beginning_tp3-
                                        add_fruit_losses_3),0)#Ensure that the value can't be lower than 0
  
  #under/over bearing
  underbearing_tp3<-sum(n_fruits_per_tree_beginning_tp3<lower_opt_n_fruits_3)
  overbearing_tp3<-sum(n_fruits_per_tree_beginning_tp3>(lower_opt_n_fruits_3+
                                                          range_opt_n_fruits_3))
  
  #fruit diameter##
  #weekly diameter increase
  if(overbearing_tp3==1){weekly_diameter_increase_tp3_tp4_adjusted<-weekly_diameter_increase_3*
    (1+change_diameter_increase_overbearing_3)}else if(
      underbearing_tp3==1){weekly_diameter_increase_tp3_tp4_adjusted<-weekly_diameter_increase_3*
        (1+change_diameter_increase_underbearing_3)}else{
          weekly_diameter_increase_tp3_tp4_adjusted<-weekly_diameter_increase_3
        }
  
  #drought
  ifelse(subset(Management_values,Management_values$management_measure=="irrigation")$value==1, drought_risk<-risk_drought_irrigation_3, drought_risk<-risk_drought_3)
  
  drought_influence<-chance_event(drought_risk,
                                  value_if = reducing_factor_diameter_increase_drought_3,
                                  value_if_not = 0,
                                  n=1) 
  #fruit diameter at tp4
  fruit_diameter_tp4_p3<-fruit_diameter_3+
    weekly_diameter_increase_tp3_tp4_adjusted*
    (1-drought_influence)*
    max((total_timespan-timespan_3-timespan_4),0)/7
  
  #trees per ha
  tree_losses_tp3_tp4<-chance_event(risk_tree_losses_3,
                                    value_if = percentage_tree_losses_3,
                                    value_if_not = 0)
  n_trees_tp4_p3<-max(round(trees_per_ha_3*(1-tree_losses_tp3_tp4)),0)
  
  #return
  return(list(number_of_apples_at_tp4_predicted_at_tp3 = n_fruits_per_tree_tp4_p3,
              fruit_diameter_at_tp4_predicted_at_tp3 = fruit_diameter_tp4_p3,
              n_trees_at_tp4_predicted_at_tp3= n_trees_tp4_p3))
}


#Yield model tp 2####
yield_prediction_tp_2_function<-function(fruits_per_tree_2,
                                         thinner_efficiency_2,
                                         lower_opt_n_fruits_2,
                                         range_opt_n_fruits_2,
                                         add_fruit_losses_2,
                                         june_drop_change_overbearing_2,
                                         stress_risk_2,
                                         stress_risk_irrigation_2,
                                         occourrence_bad_seed_structure,
                                         june_drop_change_bad_seed_structure_2,
                                         june_drop_change_stress_and_supply_problem_2,
                                         drop_rate_2,
                                         timespan_1,
                                         timespan_3,
                                         reducing_factor_weight_increase_stress_2,
                                         weekly_diameter_increase_2,
                                         change_diameter_increase_overbearing_2,
                                         change_diameter_increase_underbearing_2,
                                         fruit_diameter_2,
                                         reducing_factor_diameter_increase_stress_2,
                                         percentage_tree_losses_2,
                                         risk_tree_losses_2,
                                         trees_per_ha_2){
  #number of fruits per tree##
  
  #chemical fruit thinning
  if(subset(Management_values,Management_values$management_measure=="chemical_fruit_thinning")$value==1){
    n_fruits_per_tree_after_chemical_thinning<-fruits_per_tree_2*(1-thinner_efficiency_2)}else{
      n_fruits_per_tree_after_chemical_thinning<-fruits_per_tree_2
    }
  
  
  #over/under bearing
  overbearing_tp2<-sum(n_fruits_per_tree_after_chemical_thinning>(lower_opt_n_fruits_2+
                                                                    range_opt_n_fruits_2+
                                                                    n_fruits_per_tree_after_chemical_thinning*
                                                                    drop_rate_2))
  underbearing_tp2<-sum(n_fruits_per_tree_after_chemical_thinning<(lower_opt_n_fruits_2+
                                                                     n_fruits_per_tree_after_chemical_thinning*
                                                                     drop_rate_2))
  #influences on June drop rate
  if(overbearing_tp2==1){
    overbearing_influence_june_drop<-1+june_drop_change_overbearing_2
  }else{overbearing_influence_june_drop<-1}
  
  if(occourrence_bad_seed_structure!=0){seed_influence_june_drop<-(1+june_drop_change_bad_seed_structure_2)}else{
    seed_influence_june_drop<-1
  }
  
  #stress and supply problems
  ifelse(subset(Management_values,Management_values$management_measure=="irrigation")$value==1, stress_risk<-stress_risk_irrigation_2, stress_risk<-stress_risk_2)
  
  stress_and_supply_influence<-chance_event(stress_risk,
                                            value_if = 1,
                                            value_if_not = 0,
                                            n=1)
  
  ifelse(stress_and_supply_influence==1, stress_and_supply_influence_june_drop<-(1+june_drop_change_stress_and_supply_problem_2), stress_and_supply_influence_june_drop<-1)
  
  #June drop
  june_drop_rate_adj<-drop_rate_2*
    seed_influence_june_drop*
    stress_and_supply_influence_june_drop*
    overbearing_influence_june_drop
  
  june_drop<-n_fruits_per_tree_after_chemical_thinning*
    june_drop_rate_adj
  
  #number of fruits at tp3
  n_fruits_per_tree_tp3_p2<-max(round(n_fruits_per_tree_after_chemical_thinning-
                                        june_drop-
                                        add_fruit_losses_2),0)
  
  
  
  #fruit diameter##
  
  #influence of over or under bearing on diameter increase
  if(overbearing_tp2==1){weekly_diameter_increase_tp2_tp3_adjusted<-weekly_diameter_increase_2*
    (1+change_diameter_increase_overbearing_2)}else if(
      underbearing_tp2==1){weekly_diameter_increase_tp2_tp3_adjusted<-weekly_diameter_increase_2*
        (1+change_diameter_increase_underbearing_2)}else{
          weekly_diameter_increase_tp2_tp3_adjusted<-weekly_diameter_increase_2
        }
  #drought influence
  ifelse(stress_and_supply_influence==1, weekly_diameter_increase_tp2_tp3_drought_influence<-reducing_factor_diameter_increase_stress_2, weekly_diameter_increase_tp2_tp3_drought_influence<-0)
  
  #fruit diameter at tp3
  fruit_diameter_tp3_p2<-fruit_diameter_2+
    weekly_diameter_increase_tp2_tp3_adjusted*
    (1-weekly_diameter_increase_tp2_tp3_drought_influence)*
    (timespan_3-timespan_1)/7
  
  #trees per ha
  tree_losses_tp2_tp3<-chance_event(risk_tree_losses_2,
                                    value_if = percentage_tree_losses_2,
                                    value_if_not = 0)
  n_trees_tp3_p2<-max(round(trees_per_ha_2*(1-tree_losses_tp2_tp3)),0)
  
  #return
  return(list(number_of_fruits_at_tp3_predicted_at_tp2=n_fruits_per_tree_tp3_p2,
              fruit_diameter_at_tp3_predicted_at_tp2=fruit_diameter_tp3_p2,
              n_trees_at_tp3_predicted_at_tp2= n_trees_tp3_p2))
}


#Yield model tp 1####
yield_prediction_tp_1_function<-function(flower_cluster_1,
                                         flowers_per_cluster_1,
                                         risk_frost_1,
                                         frost_damage_1,
                                         frost_protection_eff_1,
                                         eff_mech_flower_thinning_1,
                                         eff_chem_flower_thinning_1,
                                         frist_drop_rate_1,
                                         exp_fruit_diameter_tp2_1,
                                         insect_damage_risk_tp1_tp2,
                                         loss_insect_damage_tp1_tp2,
                                         unfavorable_weather_during_blossom_risk,
                                         additional_fruit_drop_unfavorable_weather_tp1_tp2,
                                         fruit_drop_decrease_pollinator_support_tp1,
                                         percentage_tree_losses_1,
                                         risk_tree_losses_1,
                                         trees_per_ha_1){
  #number of flowers
  n_flowers_tp1<-flower_cluster_1*flowers_per_cluster_1
  #frost
  potential_frost_damage<-chance_event(risk_frost_1,
                                       value_if = frost_damage_1, 
                                       value_if_not = 0,
                                       n=1)
  if(subset(Management_values,Management_values$management_measure=="frost_protection")$value==1){frost_damage<-potential_frost_damage*
    (1-frost_protection_eff_1)}else{frost_damage<-potential_frost_damage}
  #early insect damage e.g. winter moth, apple blossom weevil 
  early_insect_damage<-chance_event(insect_damage_risk_tp1_tp2,#risk_insect_damage_tp1_tp2,
                                    value_if = loss_insect_damage_tp1_tp2,#insect_damage_loss_tp1_tp2, 
                                    value_if_not = 0,
                                    n=1)
  #flower thinning
  if(subset(Management_values,Management_values$management_measure=="mechanical_flower_thinning")$value==1){percentage_of_thinned_flowers<-eff_mech_flower_thinning_1}else if(
    subset(Management_values,Management_values$management_measure=="chemical_flower_thinning")$value==1){percentage_of_thinned_flowers<-eff_chem_flower_thinning_1}else{percentage_of_thinned_flowers<-0}
  
  #unfavorable weather conditions during blossom
  additional_fruit_drop_unfavorable_weather_during_blossom<-chance_event(unfavorable_weather_during_blossom_risk,#risk_unfavorable_weather_tp1
                                                                         value_if = additional_fruit_drop_unfavorable_weather_tp1_tp2, #increase_fruit_drop_unfavorable_weather_tp1
                                                                         value_if_not = 0,
                                                                         n=1)
  #supporting pollinator insects
  if(subset(Management_values,Management_values$management_measure=="pollinator_support")$value==1){
    fruit_drop_decrease_pollinator_support<-fruit_drop_decrease_pollinator_support_tp1}else{ #decrease_fruit_drop_pollinator_support_tp1
      fruit_drop_decrease_pollinator_support<-0
    }
  #first drop period after bloom (Nachblütefall)
  frist_drop_1<-frist_drop_rate_1*
    (1-fruit_drop_decrease_pollinator_support)
  
  #number of fruits per tree#
  n_fruits_per_tree_tp2_p1<-max(round(n_flowers_tp1*
                                        (1-frost_damage)*
                                        (1-percentage_of_thinned_flowers)*
                                        (1-frist_drop_1)*
                                        (1-early_insect_damage)-
                                        additional_fruit_drop_unfavorable_weather_during_blossom),0)
  
  #fruit diameter
  fruit_diameter_tp2_p1<-exp_fruit_diameter_tp2_1
  
  #trees per ha
  tree_losses_tp1_tp2<-chance_event(risk_tree_losses_1,
                                    value_if = percentage_tree_losses_1,
                                    value_if_not = 0)
  n_trees_tp2_p1<-max(round(trees_per_ha_1*(1-tree_losses_tp1_tp2)),0)
  
  #return
  return(list(number_of_fruits_at_tp_2_predicted_at_tp_1 = n_fruits_per_tree_tp2_p1,
              fruit_diameter_at_tp2_predicted_at_tp1 = fruit_diameter_tp2_p1,
              frost_occurrence = frost_damage,
              insect_damage_tp1 = early_insect_damage,
              n_trees_at_tp2_predicted_at_tp1= n_trees_tp2_p1))
  
}



#Quality tp4####
quality_tp4_function<-function(n_fruits_4,
                               visibly_damaged_4,
                               risk_sunburn_4,
                               damage_sunburn_4,
                               risk_sunburn_hailnet_4,
                               damage_sunburn_hailnet_4,
                               risk_sunburn_climatizing_irrigation_4,
                               damage_sunburn_climatizing_irrigation_4,
                               risk_sunburn_hailnet_climatizing_irrigation_4,
                               damage_sunburn_hailnet_climatizing_irrigation_4,
                               risk_rotting_4,
                               damage_rotting_4,
                               risk_hail_4,
                               damage_hail_4,
                               risk_hail_hailnet_4,
                               damage_hail_hailnet_4,
                               percentage_of_hail_falling,
                               risk_bird_4,
                               damage_bird_4,
                               risk_mechanical_4,
                               damage_mechanical_4,
                               risk_fruit_scab_4,
                               damage_fruit_scab_4,
                               risk_other_fungal_diseases_4,
                               damage_other_fungal_diseases_4,
                               risk_codling_moth_4,
                               damage_codling_moth_4,
                               risk_aphids_4,
                               damage_aphids_4,
                               risk_other_insects_4,
                               damage_other_insects_4,
                               risk_bitter_pit_4,
                               damage_bitter_pit_4,
                               risk_bitter_pit_leaf_fertilization_4,
                               damage_bitter_pit_leaf_fertilization_4,
                               risk_other_physiological_disorders_4,
                               damage_other_physiological_disorders_4,
                               risk_other_physiological_disorders_leaf_fertilization_4,
                               damage_other_physiological_disorders_leaf_fertilization_4,
                               risk_color_decrease_summer_pruning_4,
                               risk_color_decrease_removing_leaves_4,
                               risk_color_increase_hailnet_4,
                               risk_no_cold_nights_4,
                               risk_color_increase_no_cold_4,
                               damage_color_decrease_summer_pruning_4,
                               damage_color_decrease_removing_leaves_4,
                               damage_color_increase_hailnet_4,
                               risk_color_variety,
                               damage_color_variety,
                               damage_color_increase_no_cold,
                               risk_dirty_fruits_4,
                               damage_dirty_fruits_4,
                               add_sunburn_removing_leaves_4,
                               add_sunburn_summer_pruning_4,
                               sunburn_reduce_kaolin_4,
                               risk_fruit_cracking_4,
                               damage_fruit_cracking_4){
  #number all apples per tree
  numbered_apples<-c(1:n_fruits_4)
  #visibly damaged beforehand
  n_damaged_tp4<-round(n_fruits_4*visibly_damaged_4)
  damaged_apples_tp4<-sample(numbered_apples,size=min(length(numbered_apples), n_damaged_tp4))
  
  #Sunburn
  #hailnet and climatizing irrigation need specific infrastructure while Kaolin and Leaf removing are yearly management options
  #therefore: the infrastructure defines the baseline risk which can be adapted by management
  if(subset(Management_values,Management_values$management_measure=="hailnet")$value==0 & subset(Management_values,Management_values$management_measure=="climatizing_ov_irrigation")$value==0){
    percentage_of_sunburn_damage_infrastructure<-chance_event(risk_sunburn_4,
                                                              value_if = damage_sunburn_4,
                                                              value_if_not = 0)}else if(subset(Management_values,Management_values$management_measure=="hailnet")$value==1 & subset(Management_values,Management_values$management_measure=="climatizing_ov_irrigation")$value==0){
                                                                percentage_of_sunburn_damage_infrastructure<-chance_event(risk_sunburn_hailnet_4,
                                                                                                                          value_if = damage_sunburn_hailnet_4,
                                                                                                                          value_if_not = 0)                                             
                                                              }else if(subset(Management_values,Management_values$management_measure=="hailnet")$value==0 & subset(Management_values,Management_values$management_measure=="climatizing_ov_irrigation")$value==1){
                                                                percentage_of_sunburn_damage_infrastructure<-chance_event(risk_sunburn_climatizing_irrigation_4,
                                                                                                                          value_if = damage_sunburn_climatizing_irrigation_4,
                                                                                                                          value_if_not = 0)
                                                              }else{
                                                                percentage_of_sunburn_damage_infrastructure<-chance_event(risk_sunburn_hailnet_climatizing_irrigation_4,
                                                                                                                          value_if = damage_sunburn_hailnet_climatizing_irrigation_4,
                                                                                                                          value_if_not = 0)
                                                              }
  ifelse(subset(Management_values,Management_values$management_measure=="use_kaolin")$value==1, sunburn_reduce_kaolin<-sunburn_reduce_kaolin_4, sunburn_reduce_kaolin<-0)
  ifelse(subset(Management_values,Management_values$management_measure=="removing_leaves")$value==1, add_sunburn_removing_leaves<-add_sunburn_removing_leaves_4, add_sunburn_removing_leaves<-0)
  ifelse(subset(Management_values,Management_values$management_measure=="summer_pruning")$value==1, add_sunburn_summer_pruning<-add_sunburn_summer_pruning_4, add_sunburn_summer_pruning<-0)
  
  percentage_of_sunburn_damage<-min(percentage_of_sunburn_damage_infrastructure*
                                      (1-sunburn_reduce_kaolin)+
                                      add_sunburn_removing_leaves+
                                      add_sunburn_summer_pruning,1)
  n_sunburn_apples<-round(n_fruits_4*percentage_of_sunburn_damage)
  sunburn_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_sunburn_apples))
  
  #fruit rotting
  percentage_of_rotting_damage<-chance_event(risk_rotting_4,
                                             value_if = damage_rotting_4,
                                             value_if_not = 0)
  n_rotting_apples<-round(n_fruits_4*percentage_of_rotting_damage)
  rotting_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_rotting_apples))
  
  #color
  ifelse(subset(Management_values,Management_values$management_measure=="summer_pruning")$value==1, risk_color_decrease_summer_pruning<-risk_color_decrease_summer_pruning_4, risk_color_decrease_summer_pruning<-0)
  ifelse(subset(Management_values,Management_values$management_measure=="removing_leaves")$value==1, risk_color_decrease_removing_leaves<-risk_color_decrease_removing_leaves_4, risk_color_decrease_removing_leaves<-0)
  ifelse(subset(Management_values,Management_values$management_measure=="hailnet")$value==1, risk_color_increase_hailnet<-risk_color_increase_hailnet_4, risk_color_increase_hailnet<-0)
  
  ifelse(subset(Management_values,Management_values$management_measure=="summer_pruning")$value==1, damage_color_decrease_summer_pruning<-damage_color_decrease_summer_pruning_4, damage_color_decrease_summer_pruning<-0)
  ifelse(subset(Management_values,Management_values$management_measure=="removing_leaves")$value==1, damage_color_decrease_removing_leaves<-damage_color_decrease_removing_leaves_4, damage_color_decrease_removing_leaves<-0)
  ifelse(subset(Management_values,Management_values$management_measure=="hailnet")$value==1, damage_color_increase_hailnet<-damage_color_increase_hailnet_4, damage_color_increase_hailnet<-0)
  
  risk_color_increase_no_cold<-chance_event(risk_no_cold_nights_4,
                                            value_if = risk_color_increase_no_cold_4,
                                            value_if_not = 0)
  
  risk_color_problem<-min(risk_color_variety*
                            (1-risk_color_decrease_summer_pruning)*
                            (1-risk_color_decrease_removing_leaves)*
                            (1+risk_color_increase_hailnet)*
                            (1+risk_color_increase_no_cold),1)
  
  damage_color_problem<-min(damage_color_variety*
                              (1-damage_color_decrease_summer_pruning)*
                              (1-damage_color_decrease_removing_leaves)*
                              (1+damage_color_increase_hailnet)*
                              (1+damage_color_increase_no_cold),1)
  
  percentage_color_problem<-chance_event(risk_color_problem,
                                         value_if = damage_color_problem,
                                         value_if_not = 0)
  
  n_color_problem_apples<-round(n_fruits_4*percentage_color_problem)
  color_problem_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_color_problem_apples))
  
  
  
  #hail
  if(subset(Management_values,Management_values$management_measure=="hailnet")$value==0){percentage_of_hail_damage<-chance_event(risk_hail_4,
                                                                                                                                 value_if = damage_hail_4,
                                                                                                                                 value_if_not = 0)}else{
                                                                                                                                   percentage_of_hail_damage<-chance_event(risk_hail_hailnet_4,
                                                                                                                                                                           value_if = damage_hail_hailnet_4,
                                                                                                                                                                           value_if_not = 0)
                                                                                                                                 }
  
  n_hail_apples<-round(n_fruits_4*percentage_of_hail_damage)
  hail_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_hail_apples))
  
  n_hail_apples_falling<-round(n_hail_apples*percentage_of_hail_falling)
  hail_apples_falling<-sample(hail_apples, size = n_hail_apples_falling)
  #bird picking
  percentage_of_bird_damage<-chance_event(risk_bird_4,
                                          value_if = damage_bird_4,
                                          value_if_not = 0)
  n_bird_apples<-round(n_fruits_4*percentage_of_bird_damage)
  bird_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_bird_apples))
  
  #mechanical damage
  percentage_of_mechanical_damage<-chance_event(risk_mechanical_4,
                                                value_if = damage_mechanical_4,
                                                value_if_not = 0)
  n_mechanical_apples<-round(n_fruits_4*percentage_of_mechanical_damage)
  mechanical_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_mechanical_apples))
  
  #fruit scab damage
  percentage_of_fruit_scab_damage<-chance_event(risk_fruit_scab_4,
                                                value_if = damage_fruit_scab_4,
                                                value_if_not = 0)
  n_fruit_scab_apples<-round(n_fruits_4*percentage_of_fruit_scab_damage)
  fruit_scab_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_fruit_scab_apples))
  
  #other fungal diseases 
  percentage_of_other_fungal_diseases_damage<-chance_event(risk_other_fungal_diseases_4,
                                                           value_if = damage_other_fungal_diseases_4,
                                                           value_if_not = 0)
  n_other_fungal_diseases_apples<-round(n_fruits_4*percentage_of_other_fungal_diseases_damage)
  other_fungal_diseases_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_other_fungal_diseases_apples))
  
  #codling_moth damage
  percentage_of_codling_moth_damage<-chance_event(risk_codling_moth_4,
                                                  value_if = damage_codling_moth_4,
                                                  value_if_not = 0)
  n_codling_moth_apples<-round(n_fruits_4*percentage_of_codling_moth_damage)
  codling_moth_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_codling_moth_apples))
  
  #aphids damage
  percentage_of_aphids_damage<-chance_event(risk_aphids_4,
                                            value_if = damage_aphids_4,
                                            value_if_not = 0)
  n_aphids_apples<-round(n_fruits_4*percentage_of_aphids_damage)
  aphids_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_aphids_apples))
  
  #other_insects damage
  percentage_of_other_insects_damage<-chance_event(risk_other_insects_4,
                                                   value_if = damage_other_insects_4,
                                                   value_if_not = 0)
  n_other_insects_apples<-round(n_fruits_4*percentage_of_other_insects_damage)
  other_insects_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_other_insects_apples))
  
  #bitter pit
  if(subset(Management_values,Management_values$management_measure=="leaf_fertilization")$value==0){percentage_of_bitter_pit_damage<-chance_event(risk_bitter_pit_4,
                                                                                                                                                  value_if = damage_bitter_pit_4,
                                                                                                                                                  value_if_not = 0)}else{
                                                                                                                                                    percentage_of_bitter_pit_damage<-chance_event(risk_bitter_pit_leaf_fertilization_4,
                                                                                                                                                                                                  value_if = damage_bitter_pit_leaf_fertilization_4,
                                                                                                                                                                                                  value_if_not = 0)
                                                                                                                                                  }
  
  n_bitter_pit_apples<-round(n_fruits_4*percentage_of_bitter_pit_damage)
  bitter_pit_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_bitter_pit_apples))
  
  #other physiologigal disorders
  if(subset(Management_values,Management_values$management_measure=="leaf_fertilization")$value==0){percentage_of_other_physiological_disorders_damage<-chance_event(risk_other_physiological_disorders_4,
                                                                                                                                                                     value_if = damage_other_physiological_disorders_4,
                                                                                                                                                                     value_if_not = 0)}else{
                                                                                                                                                                       percentage_of_other_physiological_disorders_damage<-chance_event(risk_other_physiological_disorders_leaf_fertilization_4,
                                                                                                                                                                                                                                        value_if = damage_other_physiological_disorders_leaf_fertilization_4,
                                                                                                                                                                                                                                        value_if_not = 0)
                                                                                                                                                                     }
  
  n_other_physiological_disorders_apples<-round(n_fruits_4*percentage_of_other_physiological_disorders_damage)
  other_physiological_disorders_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_other_physiological_disorders_apples))
  
  #dirty fruits (e.g. bird shit) 
  percentage_of_dirty_fruits_damage<-chance_event(risk_dirty_fruits_4,
                                                  value_if = damage_dirty_fruits_4,
                                                  value_if_not = 0)
  n_dirty_fruits_apples<-round(n_fruits_4*percentage_of_dirty_fruits_damage)
  dirty_fruits_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_dirty_fruits_apples))
  
  
  #fruit cracking 
  percentage_of_fruit_cracking_damage<-chance_event(risk_fruit_cracking_4,
                                                    value_if = damage_fruit_cracking_4,
                                                    value_if_not = 0)
  n_fruit_cracking_apples<-round(n_fruits_4*percentage_of_fruit_cracking_damage)
  fruit_cracking_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_fruit_cracking_apples))
  
  #combine
  damaged_apples_harvest<-unique(c(damaged_apples_tp4,
                                   sunburn_apples,
                                   rotting_apples,
                                   color_problem_apples,
                                   hail_apples,
                                   bird_apples,
                                   mechanical_apples,
                                   fruit_scab_apples,
                                   other_fungal_diseases_apples,
                                   codling_moth_apples,
                                   aphids_apples,
                                   other_insects_apples,
                                   bitter_pit_apples,
                                   other_physiological_disorders_apples,
                                   dirty_fruits_apples
                                   
  ))
  
  #apples falling down due to a hail event
  damaged_apples_harvest<-damaged_apples_harvest[! damaged_apples_harvest %in% hail_apples_falling]
  
  #percentage of apples damaged at harvest
  percentage_damaged_at_harvest<-ifelse(n_fruits_4>0,length(damaged_apples_harvest)/n_fruits_4, 0)
  
  #return
  return(list(n_damaged_apples=length(damaged_apples_harvest),
              percentage_damaged_at_harvest=percentage_damaged_at_harvest,
              n_falling_hail=length(hail_apples_falling)))
}



#Quality tp3####
quality_tp3_function<-function(n_fruits_3,
                               visibly_damaged_3,
                               risk_sunburn_3,
                               damage_sunburn_3,
                               risk_sunburn_hailnet_3,
                               damage_sunburn_hailnet_3,
                               risk_sunburn_climatizing_irrigation_3,
                               damage_sunburn_climatizing_irrigation_3,
                               risk_sunburn_hailnet_climatizing_irrigation_3,
                               damage_sunburn_hailnet_climatizing_irrigation_3,
                               risk_rotting_3,
                               damage_rotting_3,
                               risk_hail_3,
                               damage_hail_3,
                               risk_hail_hailnet_3,
                               damage_hail_hailnet_3,
                               percentage_of_hail_falling,
                               risk_bird_3,
                               damage_bird_3,
                               risk_mechanical_3,
                               damage_mechanical_3,
                               risk_fruit_scab_3,
                               damage_fruit_scab_3,
                               risk_other_fungal_diseases_3,
                               damage_other_fungal_diseases_3,
                               risk_codling_moth_3,
                               damage_codling_moth_3,
                               risk_aphids_3,
                               damage_aphids_3,
                               risk_other_insects_3,
                               damage_other_insects_3,
                               risk_bitter_pit_3,
                               damage_bitter_pit_3,
                               risk_bitter_pit_leaf_fertilization_3,
                               damage_bitter_pit_leaf_fertilization_3,
                               risk_other_physiological_disorders_3,
                               damage_other_physiological_disorders_3,
                               risk_other_physiological_disorders_leaf_fertilization_3,
                               damage_other_physiological_disorders_leaf_fertilization_3,
                               add_sunburn_summer_pruning_3,
                               sunburn_reduce_kaolin_3,
                               risk_fruit_cracking_3,
                               damage_fruit_cracking_3,
                               damage_remove_manual_thinning_3,
                               thinning_goal_3,
                               variation_thinning_3){
  
  #manual thinning 
  if(subset(Management_values,Management_values$management_measure=="manual_thinning_after_june_drop")$value==1){
    n_fruits_per_tree_beginning_tp3<-round(min(vv(thinning_goal_3,
                                                  var_CV = variation_thinning_3,#deviation from the desired number of fruits after manual thinning
                                                  n=1),
                                               n_fruits_3))}else{
                                                 n_fruits_per_tree_beginning_tp3<-round(n_fruits_3)
                                               }
  #visibly damaged apples (and their remove during manual thinning)
  n_visibly_damaged_3<-round(n_fruits_3*visibly_damaged_3)
  if(subset(Management_values,Management_values$management_measure=="manual_thinning_after_june_drop")$value==1){
    n_visibly_damaged_remaining<-n_visibly_damaged_3- 
      min(n_visibly_damaged_3*
            damage_remove_manual_thinning_3, n_fruits_3-n_fruits_per_tree_beginning_tp3)
  }else{n_visibly_damaged_remaining<-n_visibly_damaged_3}
  
  #numbering apples
  numbered_apples<-c(1:n_fruits_per_tree_beginning_tp3)
  
  #visibly damaged apples after manual thinning
  damaged_apples_tp3<-sample(numbered_apples,size=min(length(numbered_apples),n_visibly_damaged_remaining))
  
  #Sunburn
  if(subset(Management_values,Management_values$management_measure=="hailnet")$value==0 & subset(Management_values,Management_values$management_measure=="climatizing_ov_irrigation")$value==0){
    percentage_of_sunburn_damage_infrastructure<-chance_event(risk_sunburn_3,
                                                              value_if = damage_sunburn_3,
                                                              value_if_not = 0)}else if(subset(Management_values,Management_values$management_measure=="hailnet")$value==1 & subset(Management_values,Management_values$management_measure=="climatizing_ov_irrigation")$value==0){
                                                                percentage_of_sunburn_damage_infrastructure<-chance_event(risk_sunburn_hailnet_3,
                                                                                                                          value_if = damage_sunburn_hailnet_3,
                                                                                                                          value_if_not = 0)                                             
                                                              }else if(subset(Management_values,Management_values$management_measure=="hailnet")$value==0 & subset(Management_values,Management_values$management_measure=="climatizing_ov_irrigation")$value==1){
                                                                percentage_of_sunburn_damage_infrastructure<-chance_event(risk_sunburn_climatizing_irrigation_3,
                                                                                                                          value_if = damage_sunburn_climatizing_irrigation_3,
                                                                                                                          value_if_not = 0)
                                                              }else{
                                                                percentage_of_sunburn_damage_infrastructure<-chance_event(risk_sunburn_hailnet_climatizing_irrigation_3,
                                                                                                                          value_if = damage_sunburn_hailnet_climatizing_irrigation_3,
                                                                                                                          value_if_not = 0)
                                                              }
  
  ifelse(subset(Management_values,Management_values$management_measure=="use_kaolin")$value==1, sunburn_reduce_kaolin<-sunburn_reduce_kaolin_3, sunburn_reduce_kaolin<-0)
  ifelse(subset(Management_values,Management_values$management_measure=="summer_pruning")$value==1, add_sunburn_summer_pruning<-add_sunburn_summer_pruning_3, add_sunburn_summer_pruning<-0)
  
  percentage_of_sunburn_damage<-min(percentage_of_sunburn_damage_infrastructure*
                                      (1-sunburn_reduce_kaolin)+
                                      add_sunburn_summer_pruning,1)
  n_sunburn_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_sunburn_damage)
  sunburn_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_sunburn_apples))
  
  #fruit rotting
  percentage_of_rotting_damage<-chance_event(risk_rotting_3,
                                             value_if = damage_rotting_3,
                                             value_if_not = 0)
  n_rotting_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_rotting_damage)
  rotting_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_rotting_apples))
  
  #hail
  if(subset(Management_values,Management_values$management_measure=="hailnet")$value==0){percentage_of_hail_damage<-chance_event(risk_hail_3,
                                                                                                                                 value_if = damage_hail_3,
                                                                                                                                 value_if_not = 0)}else{
                                                                                                                                   percentage_of_hail_damage<-chance_event(risk_hail_hailnet_3,
                                                                                                                                                                           value_if = damage_hail_hailnet_3,
                                                                                                                                                                           value_if_not = 0)
                                                                                                                                 }
  n_hail_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_hail_damage)
  hail_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_hail_apples))
  
  n_hail_apples_falling<-round(n_hail_apples*percentage_of_hail_falling)
  hail_apples_falling<-sample(hail_apples, size = n_hail_apples_falling)
  #bird picking
  percentage_of_bird_damage<-chance_event(risk_bird_3,
                                          value_if = damage_bird_3,
                                          value_if_not = 0)
  n_bird_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_bird_damage)
  bird_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_bird_apples))
  
  #mechanical damage
  percentage_of_mechanical_damage<-chance_event(risk_mechanical_3,
                                                value_if = damage_mechanical_3,
                                                value_if_not = 0)
  n_mechanical_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_mechanical_damage)
  mechanical_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_mechanical_apples))
  
  
  #fruit scab damage
  percentage_of_fruit_scab_damage<-chance_event(risk_fruit_scab_3,
                                                value_if = damage_fruit_scab_3,
                                                value_if_not = 0)
  n_fruit_scab_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_fruit_scab_damage)
  fruit_scab_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_fruit_scab_apples))
  
  #other fungal diseases 
  percentage_of_other_fungal_diseases_damage<-chance_event(risk_other_fungal_diseases_3,
                                                           value_if = damage_other_fungal_diseases_3,
                                                           value_if_not = 0)
  n_other_fungal_diseases_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_other_fungal_diseases_damage)
  other_fungal_diseases_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_other_fungal_diseases_apples))
  
  #codling moth damage
  percentage_of_codling_moth_damage<-chance_event(risk_codling_moth_3,
                                                  value_if = damage_codling_moth_3,
                                                  value_if_not = 0)
  n_codling_moth_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_codling_moth_damage)
  codling_moth_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_codling_moth_apples))
  
  #aphids damage
  percentage_of_aphids_damage<-chance_event(risk_aphids_3,
                                            value_if = damage_aphids_3,
                                            value_if_not = 0)
  n_aphids_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_aphids_damage)
  aphids_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_aphids_apples))
  
  #other insects damage
  percentage_of_other_insects_damage<-chance_event(risk_other_insects_3,
                                                   value_if = damage_other_insects_3,
                                                   value_if_not = 0)
  n_other_insects_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_other_insects_damage)
  other_insects_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_other_insects_apples))
  
  #bitter pit
  if(subset(Management_values,Management_values$management_measure=="leaf_fertilization")$value==0){percentage_of_bitter_pit_damage<-chance_event(risk_bitter_pit_3,
                                                                                                                                                  value_if = damage_bitter_pit_3,
                                                                                                                                                  value_if_not = 0)}else{
                                                                                                                                                    percentage_of_bitter_pit_damage<-chance_event(risk_bitter_pit_leaf_fertilization_3,
                                                                                                                                                                                                  value_if = damage_bitter_pit_leaf_fertilization_3,
                                                                                                                                                                                                  value_if_not = 0)
                                                                                                                                                  }
  
  n_bitter_pit_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_bitter_pit_damage)
  bitter_pit_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_bitter_pit_apples))
  
  #other physiological disorders
  if(subset(Management_values,Management_values$management_measure=="leaf_fertilization")$value==0){percentage_of_other_physiological_disorders_damage<-chance_event(risk_other_physiological_disorders_3,
                                                                                                                                                                     value_if = damage_other_physiological_disorders_3,
                                                                                                                                                                     value_if_not = 0)}else{
                                                                                                                                                                       percentage_of_other_physiological_disorders_damage<-chance_event(risk_other_physiological_disorders_leaf_fertilization_3,
                                                                                                                                                                                                                                        value_if = damage_other_physiological_disorders_leaf_fertilization_3,
                                                                                                                                                                                                                                        value_if_not = 0)
                                                                                                                                                                     }
  
  n_other_physiological_disorders_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_other_physiological_disorders_damage)
  other_physiological_disorders_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_other_physiological_disorders_apples))
  
  
  #fruit cracking 
  percentage_of_fruit_cracking_damage<-chance_event(risk_fruit_cracking_3,
                                                    value_if = damage_fruit_cracking_3,
                                                    value_if_not = 0)
  n_fruit_cracking_apples<-round(n_fruits_per_tree_beginning_tp3*percentage_of_fruit_cracking_damage)
  fruit_cracking_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_fruit_cracking_apples)) 
  
  #combine
  damaged_apples_tp4_p3<-unique(c(damaged_apples_tp3,
                                  sunburn_apples,
                                  rotting_apples,
                                  hail_apples,
                                  bird_apples,
                                  mechanical_apples,
                                  fruit_scab_apples,
                                  other_fungal_diseases_apples,
                                  codling_moth_apples,
                                  aphids_apples,
                                  other_insects_apples,
                                  bitter_pit_apples,
                                  other_physiological_disorders_apples
  ))
  
  #apples falling down due to a hail event
  damaged_apples_tp4_p3<-damaged_apples_tp4_p3[! damaged_apples_tp4_p3 %in% hail_apples_falling]
  
  #percentage of damaged apples at tp4
  percentage_damaged_at_tp4_p3<-ifelse(n_fruits_3>0,length(damaged_apples_tp4_p3)/n_fruits_3, 0)
  
  #return
  return(list(n_damaged_apples_tp4_p3=length(damaged_apples_tp4_p3),
              percentage_damaged_at_tp4_p3=percentage_damaged_at_tp4_p3,
              n_falling_hail_tp3_tp4=length(hail_apples_falling),
              n_fruits_after_manual_thinning_timepoint=n_fruits_per_tree_beginning_tp3))
  
}

#Quality tp2 ####
quality_tp2_function<-function(n_fruits_2,
                               visibly_damaged_2,
                               bad_seed_structure_risk_2,
                               reduce_bad_seed_structure_risk_pollinator_support,#bad_seed_structure_reduce_pollinator_support
                               deformation_bad_seed_structure_2,#bad_seed_structure_deformation_tp2
                               risk_hail_2,
                               damage_hail_2,
                               risk_hail_hailnet_2,
                               damage_hail_hailnet_2,
                               percentage_of_hail_falling,
                               risk_mechanical_2,
                               damage_mechanical_2,
                               risk_fruit_scab_2,
                               damage_fruit_scab_2,
                               risk_other_fungal_diseases_2,
                               damage_other_fungal_diseases_2,
                               risk_codling_moth_2,
                               damage_codling_moth_2,
                               risk_aphids_2,
                               damage_aphids_2,
                               risk_other_insects_2,
                               damage_other_insects_2,
                               risk_excessive_russeting_2,
                               damage_excessive_russeting_2,
                               risk_fruit_cracking_2,
                               damage_fruit_cracking_2,
                               risk_rotting_2,
                               damage_rotting_2){
  #number all apples
  numbered_apples<-c(1:n_fruits_2)
  #visibly damaged beforehand
  n_damaged_tp2<-max(round(n_fruits_2*visibly_damaged_2),0)
  damaged_apples_tp2<-sample(numbered_apples,size=min(length(numbered_apples),n_damaged_tp2))
  
  
  #hail
  if(subset(Management_values,Management_values$management_measure=="hailnet")$value==0){percentage_of_hail_damage<-chance_event(risk_hail_2,
                                                                                                                                 value_if = damage_hail_2,
                                                                                                                                 value_if_not = 0)}else{
                                                                                                                                   percentage_of_hail_damage<-chance_event(risk_hail_hailnet_2,
                                                                                                                                                                           value_if = damage_hail_hailnet_2,
                                                                                                                                                                           value_if_not = 0)
                                                                                                                                 }
  n_hail_apples<-round(n_fruits_2*percentage_of_hail_damage)
  hail_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_hail_apples))
  
  n_hail_apples_falling<-round(n_hail_apples*percentage_of_hail_falling)
  hail_apples_falling<-sample(hail_apples, size = n_hail_apples_falling)
  
  #deformation bad seed structure
  if(subset(Management_values,Management_values$management_measure=="pollinator_support")$value==1){
    bad_seed_structure_risk_adapted<-bad_seed_structure_risk_2*(1-reduce_bad_seed_structure_risk_pollinator_support)
  }else{bad_seed_structure_risk_adapted<-bad_seed_structure_risk_2}
  
  percentage_of_bad_seed_structure_deformation<-chance_event(bad_seed_structure_risk_adapted,
                                                             value_if = deformation_bad_seed_structure_2,
                                                             value_if_not = 0,
                                                             n=1)
  n_bad_seed_deformation_apples<-round(n_fruits_2*percentage_of_bad_seed_structure_deformation)
  bad_seed_deformation_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_bad_seed_deformation_apples))
  
  
  #mechanical damage
  percentage_of_mechanical_damage<-chance_event(risk_mechanical_2,
                                                value_if = damage_mechanical_2,
                                                value_if_not = 0)
  n_mechanical_apples<-round(n_fruits_2*percentage_of_mechanical_damage)
  mechanical_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_mechanical_apples))
  
  #fruit scab damage
  percentage_of_fruit_scab_damage<-chance_event(risk_fruit_scab_2,
                                                value_if = damage_fruit_scab_2,
                                                value_if_not = 0)
  n_fruit_scab_apples<-round(n_fruits_2*percentage_of_fruit_scab_damage)
  fruit_scab_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_fruit_scab_apples))
  
  #other fungal diseases 
  percentage_of_other_fungal_diseases_damage<-chance_event(risk_other_fungal_diseases_2,
                                                           value_if = damage_other_fungal_diseases_2,
                                                           value_if_not = 0)
  n_other_fungal_diseases_apples<-round(n_fruits_2*percentage_of_other_fungal_diseases_damage)
  other_fungal_diseases_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_other_fungal_diseases_apples))
  
  #codling moth damage
  percentage_of_codling_moth_damage<-chance_event(risk_codling_moth_2,
                                                  value_if = damage_codling_moth_2,
                                                  value_if_not = 0)
  n_codling_moth_apples<-round(n_fruits_2*percentage_of_codling_moth_damage)
  codling_moth_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_codling_moth_apples))
  
  #aphids damage
  percentage_of_aphids_damage<-chance_event(risk_aphids_2,
                                            value_if = damage_aphids_2,
                                            value_if_not = 0)
  n_aphids_apples<-round(n_fruits_2*percentage_of_aphids_damage)
  aphids_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_aphids_apples))
  
  #other insects damage
  percentage_of_other_insects_damage<-chance_event(risk_other_insects_2,
                                                   value_if = damage_other_insects_2,
                                                   value_if_not = 0)
  n_other_insects_apples<-round(n_fruits_2*percentage_of_other_insects_damage)
  other_insects_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_other_insects_apples))
  
  #excessive russeting 
  percentage_of_excessive_russeting_damage<-chance_event(risk_excessive_russeting_2,
                                                         value_if = damage_excessive_russeting_2,
                                                         value_if_not = 0)
  n_excessive_russeting_apples<-round(n_fruits_2*percentage_of_excessive_russeting_damage)
  excessive_russeting_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_excessive_russeting_apples))
  
  #fruit cracking 
  percentage_of_fruit_cracking_damage<-chance_event(risk_fruit_cracking_2,
                                                    value_if = damage_fruit_cracking_2,
                                                    value_if_not = 0)
  n_fruit_cracking_apples<-round(n_fruits_2*percentage_of_fruit_cracking_damage)
  fruit_cracking_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_fruit_cracking_apples))
  
  #fruit rotting
  percentage_of_rotting_damage<-chance_event(risk_rotting_2,
                                             value_if = damage_rotting_2,
                                             value_if_not = 0)
  n_rotting_apples<-round(n_fruits_2*percentage_of_rotting_damage)
  rotting_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_rotting_apples))
  
  #combine
  damaged_apples_tp3_p2<-unique(c(damaged_apples_tp2,
                                  hail_apples,
                                  #bird_apples,
                                  mechanical_apples,
                                  bad_seed_deformation_apples,
                                  fruit_scab_apples,
                                  other_fungal_diseases_apples,
                                  codling_moth_apples,
                                  aphids_apples,
                                  other_insects_apples,
                                  excessive_russeting_apples,
                                  rotting_apples))
  #apples falling down due to a hail event
  damaged_apples_tp3_p2<-damaged_apples_tp3_p2[! damaged_apples_tp3_p2 %in% hail_apples_falling]
  
  #percentage of apples damaged at tp 3
  percentage_damaged_at_tp3_p2<-ifelse(n_fruits_2>0,length(damaged_apples_tp3_p2)/n_fruits_2, 0)
  
  #return
  return(list(n_damaged_apples_tp3_p2=length(damaged_apples_tp3_p2),
              percentage_damaged_at_tp3_p2=percentage_damaged_at_tp3_p2,
              bad_seed_structure_occurrence=percentage_of_bad_seed_structure_deformation,
              n_falling_hail_tp2_tp3=length(hail_apples_falling)))
  
}

#Quality tp1 ####
quality_tp1_function<-function(flower_cluster_1,
                               flowers_per_cluster_1,
                               frost_event,
                               percentage_quality_reduce_frost,#quality_reduce_frost_tp1_tp2
                               insect_damage_blossom,
                               percentage_quality_reduce_insects,#quality_reduce_insects_tp1_tp2
                               risk_aphids_1,
                               damage_aphids_1,
                               risk_mechanical_1,
                               damage_mechanical_1,
                               risk_fruit_scab_1,
                               damage_fruit_scab_1,
                               risk_excessive_russeting_1,
                               damage_excessive_russeting_1,
                               risk_hail_1,
                               damage_hail_1,
                               risk_hail_hailnet_1,
                               damage_hail_hailnet_1,
                               percentage_of_hail_falling,
                               risk_rotting_1,
                               damage_rotting_1){
  #number of flowers
  n_flowers_1<-max(round(flower_cluster_1*flowers_per_cluster_1),0)
  #number all flowers/fruitlets
  numbered_apples<-c(1:n_flowers_1)
  
  #frost damage
  if(frost_event!=0){
    n_frost_damage_apples<-round(n_flowers_1*percentage_quality_reduce_frost)
  }else{n_frost_damage_apples<-0}
  frost_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_frost_damage_apples))
  
  #insect damage
  if(insect_damage_blossom!=0){
    n_insect_damage_apples<-round(n_flowers_1*percentage_quality_reduce_insects)
  }else{n_insect_damage_apples<-0}
  insect_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_insect_damage_apples))
  
  #aphids damage
  percentage_of_aphids_damage<-chance_event(risk_aphids_1,
                                            value_if = damage_aphids_1,
                                            value_if_not = 0)
  n_aphids_apples<-round(n_flowers_1*percentage_of_aphids_damage)
  aphids_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_aphids_apples))
  
  #fruit_scab damage
  percentage_of_fruit_scab_damage<-chance_event(risk_fruit_scab_1,
                                                value_if = damage_fruit_scab_1,
                                                value_if_not = 0)
  n_fruit_scab_apples<-round(n_flowers_1*percentage_of_fruit_scab_damage)
  fruit_scab_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_fruit_scab_apples))
  
  #fruit rotting
  percentage_of_rotting_damage<-chance_event(risk_rotting_1,
                                             value_if = damage_rotting_1,
                                             value_if_not = 0)
  n_rotting_apples<-round(n_flowers_1*percentage_of_rotting_damage)
  rotting_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_rotting_apples))
  #mechanical damage
  percentage_of_mechanical_damage<-chance_event(risk_mechanical_1,
                                                value_if = damage_mechanical_1,
                                                value_if_not = 0)
  n_mechanical_apples<-round(n_flowers_1*percentage_of_mechanical_damage)
  mechanical_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_mechanical_apples))
  
  #excessive russeting 
  percentage_of_excessive_russeting_damage<-chance_event(risk_excessive_russeting_1,
                                                         value_if = damage_excessive_russeting_1,
                                                         value_if_not = 0)
  n_excessive_russeting_apples<-round(n_flowers_1*percentage_of_excessive_russeting_damage)
  excessive_russeting_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_excessive_russeting_apples))
  #hail
  if(subset(Management_values,Management_values$management_measure=="hailnet")$value==0){percentage_of_hail_damage<-chance_event(risk_hail_1,
                                                                                                                                 value_if = damage_hail_1,
                                                                                                                                 value_if_not = 0)}else{
                                                                                                                                   percentage_of_hail_damage<-chance_event(risk_hail_hailnet_1,
                                                                                                                                                                           value_if = damage_hail_hailnet_1,
                                                                                                                                                                           value_if_not = 0)
                                                                                                                                 }
  n_hail_apples<-round(n_flowers_1*percentage_of_hail_damage)
  hail_apples<-sample(numbered_apples,size=min(length(numbered_apples),n_hail_apples))
  
  n_hail_apples_falling<-round(n_hail_apples*percentage_of_hail_falling)
  hail_apples_falling<-sample(hail_apples, size = n_hail_apples_falling)
  
  #combine
  damaged_apples_tp2_p1<-unique(c(frost_apples,
                                  insect_apples,
                                  mechanical_apples,
                                  excessive_russeting_apples,
                                  hail_apples,
                                  rotting_apples,
                                  fruit_scab_apples,
                                  aphids_apples))
  #apples falling down due to a hail event
  damaged_apples_tp2_p1<-damaged_apples_tp2_p1[! damaged_apples_tp2_p1 %in% hail_apples_falling]
  
  #percentage apples damaged at tp2
  percentage_damaged_at_tp2_p1<-ifelse(n_flowers_1>0, length(damaged_apples_tp2_p1)/n_flowers_1, 0)
  
  #return
  return(list(n_damaged_apples_tp2_p1=length(damaged_apples_tp2_p1),
              percentage_damaged_at_tp2_p1=percentage_damaged_at_tp2_p1,
              n_falling_hail_tp1_tp2=length(hail_apples_falling)))
}
