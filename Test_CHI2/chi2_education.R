source("chi2_shared.R")

run_variable_analysis(
  variable_name = "I.- IDENTIFICATION DU CHEF DE MENAGE /Niveau d'instruction: 1=Aucun, 2=Primaire, 3=Secondaire, 4=Superieure, 5=vide",
  output_path = "chi2_education_results.xlsx",
  variable_label = "Niveau d'instruction"
)
