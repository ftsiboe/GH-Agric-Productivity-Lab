#!/bin/bash -l

#SBATCH --job-name=cov_bal 
#SBATCH --time=0-01:00:00    
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=5G
#SBATCH --array=1-1
#SBATCH --mail-user=ftsiboe@ksu.edu
#SBATCH --mail-type=ALL
#SBATCH --output=/dev/null

module purge
module load GDAL/3.5.0-foss-2022a PROJ/9.0.0-GCCcore-11.3.0

cd "/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/replications/tech_inefficiency_disability"
module load R/4.2.1-foss-2022a
R --no-save -q < 002_tech_inefficiency_disability_MATCH.R

cd "/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/replications/tech_inefficiency_education"
module load R/4.2.1-foss-2022a
R --no-save -q < 002_tech_inefficiency_education_MATCH.R

cd "/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/replications/tech_inefficiency_extension"
module load R/4.2.1-foss-2022a
R --no-save -q < 002_tech_inefficiency_extension_MATCH.R

cd "/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/replications/tech_inefficiency_land_tenure"
module load R/4.2.1-foss-2022a
R --no-save -q < 002_tech_inefficiency_land_tenure_MATCH.R

cd "/homes/ftsiboe/Articles/GH/GH_AgricProductivityLab/replications/tech_inefficiency_resource_extract"
module load R/4.2.1-foss-2022a
R --no-save -q < 002_tech_inefficiency_resource_extract_MATCH.R