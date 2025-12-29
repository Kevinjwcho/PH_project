Climate–Nutrition–Disease (CND) Data Pipeline

This repository documents the data preprocessing, clustering, and integration pipeline used to construct the Climate–Nutrition–Disease (CND) typology and the CARE-DDI resilience framework.

The code demonstrates how global datasets on climate exposure, dietary intake, disease burden, and socioeconomic structure are harmonized into a unified country–period panel dataset covering 183 countries between 1990 and 2020.

⸻

Data Domains

The pipeline integrates four domains:
	•	Climate and air-quality stressors
Mean temperature, heat index, relative humidity, precipitation, and PM2.5.
	•	Dietary intake (Global Dietary Database)
Nineteen food and beverage variables capturing global nutrition transitions.
	•	Disease burden (Global Burden of Disease)
Age-standardized DALY rates across 18 disease categories.
	•	Socioeconomic indicators (World Bank)
GDP per capita, urbanization rate, and trade openness.

All datasets are aligned by ISO3 country codes and calendar year. Year 2020 is excluded to avoid partial reporting.

⸻

Clustering and CND Typology

Each domain is clustered independently using k-means (K = 5) after z-score standardization:
	•	Climate clusters: C1–C5
	•	Dietary clusters: N1–N5
	•	Disease clusters: D1–D5

Cluster labels are merged at the country–period level to construct the CND typology (C×N×D).
Out of 125 theoretical combinations, 64 unique CND configurations are empirically observed.

⸻

Final Integrated Dataset

The final merged dataset includes:
	•	Climate, dietary, and disease variables
	•	CND cluster assignments
	•	Socioeconomic indicators used for CARE-DDI construction

This dataset underlies all analyses in the accompanying manuscript, including climate-stratified diet–disease associations, geospatial mapping of CND types, and CARE-DDI resilience estimation.

⸻

Reproducibility
	•	All paths are relative to the project root.
	•	Clustering procedures use fixed random seeds.
	•	Figure generation is intentionally excluded.
	•	The full preprocessing logic is implemented in a single script.

⸻

Reference

This repository supports the analyses described in:

Global Climate Vulnerability and Adaptive Capacity in 183 Countries:
CND Types and the CARE-DDI Resilience Index
