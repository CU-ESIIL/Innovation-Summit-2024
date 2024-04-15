# Tutorial on Historic Redlining and Environmental Analysis

## Introduction

This tutorial explores the enduring impacts of historic redlining on contemporary urban greenspace. Drawing upon a recent study by Nardone et al. (2021), which assessed the correlation between historic redlining and greenspace disparities in urban settings, we aim to replicate and expand upon their analysis using similar datasets and methods.

## Understanding Redlining

Redlining, a discriminatory practice initiated in the 1930s by the Home Owners’ Loan Corporation (HOLC), involved marking out neighborhoods deemed poor financial risks often based on racial composition, which were colored in red on maps. This practice denied mortgages and insurance to residents in these neighborhoods and led to long-term economic and racial segregation.

Maps have the power to shape economic opportunities, dictate infrastructure development, and influence community services. The HOLC maps not only reflected societal biases; they also reinforced and exacerbated them, contributing to significant social and economic disparities that persist today.

## Goals of the Tutorial

The primary objectives of this tutorial are:
1. To analyze the relationship between HOLC grades and the presence of urban greenspace.
2. To understand how historic policies continue to affect the spatial distribution of environmental amenities.
3. To visualize the impact of redlining on urban greenspace using Geographic Information Systems (GIS) and statistical analysis tools.

## Part 1: Accessing and Visualizing Historic Redlining Data

We will begin by accessing HOLC maps from the Mapping Inequality project and overlaying this data with modern geographic datasets to visualize the historical impact on contemporary urban landscapes.

### Data Acquisition
- Download HOLC map shapefiles from the University of Richmond’s Mapping Inequality Project.
- Utilize satellite imagery and other geospatial data to map current greenspace using the normalized difference vegetation index (NDVI).

### Analysis Methodology
- Replicate the approach used by Nardone et al. to calculate NDVI values for each HOLC neighborhood, assessing greenspace as a health-promoting resource.
- Employ statistical methods such as propensity score matching to control for confounding variables and estimate the true impact of HOLC grades on urban greenspace.

## Part 2: Integrating Environmental Data

### Data Processing
- Use satellite data from 2010 to analyze greenspace using NDVI, an index that measures the quantity of vegetation in an area.
- Apply methods to adjust for potential confounders as described in the study, ensuring that comparisons of greenspace across HOLC grades are valid and not biased by historical or socio-demographic factors.

## Part 3: Comparative Analysis and Visualization

### Statistical Analysis
- Conduct a detailed statistical analysis to compare greenspace across different HOLC grades, using techniques like Targeted Maximum Likelihood Estimation (TMLE) to assess the association between historical redlining and current greenspace levels.
- Visualize the disparities in greenspace distribution using GIS tools, highlighting how redlining has shaped urban ecological landscapes.

## Conclusion

This tutorial provides tools and methodologies to explore the lingering effects of historic redlining on urban greenspace, offering insights into the intersection of urban planning, environmental justice, and public health.

## References

- Nardone, A., Rudolph, K. E., Morello-Frosch, R., & Casey, J. A. (2021). Redlines and Greenspace: The Relationship between Historical Redlining and 2010 Greenspace across the United States. Environmental Health Perspectives, 129(1), 017006. [DOI:10.
