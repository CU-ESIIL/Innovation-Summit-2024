# Tutorial on Historic Redlining and Environmental Analysis

# Introduction

This tutorial delves into the enduring impacts of historic redlining on contemporary urban greenspace. Inspired by the research conducted by Nardone et al. (2021), which explores the correlation between historic redlining and disparities in urban greenspace, this guide aims to replicate and expand upon their analysis using similar datasets and methods. Our goal is to illuminate how historic policies continue to influence modern urban environments.

## Understanding Redlining

Redlining began in the 1930s as a discriminatory practice where banks and other lending institutions denied mortgages or offered unfavorable terms to certain neighborhoods based on racial and ethnic compositions. This practice was systematically executed through maps created by the Home Owners' Loan Corporation (HOLC), which color-coded "risky" investment areas in red, thus coining the term "redlining." Predominantly, these redlined areas housed minority populations, who were systematically excluded from investment and development opportunities.

Maps are powerful tools that extend beyond simple navigation; they serve as instruments of communication and control. By delineating neighborhoods in this way, HOLC maps not only reflected but also reinforced societal prejudices. They dictated economic opportunities and profoundly shaped the physical and social architectures of communities. As instruments of exclusion, these maps had a lasting impact on urban landscapes, influencing where infrastructure was built, which areas received services, and ultimately, which communities would flourish or fail.

The repercussions of redlining are deep and persistent. Economically and socially, it initiated a cycle of poverty, entrenched racial segregation, and a deep-rooted wealth gap that persists today. Environmentally, redlined neighborhoods often endure significant injustices—they typically experience limited access to green spaces, are subjected to higher pollution levels, and face increased vulnerability to climate change impacts, exacerbating health and well-being challenges for their residents.

By analyzing these patterns and understanding their roots through mapping, we gain critical insights into the systemic barriers that have sculpted our urban landscapes. This tutorial not only seeks to reveal the legacy of redlining but also to demonstrate how such discriminatory policies continue to shape urban environments, highlighting the crucial role of mapping in urban policy and social equity.



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

### References

- Nardone, A., Rudolph, K. E., Morello-Frosch, R., & Casey, J. A. (2021). Redlines and Greenspace: The Relationship between Historical Redlining and 2010 Greenspace across the United States. *Environmental Health Perspectives*, 129(1), 017006. DOI:10.1289/EHP7495.
- Nelson, R.K., Winling, L., Marciano, R., Connolly, N. (2019). "Mapping Inequality." *Science*. [Available online](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7839347/pdf/ehp7495.pdf).
