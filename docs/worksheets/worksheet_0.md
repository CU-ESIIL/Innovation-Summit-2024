# Exploring the Impact of Historical Redlining on Urban Greenspace: A Collaborative Examination of Maps, Justice, and Resilience

## Introduction

In this collaborative exploration, we delve into the long-term impacts of historical redlining on urban greenspace and uncover the resilience of communities in adapting to these entrenched injustices. We underscore the critical role of maps in shaping environmental and social landscapes and emphasize how discriminatory practices encoded in maps have led to persistent disparities. Studies like that of Nardone et al. (2021) and Hoffman et al. (2020) inform our investigation by showing how redlining has contributed to urban heat islands and the disproportionate exposure of marginalized communities to extreme heat.

Historical housing policies have not only led to economic and social segregation but have also set the stage for environmental injustices that resonate to this day. The analysis by Hoffman et al. (2020) reveals a stark 2.6°C average increase in land surface temperatures in redlined areas compared to non-redlined ones, signaling a call to action for urban planners and policymakers to mitigate these disparities and prevent further exacerbation due to current and future climate change.

This introduction sets the stage for a deeper examination of the multi-dimensional consequences of redlining. Through a lens of justice and equity, we aim to highlight the power of adaptation and resilience, illustrating how urban communities can respond to historical disturbances with innovative and sustainable strategies.

![1938 Map of Atlanta uses colors as grades for neighborhoods. The red swaths identify each area with large African-American populations that were deemed “less safe.”](../assets/redlining/redlining.png))

**Figure 1:** 1938 Map of Atlanta uses colors as grades for neighborhoods. The red swaths identify each area with large African-American populations that were deemed “less safe.”

[![ArcGIS Story Map](../assets/redlining/georectified-thumbnail.png)](https://storymaps.arcgis.com/stories/0f58d49c566b486482b3e64e9e5f7ac9)

**Explore the Story Map:** Click on the image above to explore the interactive story map about the enduring effects of redlining on urban environments.

[![Watch the video](https://img.youtube.com/vi/O5FBJyqfoLM/hqdefault.jpg)](https://youtu.be/O5FBJyqfoLM)

**Video Title:** Exploring the Impacts of Historical Redlining on Urban Development  
**Description:** Click on the image above to watch a video that delves into the consequences of historical redlining and its ongoing impact on urban environments. This educational piece offers insights into how such discriminatory practices have shaped cities and what can be learned from them.

### References

- Nardone, A., Rudolph, K. E., Morello-Frosch, R., & Casey, J. A. (2021). Redlines and Greenspace: The Relationship between Historical Redlining and 2010 Greenspace across the United States. *Environmental Health Perspectives*, 129(1), 017006. DOI:10.1289/EHP7495.
- Hoffman, J. S., Shandas, V., & Pendleton, N. (2020). The Effects of Historical Housing Policies on Resident Exposure to Intra-Urban Heat: A Study of 108 US Urban Areas. *Climate*, 8(1), 12. DOI:10.3390/cli8010012.


## Goals of the Tutorial

The primary objectives of this tutorial are:
1. To analyze the relationship between HOLC grades and the presence of urban greenspace.
2. To understand how historic policies continue to affect the spatial distribution of environmental amenities.
3. To visualize the impact of redlining on urban greenspace using mapping and statistical analysis tools.

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

- Nardone, A., Rudolph, K. E., Morello-Frosch, R., & Casey, J. A. (2021). Redlines and Greenspace: The Relationship between Historical Redlining and 2010 Greenspace across the United States. *Environmental Health Perspectives*, 129(1), 017006. DOI:10.1289/EHP7495. [Available online](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7839347/pdf/ehp7495.pdf)
