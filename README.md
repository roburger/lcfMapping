# readmeTest

Repository for the MSc Thesis on land cover fraction mapping

> **Be aware: still under development!**

### Data
4 reference datasets used:
- IIASA (2015)
- IIASA (2015-2018)
- WUR (2015)
- WUR (2015-2018)

### (Current) Usage
Scripts should *at this moment* be run in the following order:
1. temporalFilter.R
2. calcVIs.R
3. GetHarmonics.R

The first three scripts process the IIASA (2015) Landsat-8 timeseries and creates features.<br /><br />

4. loadValiData.R &emsp;(processes the WUR (2015) Landsat-8 timeseries and creates features)
5. spliTimeSeries.R &emsp;(splits the WUR (2015) Landsat-8 timeseries into yearly datasets)

6. wurChange.R &emsp;(processes the WUR (2015-2018) Landsat-8 timeseries and creates yearly datasets)
7. iiasaChange.R &emsp;(processes the IIASA (2015-2018) Landsat-8 timeseries and creates yearly datasets)
8. newFeatures.R &emsp;(calculates new features for all 4 reference datasets)
<br />
After processing of the Landsat-8 timeseries, Random Forest regression can be performed in RFfunctionNew.R
