The provided R code replicates the key results from the paper by Jianqiu Wang, Zhuo Wang, and Ke Wu titled "Forecasting Stock Market Return with Anomalies: Evidence from China," published in the International Journal of Forecasting. If you utilize this code, please ensure to cite the original paper appropriately.


DATA DESCRIPTION

dataCH3.xlsx: Liu et al. (2019) three-factor model 

dataLongMissing.xlsx: long-leg anomaly portfolio returns

dataShortMissing.xlsx: short-leg anomaly portfolio returns

dataLongShortMissing.xlsx: long-short anomaly portfolio returns

dataMarketRx.xlsx: market excess return

alt_anomaly_Long.xlsx: alternative long-leg anomaly portfolio returns (from https://github.com/mlfina/China-A-Sort)

alt_anomaly_Short.xlsx: alternative short-leg anomaly portfolio returns 

alt_anomaly_LongShort.xlsx: alternative long-short anomaly portfolio returns


CODE DESCRIPTION

functions.R: necessary functions

Simulation_MCP.R: simulation code for asymmetric mispricing correction persistence mechanism

Simulation_noisereduction.R: simulation code for noise reduction mechanism

summary_stat.R: summary statistics

forecast.R: Out-of-sample forecast results

forecast_altanomalyset.R: Out-of-sample forecast results for alternative anomaly set

OOSR2.R: Out-of-sample R2

OOSR2_altanomalyset.R: Out-of-sample R2 using alternative anomaly set

utility_sharpe.R: utility gain and annualized Sharpe ratio for the monthly market excess return forecast

utility_sharpe_altanomalyset.R: utility gain and annualized Sharpe ratio for the monthly market excess return forecast using alternative anomaly set

subsample_test.R: Out-of-sample R2, utility gain and Sharpe ratio for the monthly market excess return forecast across various idiosyncratic volatility subsamples

slope_coefficient.R:  recursive estimates of the standardized slope coefficients

draw_figures.R: draw figures


REPLICATION PROCESS


To reproduce all the results from this paper, run the provided programs sequentially. The output files will be saved in the "Table" folder. The runtime depends on your computer's performance but typically requires only a few minutes. All analyses were conducted using R version 4.1.0.

Table 3：Run summary_stat.R

Table 4：Run forecast.R, then run OOSR2.R 

Table 5：Run utility_sharpe.R

Table 6：Run subsample_test.R

Figure 1-6：Run slope_coefficient.R and then run draw_figures.R

Table 7：Rerun forecast.R and OOSR2.R after adjusting r to 84, 108, or 132 in both programs.

Table 8：Run forecast_altanomalyset.R, and then run OOSR2_altanomalyset.R and utility_sharpe_altanomalyset.R

Simulation:
Table 1：Run Simulation_MCP.R

Table 2：Run Simulation_noisereduction.R







