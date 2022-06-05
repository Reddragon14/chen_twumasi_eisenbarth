

clean_imf_pwt.R cleans both the IMF Global Debt Data and Penn World Table 10. and merges them together 
based on a consistent set of codes established through a match and merging of the IMF's International Finance Statistics country code (IFS code)
and ISO country code provided in the Penn World Table

merged_debt.csv

Contains the following variables:
year
countrycode - ISO country code
ifscode - International Financial Statistics country code
country 
pvd - private debt share of GDP
hhd - household debt share of GDP
gg - general (total) government debt share of GDP
cg  - central government debt share of GDP
gdp - current GDP in national currency
cgdpe - Expenditure-side real GDP at current PPPs, to compare relative living standards across countries at a single point in time
cgdpo - Output-side real GDP at current PPPs, to compare relative productive capacity across countries at a single point in time
rgdpe - Expenditure-side real GDP at chained PPPs, to compare relative living standards across countries and over time
rgdpo - Output-side real GDP at chained PPPs, to compare relative productive capacity across countries and over time
pop - population in millons
emp - employed persons in millions
hc - human capital index 
cn - capital stock
delta - average depreciation rate of capital
ccon - real expenditure of households and government 
tfp - total factor productivity
labsh - labor share compensation of GDP at current prices
pl_c - price level consumer expenditure
pl_i  - price level capital formation
pl_g - price level government spending 
pl_x - price level of exports
pl_m - price level of imports


