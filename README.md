# 526project
```
Instructions:
Download the R code & the groud.csv
Change line 51 in the R code to point to your groud.csv location
Run the R code
You will get 3 contingency tables - spyBest, diaBest, qqqBest
They should look the same as spyCont.png, diaCont.png & qqqCont.png

Q. How to read the contingency table ?
A. It has 8 columns & 8 rows

The columns mean
Col 1. < -3%
Col 2. -2 - -3%
Col 3. -1 to -2%
Col 4. 0 to -1%
Col 5. 0 to 1%
Col 6. 1 to 2%
Col 7. 2 to 3%
Col 8. > 3%

The rows mean the same as columns.

Along the column we have the ETF
Down the row we have the volatility index

So for example, 
Cell (5,6) = 31 => there were 31 days when vix went up between 0-1%, and spy went up 1-2%
Cell (4,7) = 6 => there were 6 days when vix went up between 0 to -1%, and spy went up 2-3%
```
