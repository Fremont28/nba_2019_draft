# nba_2019_draft
Forecasting 2019 NBA draft prospects 

This year’s NBA draft is a mixed bag. The undisputed top pick is Duke’s Zion Williamson, touted as the league’s next LeBron James. Most draft analysts believe that Ja Morant, a sophomore guard from Murray State, will likely land as the second pick in the lottery.

Outside of Williamson and Morant, there is some uncertainty hanging around other draft prospects like Rui Hachimura, R.J. Barrett, and Coby White. Using statistical modeling, we will attempt to forecast each NBA draft prospect’s value above replacement (VORP) over his first three and five seasons in the league.[1] Front offices own the rights on a draft pick’s first four seasons in the NBA and can pay these players below market value. Two international players excluded from the forecast are Sekou Doumbouya and Goga Bitadze.

Our first forecast model is built using generalized additive modeling (GAM) and attempts to predict 2019 draft pick’s VORP over their first three seasons in the NBA. The model is trained on the college statistics and physical attributes (i.e. wingspan, weight) of 273 former or current NBA players over the 2009 through 2019 seasons.[2] At the moment, we don’t currently have data on current draft prospects’ physical features.[3] Instead, we averaged past/current NBA players’ physical metrics by position (i.e. power forward or point guard) and assigned these averages to each prospect from this year’s draft class based on their college position.  

The GAM model considers defensive rebounds, steals, and three-point field goal percentage (3PG%) as important benchmarks for predicting players’ three-year VORP. The model gives us a 0.32 correlation between the projected VORP and pre-draft ranking according to the website NBADraft.net and a 0.35 residual deviance.

read here: 
