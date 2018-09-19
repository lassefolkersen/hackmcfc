# Hack-MCFC
This is the documentation for the Hack-MCFC code used in the derivation of pass-success probability. The analysis consists of three main parts:


## 1) data_parsing
The most computationally time-consuming part, but basically just parses all opta and chryonhego events and merge them in a large data.frame with each row being one frame, and opta-events assigned to closest 10th of a second. Additionally some calculations are applied to have pass-success or pass-failure readily available.



## 2) pass_identifier 
This module loops over all passes performed, and for each tries to extract as many metrics-of-interest as possible. This involves counting the number of friendly and opposing players in various defined forward areas of the pass; calculating pre- and post-pass ball speeds; as well as a script to algorithmically quantify the number of open opportunities for pass that was present at event-time. The overall goal of this module is basically to obtain as many parameters as possible, that can be evaluated in the stats_script module. Importantly little prior bias is applied as to what parameters are of importance. The overall idea is that the data will reveal those automatically. Future work should involve even more extensive algorithmically defining of metrics-of-interest.




## 3) stats_script
Having the per-pass outcome in an amendable format together with as many metrics-of-interest as possible, the last step is to perform the statistics that can estimate which of the metrics-of-interest actually were metrics-of-importance, and further to quantify how important they were. The chosen algorithm in the hackmcfc was a simple logisitic regression, but any binary-outcome statistic approach could work. Overall, the key point is that having the question phrased in a context of many-events (>1000), with known outcomes (pass/fail), with a richly defined event-characterization (game-conformation, ball-speed, open opportunity count, etc) -- these things will allow the data to reveal which parameters are of importance; and rather than gut-feelings, these are the parameters upon which focus should be in training and tactics learning of the future.
