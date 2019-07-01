# City-scalable Destination Recommender System for On-Demand Senior Mobility

### Principal Investigator: Joseph Chow *NYU*

### Co-principal Investigator: Kelvin Cheu *UTEP*

### Research Assistants: Assel Dmitriyeva *NYU*, Gyugeun Yoon *NYU*, Daniel Fay *Microsoft* 

### Research Area 1: Urban Mobility and Connected Citizens

This repository contains a prototype recommendation engine design for mobility systems to provide more reliable paratransit for elderly. 
[Project page](http://c2smart.engineering.nyu.edu/2018/02/20/city-scalable-destination-recommender-system-for-on-demand-senior-mobility-new-2/).
[Project report](http://c2smart.engineering.nyu.edu/wp-content/uploads/2019/01/C2SMART_Destination_Recommender_System_Report.pdf).

# ![framework](https://github.com/BUILTNYU/recommender-system/blob/master/docs/framework.png)

Data Processing steps to create input files for [recommendation algorithm testing](https://github.com/BUILTNYU/recommender-system/blob/master/src/LinUCB/contextual-bandit-SVD-recommender.ipynb) can be found [here](https://github.com/BUILTNYU/recommender-system/blob/master/src/DataProcessing.R). Dataset can be downloaded from the [Yelp dataset challenge](https://www.yelp.com/dataset). 

Recommendation algorithm can also be explored by running the [LinUCB implementation using simulated data](https://github.com/BUILTNYU/recommender-system/blob/master/src/LinUCB/LinUCB-without-routing-cost.R). 

Implementation of the routing based on insertion heuristic and the UCB algorithm (with logistic regression) can be found [here](https://github.com/BUILTNYU/recommender-system/tree/master/src/UCB-GLM). Please refer to the comments for better understanding of the steps. 

## Acknowledgements
Xuebo Lai helped with early development of the recommender system which is gratefully acknowledged. We would also like to thank David Sinclair, a student at NYU Wagner, for his help in conducting the elderly mobility survey in NYC. The survey research was conducted in collaboration with Professor Kelvin Cheu at the University of Texas at El Paso and his team there. Participation in the NSFRCN Workshop on Smart and Connected Communities and Aging Population at Stony Brook University on April 20, 2018 helped shed light on some recent senior surveys conducted nationwide, which is much appreciated. Funding and support is generously provided by [C2SMART](http://c2smart.engineering.nyu.edu/) Tier 1 USDOT University Transportation Center at NYU. 

[![LinkedIn Follow](https://img.shields.io/badge/LinkedIn-Follow-blue.svg)](https://www.linkedin.com/groups/7040021/)
