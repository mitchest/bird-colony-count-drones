# Towards monitoring large and complex wildlife aggregations with drones

- Data and code for an upcoming paper on monitoring and automated mapped of really large and complex bird colonies. 
- Paper just about in review. Contact for more details or check out the results as is.
- See [this Google Earth Engine app](https://www.google.com/url?q=https://mitchest.users.earthengine.app/view/ibis-drone-count&sa=D&source=hangouts&ust=1545213512105000&usg=AFQjCNFgaIsMC0XPmuhbXKHulS7t0_-IHA) to explore some of the data

The basic concept is as such:
- Most automated classificaiton algorithms focus on consistent targets with high contrast, but many biological phenomenon are not like this
- For example, many large breeding bird colonies are not like this (e.g. birds of different ages, nests of different ages that may be green/brown/white, nests that are on their own or in clumps of 5/10/50, empty or occupied nests, nests with eggs, birds by themselves, birds flying around, etc. etc.)
- This approach focuses on using a remtoe sensing approach (via drone imagery) to map all of the targets of interest, and then subsequently using an algorithmic or modelling appraoch to estimate the parameter of interest (e.g. number of nests in our case)
- We demonstrate for 4 different colonies, ranging in size from ~20-30,000 to ~200,000 birds
- The mapping is implemented in the Google Earth Engine, and modelling in R

A small extract from one colony is below, to give a flavour of the study sites - this is a nice simple example with pretty consistent targets.

### Orthomosaic:
- ~3 cm RGB drone imagery, processed via Pix4D  
![](https://github.com/mitchest/bird-colony-count-drones/blob/master/readme/ortho.JPG)

### Manual counting
- All visible nests were counted, a small proportion of which is used for training image classifiers  
![](https://github.com/mitchest/bird-colony-count-drones/blob/master/readme/nest_dots.JPG)

### Nest/bird classificaiton
- Nest material and birds classified  
![](https://github.com/mitchest/bird-colony-count-drones/blob/master/readme/nest_class.JPG)

### Nest count estimation
- The mapped areas are then used - again using a small proportion of the maunally coutned nest data as training - to estimate the parameter of interest: the number of nests
- We are trying to maximise the accuracy of the counts, and minimise the amount of manual effort
- Note these reuslts are not finalised yet  
![](https://github.com/mitchest/bird-colony-count-drones/blob/master/readme/nest_est.png)
