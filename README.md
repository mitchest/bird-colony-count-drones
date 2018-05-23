# Monitoring large and complex groups of animals via drones: nesting bird colonies as an example

- Data and code for an upcoming paper on monitoring and automated mapped of really large and complex bird colonies. 
- Paper just about in review. Contact for more details or check out the results as is.

The basic concept is as such:
- Most automated classificaiton algorithms focus on consistent targets with high contrast, but many biological phenomenon are not like this
- For example, many large breeding bird colonies are not like this (e.g. birds of different ages, nests of different ages that may be green/brown/white, nests that are on their own or in clumps of 5/10/50, empty or occupied nests, nests with eggs, birds by themselves, birds flying around, etc. etc.)
- This approach focuses on using a remtoe sensing approach (via drone imagery) to map all of the targets of interest, and then subsequently using an algorithmic or modelling appraoch to estimate the parameter of interest (e.g. number of nests in our case)
- We demonstrate for 4 different colonies, ranging in size from ~20-30,000 to ~200,000 birds
- The mapping is implemented in the Google Earth Engine, and modelling in R

A small extract from one colony is below, to give a flavour of the study sites - this is a nice simple example with pretty consistent targets.

## Orthomosaic:
- ~3 cm RGB drone imagery, processed via Pix4D
![Alt text](readme/ortho.jpg?raw=true "orthomosaic")

## Manual counting
- All visible nests were counted, a small proportion of which is used for training image classifiers
![Alt text](readme/nest_dots.jpg?raw=true "nest counts")

## Nest/bird classificaiton
- Nest material and birds classified
![Alt text](readme/nest_class.jpg?raw=true "nest counts")

## Nest count estimation
- The mapped areas are then used - again using a small proportion of the maunally coutned nest data as training - to estimate the parameter of interest: the number of nests
- We are trying to maximise the accuracy of the counts, and minimise the amount of manual effort
- Note these reuslts are not finalised yet
![Alt text](readme/nest_est.jpg?raw=true "nest counts")
