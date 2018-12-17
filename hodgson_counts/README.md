## Adaptation of Matlab code from Hodgson et al. 2018  

Hodgson JC, Mott R, Baylis SM, Pham TT, Wotherspoon S, Kilpatrick AD, Raja Segaran R, Reid I, Terauds A, Koh LP (2018) Drones count wildlife more accurately and precisely than humans. Methods in Ecology and Evolution 9(5): 1160-1167. https://doi.org/10.1111/2041-210x.12974

The Matlab code is available at this DOI:  
https://doi.org/10.5061/dryad.rd736

## Running code with data from waterbird (Ibis) colonies  
I haven't replicated the source code here, just training/test data sets.
You just need to take the contents from ```/data``` in this repo, and update the appropriate folders the ```/data``` folder when you download the Matlab source code.  

Replace the ```/70``` folder in:  
 - ```/testing_images```
 - ```/training_background```
 - ```/training_birds```

Then you can just update the ```/src/demo.m``` Matlab script to point to the '70' folders for trianing the model and either the 'b1' or 'matlab1' test images  

I couldn't really get it to work very well, so please let me know if you can.  


