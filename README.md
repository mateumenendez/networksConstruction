# Co-occurrence/exclusion networks construction

This repository includes the needed R functions and scripts for building co-occurrences and co-exclusions networks as detailed in Menéndez-Serra et al 2021. Network construction method is based on the probabilistic method propose by Vellend (2013) and the conceptual framework proposed by Blois and collaborators (2014). This repository also includes a test dataset consisting on the necessary files for building the co-occurrence and co-exclusion networks of the microbial metacommunity inhabiting the Monegros Desert lacustrine system, described in Menéndez-Serra et al 2021. 

## Functions

veech(): determine statictically significant co-occurrences and co-exclusions between pairs of zOTUS.

blois(): detemine the potential explanation of observec co-occurrence/exclusions based on the selected environmental variables and ponds spatial distributions.

collapseBlois(): integrate output from "blois" function for all testes environmental variables.

## Data

envMonegros.RData: environmental data linked to the samples included on the Monegros Desert lacustrine system dataset.

tableMonegros.RData: presence/absence observations table corresponding to the Monegros Desert lacustrine system dataset.


## References

Blois, J. L., Gotelli, N. J., Behrensmeyer, A. K., Faith, J. T., Lyons, S. K., Williams, J. W., … Wing, S. (2014). A framework for evaluating the influence of climate, dispersal limitation, and biotic interactions using fossil pollen associations across the late Quaternary. Ecography, 37(11), 1095–1108. doi: 10.1111/ecog.00779

Menéndez-Serra, M., Ontiveros, V., Barberan, A., & Casamayor, E. (2021) Increasing saline stress reduces microbial co–exclusions while co–occurrences remain stable pointing to a lack of stress promoted facilitation. Not yet published.

Veech, J. A. (2013). A probabilistic model for analysing species co-occurrence. Global Ecology and Biogeography, 22(2), 252–260. doi: 10.1111/j.1466-8238.2012.00789.x!
