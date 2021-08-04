# mADBio

The **madbio** package implements the m-ADBio epidemic forecasting model. The m-ADBio model, or Multi-Level Adaptive and Dynamic Biosensor model, is a compartmental model. 

Similar to the standard SIR/SEIR model, m-ADBio forecasts the number of Susceptible, Exposed, Infectious, and Recovered individuals at discrete points in time. In contrast to SIR/SEIR, which assumes that the geography being modeled is a closed system, the m-ADBio model incorporates commuters to and from areas outside that being modeled. Furthermore, m-ADBio is able to incorporate assumptions regarding data gathered via biosensors in addition to data gathered via standard testing protocols. This can result in more accurate epidemic forecasts. 

For a guide on how to use this package, please see the "Getting-Started" vignette, and refer to the documentation for individual functions for detailed input parameters.
