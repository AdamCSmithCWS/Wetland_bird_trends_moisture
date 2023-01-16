# Wetland_bird_trends_moisture

## Collaborative project to separate annual fluctuations in wetland bird abundance from longer-term non-linear patterns in population change

Using data from the North American Breeding Bird Survey (intially for Black Tern), our goal is to model annual fluctuations and long-term population change. We intend to use annual moisture measures as covariates on the annual fluctuations and non-linear smoothing functions to estimate the longer-term patterns in populaiton change. 

The base-model will be very similar to the GAMYE (GAM with year-effects) model used by the CWS to estimate trends with the BBS data. We also intend to incorporate spatially explicit hierarchical structures using intrinsic Conditional Autoregressive components (iCAR) to share information among spatial strata. In effect the models will be spatio-temporal smoothing models that allow patterns in trends and annual fluctuations to vary in space and time, as well as the effects of covariates to vary in space.


