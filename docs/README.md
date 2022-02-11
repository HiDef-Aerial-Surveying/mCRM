

<!-- badges: start -->
<img src='images/hexSticker.png' align="left" height="139" style="margin-right:20px;" />
<!-- badges: end -->

## mCRM R Shiny user guide  

The migration collision risk model (mCRM) shiny application is a stochastic adaptation of the Band (2012) migration collision risk worksheet.  The tool can be found at <a href="https://hidefdevo.shinyapps.io/mCRM" target="_blank">https://hidefdevo.shinyapps.io/mCRM</a> .   
The web application is a wrapper around the `r mig_stoch_crm()` function in the `{stochLAB}` package <a target="_blank" href="https://www.github.com/HiDef-Aerial-Surveying/stochLAB">https://www.github.com/HiDef-Aerial-Surveying/stochLAB</a>. The web application contains default values for 70 species of migratory birds as well as polygons that represent their approximate migratory pathways. The mCRM tool does a couple of things:
* Creates population estimates in wind farms by sampling migratory pathways via straight lines drawn between UK and non-UK coastlines
* Runs a stochastic version of the migratory collision risk model based on the population estimates and user-input parameters. 


