Development on CVPIA DSM calibration inputs has moved to [Central Valley Project Improvement Act - Open Science Collaborative](https://github.com/CVPIA-OSC). This repository remains posted for archive purposes. The calibration inputs were used to inform FY2019-20 recommendations. Calibration inputs were developed through a formal Structured Decision Making process by the [CVPIA Science Integration Team](http://cvpia.scienceintegrationteam.com/).

------
<img src="cvpia_logo.png" align="right" width="40%"/>

### Calibration Data for the CVPIA SIT Model
*This data package contains habitat and temperature data for calibrating theCVPIA SIT Salmon Life Cycle Model.*

#### Installation
``` r
# install.packages('devtools')
devtools::install_github('FlowWest/cvpiaCalibration')
```

#### Usage
``` r
# view documentation about fall habitat values
?fall_habitats

# access American River monthly mean temperature values
cvpiaCalibration::amer_temp

# access American River proportion diverted
cvpiaCalibration::amer_prop_div

# access American River total diverted
cvpiaCalibration::amer_tot_div

```

<style>.logo{margin-top: 40px;}</style>
<div class = 'logo'>Data Assembled and Maintained by <a href = "http://www.flowwest.com/" target = "_blank"> <img src="TransLogoTreb.png" width="150px"/></div>
