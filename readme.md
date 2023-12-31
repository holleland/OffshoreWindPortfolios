Optimal allocation of 30GW offshore wind power in the Norwegian Economic
Zone
================

*Contributors: Sondre Hølleland<sup>†1</sup>, Geir Drage
Berentsen<sup>1</sup>, Håkon Otneim<sup>1</sup>, Ida Marie
Solbrekke<sup>2</sup>.*

*<sup>†</sup> Responsible for the code.*

<sup>1</sup> *Norwegian School of Economics, Norway.*

<sup>2</sup> *Forecast Engine, Norwegian research centre (NORCE),
Norway*

*Correspondance to: <sondre.holleland@nhh.no>*

This github repository contains the necessary code for reproducing the
modern portfolio analysis performed in the paper *Optimal allocation of
30GW offshore wind power in the Norwegian Economic Zone*. You may also
find the NORA3-WP dataset for the specific locations presented in the
paper (in the data folder). Some code may not run due to details not
published (e.g. some tables using the wind power suitability scores from
Solbrekke and Sorteberg(2023)).

### Data

The full NORA3-WP dataset (Solberg and Sorteberg, 2022) is available at
<https://archive.sigma2.no/pages/public/datasetDetail.jsf?id=10.11582/2021.00068>.
The extracted sample for the 40 locations used in the paper is published
here in agreement with the authors.

### Sequential build-out for the NVE regions

![](output/sequential_buildout_animation.gif)

In the animation above, we show the sequential build-out (from case D in
the paper), for a target capacity factor of 60%. The map on the left
show the number of turbines installed in each region, while the curve on
the right show the decreasing standard deviation as a function of
installed power. The dashed horizontal line corresponds to the
corresponding case A standard deviation.

## Author’s github account

**Sondre Hølleland** - [holleland](https://github.com/holleland)

## License

This project is licensed under [CC BY 4.0 LEGAL
CODE](https://creativecommons.org/licenses/by/4.0/legalcode), same as
the NORA3-WP data.

## References

- Ida Marie Solbrekke and Asgeir Sorteberg. [NORA3-WP: A high-resolution
  offshore wind power dataset for the Baltic, North, Norwegian, and
  Barents Seas](https://www.nature.com/articles/s41597-022-01451-x).
  Scientific Data, 9(1):362, December 2022.
- Ida Marie Solbrekke and Asgeir Sorteberg. [Norwegian offshore wind
  power – Spatial planning using multi-criteria decision
  analysis](https://onlinelibrary.wiley.com/doi/10.1002/we.2871). Wind
  Energy, 2023.
