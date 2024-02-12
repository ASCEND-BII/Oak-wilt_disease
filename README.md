[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.8275122.svg)](https://doi.org/10.5281/zenodo.8275122)

# Mapping oak wilt disease from space using land surface phenology

<br />

These codes are used to predict the present of symptomatic oak trees to oak wilt 
disease using land surface phenology (LSP) metrics (Fig 1). LSP metrics are 
derived from Analysis Ready Data (ARD) of Sentinel-2 using the processing 
workflow of [FORCE](https://force-eo.readthedocs.io/en/latest/) (Fig. 2).

<p align="center">
 <img src="https://github.com/ASCEND-BII/Oak-wilt/blob/main/inst/Figure_1.png?raw=true" align="center" alt="500" width="500"/>
    <br>
    <em>Fig 1. Temporal changes in the Chlorophyll/Carotenoid Index (CCI) of a pixel from an oak tree
that died from oak wilt disease (a) and the expected behavior of land surface phenology metrics
when oak tree conditions are compared with the surrounding healthy vegetation for a given
phenological year (b).</em>
</p>

<br />

<p align="center">
 <img src="https://github.com/ASCEND-BII/Oak-wilt/blob/main/inst/workflow.png?raw=true" align="center" alt="720" width="720"/>
    <br>
    <em>Fig 2. Schematic workflow description.</em>
</p>

<br />

Most of the data used in our manuscript that accompany these codes are available 
at the [DRUM](https://doi.org). Please follow the instructions described there for 
proper citation of the data if used.

<br />

#### Products available

We made available all the scenes of [LSP](https://app.globus.org/file-manager?origin_id=d5f9b461-7d6e-442b-87ed-be8aa2ca6763&origin_path=%2F) 
(L3) and the predicted [maps of probabilities](https://app.globus.org/file-manager?origin_id=2ad70821-cc5a-424e-aa72-8553d2bb45eb&origin_path=%2F) (L4) for both states (i.e., Minnesota and Wisconsin). These can be accessible 
using a free [Globus](https://www.globus.org/) account.

#### Citing codes, manuscript, and data.

##### Codes

```
@codes{oak-wilt,
  author       = {Guzmán, J.A., Pinto-Ledezma, J., Frantz, D., Townsend, P.A., Juzwik, J., Cavender-Bares, J.},
  title        = {Mapping oak wilt disease from space using land surface phenology},
  month        = August,
  year         = 2023,
  publisher    = {Zenodo},
  version      = {v1.0},
  doi          = {10.5281/zenodo.8275122},
  url          = {https://doi.org/10.5281/zenodo.8275122}
}

```

##### Manuscript

```
@article{oak-wilt,
  author       = {Guzmán, J.A., Pinto-Ledezma, J., Frantz, D., Townsend, P.A., Juzwik, J., Cavender-Bares, J.},
  title        = {Mapping oak wilt disease from space using land surface phenology},
  journal      = {Remote Sensing of Environment},
  year         = {2023},
  volume       = {pending},
  pages        = {pending}
  doi          = {pending}
}

```
