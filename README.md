# HighPT

HighPT is a Mathematica package for the analysis of high-energy data of semileptonic transitions at hadron colliders. It allows to compute high-$p_T$ tail observables for semileptonic processes, i.e. Drell-Yan cross sections, for dilepton and monolepton final states, at the LHC. 

These observables can be calculated within:
* The Standard Model Effective Field Theory at tree-level, including the relevant operators up to mass dimension eight to ensure a consistent description of the cross section including terms of $\mathcal{O}(\Lambda^{−4})$ in the EFT cutoff scale $\Lambda$.
* BSM models with new tree-level mediators, that can be resolved at LHC energies, enabling the computation of the cross section including the full propagation effects of these new particles.

The main routines of HighPT allow to compute:
* differntial cross sections,
* integrated cross sections,
* event yields,
* likelihoods.

The experimental searches for heavy resonances, based on the full run-II data by ATLAS or CMS, recasted and implemented in HighPT are:
* pp → ττ | ATLAS [\[2002.12223\]](http://arxiv.org/abs/2002.12223)
* pp → μμ | CMS [\[2103.02708\]](http://arxiv.org/abs/2103.02708)
* pp → ee | CMS [\[2103.02708\]](http://arxiv.org/abs/2103.02708)
* pp → τν | ATLAS [\[ATLAS-CONF-2021-025\]](https://cds.cern.ch/record/2773301/)
* pp → μν | ATLAS [\[1906.05609\]](http://arxiv.org/abs/1906.05609)
* pp → eν | ATLAS [\[1906.05609\]](http://arxiv.org/abs/1906.05609)
* pp → τμ | CMS [\[2205.06709\]](http://arxiv.org/abs/2205.06709)
* pp → τe | CMS [\[2205.06709\]](http://arxiv.org/abs/2205.06709)
* pp → μe | CMS [\[2205.06709\]](http://arxiv.org/abs/2205.06709)

## Reference

If you use HighPT please cite: 
* L. Allwicher, D. A. Faroughy, F. Jaffredo, O. Sumensari, and F. Wilsch, *HighPT: A Tool for high-$p_T$ Drell-Yan Tails Beyond the Standard Model*, [\[2207.xxxxx\]](https://arxiv.org/abs/2207.xxxxx)
* L. Allwicher, D. A. Faroughy, F. Jaffredo, O. Sumensari, and F. Wilsch, *Drell-Yan Tails Beyond the Standard Model*, [\[2207.xxxxx\]](https://arxiv.org/abs/2207.xxxxx).

## Installing and loading of the package

The simplest way to download and install HighPT is to run the following command in a Mathematica session:

```
Import["https://github.com/HighPT/HighPT/raw/master/install.m"]
```

This will download and install HighPT in the Applications folder of Mathematica's base directory. 
Alternatively you can also download the package manually from this website. In this case the path to the directory containing the HighPT code must be specified every time before lading the package by
```
PrependTo[$Path,"path/to/HighPT/directory"]
```

To load HighPT use the command:

```
<< HighPT`
```

The complete set of routines and usage examples can be found in [\[2207.xxxxx\]](https://arxiv.org/abs/2207.xxxxx). This repository also contains several example notebook that can serve as templates.

## Authors

* **Lukas Allwicher** - *University of Zurich*
* **Darius A. Faroughy** - *University of Zurich*
* **Florentin Jaffredo** - *IJCLab, Orsay*
* **Olcyr Sumensari** - *IJCLab, Orsay*
* **Felix Wilsch** - *University of Zurich*

## Bugs and feature requests

Please submit bugs and feature requests using GitHub's [issue system](https://github.com/HighPT/HighPT/issues).

## License

HighPT is released under the MIT License.


## Acknowledgments

This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme under grant agreement 833280 (FLAY), and by the Swiss National Science Foundation (SNF) under contract 200020-204428.
