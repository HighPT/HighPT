# HighPT

`HighPT` is a `Mathematica` package for the analysis of high-energy data of semileptonic transitions at hadron colliders. It allows to compute high-p<sub>T</sub> tail observables for semileptonic processes, i.e. Drell-Yan cross sections, for dilepton and monolepton final states, at the LHC. 

More information can be found on the `HighPT` website at https://highpt.github.io.

These observables can be calculated within:
* The Standard Model Effective Field Theory at tree level, including the relevant energy-enhanced operators up to dimension eight to ensure a consistent description of the cross section including terms of $\mathcal{O}(\Lambda^{−4})$ in the EFT cutoff scale $\Lambda$.
* BSM models with new tree-level mediators, that can be resolved at LHC energies, enabling the computation of the cross section including the full propagation effects of these new particles. Currently only leptoquark mediators are implemented. The available mediator masses are:
    * 1 TeV
    * 2 TeV
    * 3 TeV
    * 4 TeV \[coming soon\]
    * 5 TeV \[coming soon\]

The main routines of `HighPT` allow to compute:
* differential cross sections,
* integrated cross sections,
* event yields,
* likelihoods.

---

## Experimental searches

The experimental searches for heavy resonances, based on the full LHC run-II data gathered by ATLAS or CMS, recasted and implemented in `HighPT` are:

| Process | `HighPT` label | Experiment | Reference |
| ------- | ------------ | ---------- | --------- |
| $pp\to\tau^+\tau^-$ | `"di-tau-ATLAS"` | ATLAS | [\[2002.12223\]](http://arxiv.org/abs/2002.12223) |
| $pp\to\mu^+\mu^-$ | `"di-muon-CMS"` | CMS | [\[2103.02708\]](http://arxiv.org/abs/2103.02708) |
| $pp\to e^+ e^-$ | `"di-electron-CMS"` | CMS | [\[2103.02708\]](http://arxiv.org/abs/2103.02708) |
| $pp\to\tau^\pm\nu$ | `"mono-tau-ATLAS"` | ATLAS | [\[ATLAS-CONF-2021-025\]](https://cds.cern.ch/record/2773301/) |
| $pp\to\mu^\pm\nu$ | `"mono-muon-ATLAS"` | ATLAS | [\[1906.05609\]](http://arxiv.org/abs/1906.05609) |
| $pp\to e^\pm\nu$ | `"mono-electron-ATLAS"` | ATLAS | [\[1906.05609\]](http://arxiv.org/abs/1906.05609) |
| $pp\to\tau^\pm\mu^\mp$ | `"muon-tau-CMS"` | CMS | [\[2205.06709\]](http://arxiv.org/abs/2205.06709) |
| $pp\to\tau^\pm e^\mp$ | `"electron-tau-CMS"` | CMS | [\[2205.06709\]](http://arxiv.org/abs/2205.06709) |
| $pp\to e^\pm\mu^\mp$ | `"electron-muon-CMS"` | CMS | [\[2205.06709\]](http://arxiv.org/abs/2205.06709) |

---

## Reference

If you use `HighPT` please cite: 
* L. Allwicher, D. A. Faroughy, F. Jaffredo, O. Sumensari, and F. Wilsch, *HighPT: A Tool for high-*$p_T$ *Drell-Yan Tails Beyond the Standard Model*, [\[arXiv:2207.10756\]](http://arxiv.org/abs/2207.10756)
* L. Allwicher, D. A. Faroughy, F. Jaffredo, O. Sumensari, and F. Wilsch, *Drell-Yan Tails Beyond the Standard Model*, [\[arXiv:2207.10714\]](http://arxiv.org/abs/2207.10714).

---

## Installing and loading of the package

The simplest way to download and install `HighPT` is to run the following command in a `Mathematica` session:

```wl
Import["https://github.com/HighPT/HighPT/raw/master/install.m"]
```

This will download and install `HighPT` in the Applications folder of `Mathematica`'s base directory. 
Alternatively you can also download the package manually from this website. In this case the path to the directory containing the `HighPT` code must be specified every time before loading the package by:
```wl
PrependTo[$Path,"path/to/HighPT/directory"]
```

To load `HighPT` use the command:

```wl
<< HighPT`
```

The complete set of routines and usage examples can be found in [\[arXiv:2207.10756\]](http://arxiv.org/abs/2207.10756). This repository also contains several example notebook that can serve as templates.

---

## Authors

* **Lukas Allwicher** - *University of Zurich*
* **Darius A. Faroughy** - *University of Zurich*
* **Florentin Jaffredo** - *IJCLab, Orsay*
* **Olcyr Sumensari** - *IJCLab, Orsay*
* **Felix Wilsch** - *University of Zurich*

---

## Bugs and feature requests

Please submit bugs and feature requests using GitHub's [issue system](https://github.com/HighPT/HighPT/issues).

---

## License

`HighPT` is released under the [MIT License](https://github.com/HighPT/HighPT/blob/master/LICENSE).

---

## Acknowledgments

This project has received funding from the European Research Council (ERC) under the European Union’s Horizon 2020 research and innovation programme under grant agreement 833280 (FLAY), and by the Swiss National Science Foundation (SNF) under contract 200020-204428.
