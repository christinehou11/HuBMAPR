This is not a yet a public repository, so clone the package to a local
directory

```
git clone https://github.com/mtmorgan/HuBMAPR
cd HuBMAPR
```

In R, load the package

```{r}
devtools::load_all()
```

Retrieve a tibble of datasets, samples, or donors with commands like

```{r}
datasets()
```

Note the `columns=` argument, with `"portal"` corresponding
approximately to columns reported on the HuBMAP portal, `"some"` the
columns with mostly non-NA values, and `"all"`. Other hints are in the
documentation `?datasets`, etc.

Details on an individual dataset can be retrieved with, e.g.,

```{r}
uuid <-
    datasets() |>
    slice(1) |>
    pull("uuid")

json <- dataset(uuid)
```

Currently the best way to navigate this is with

```{r}
listviewer::jsonedit(json)
```
