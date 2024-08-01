### HuBMAPR

In R, load the package which is cloning from this repository.

```{r}
devtools::load_all()
```

In HuBMAP consortium, there are five main entity types, and each entity type with its related functions are included in one separate R script.

-   dataset: `datasets.R`

    -   datasets(size = 10000L, from = 0L): returns the selected information of all datasets in tibble. The maximum number of rows returned tibble is 10000L. size and from are numeric parameters, determining the arbitrary size to return and the location of dataset to return based on the last modified date.

    -   datasets_default_columns(as = c("tibble", "character")): returns the selected default columns as character or tibble included in the returned tibble from **datasets()**. The columns are *uuid, hubmap id, group name, dataset type, data types, organ, status, last modified timestamp,* and *donor hubmap id*.

    -   dataset_detail(uuid): return all available information of one specific dataset in tibble.

-   sample: `samples.R`

    -   samples(size = 10000L, from = 0L)

    -   samples_default_columns(as = c("tibble", "character")): the selected default columns are *uuid, hubmap id, group name, sample category, organ, last modified timestamp,* and *donor hubmap id*

    -   sample_detail(uuid)

    -   samples_derived(uuid, entity_type = c("Dataset", "Sample")): returns the derived Dataset(s) or Sample(s) information in tibble.

        -   Derived dataset(s) include columns *uuid, hubmap id, group name, dataset type, data types, status, last modified timestamp* (same as portal page)

        ![Example from HBM876.SJWP.278 (uuid = e3b74d607c3b20c9c2349d98c235ce72)](images/Screenshot%202024-07-31%20at%2019.32.12.png){style="class: center" width="270"}

        -   Derived sample(s) include columns *uuid, hubmap id, group name, sample category, last modified timestamp, organ*

            Note: The portal page does not show the tab to see the derived samples as the screenshot, but based on the provenance offered, there indeed is/are derived sample(s).

    ![Example from HBM876.SJWP.278 (uuid = e3b74d607c3b20c9c2349d98c235ce72)](images/Screenshot%202024-07-31%20at%2019.50.43.png){width="408"}

-   donor: `donors.R`

    -   donors(size = 10000L, from = 0L)

    -   donors_default_columns(as = c("character","tibble")): the selected default columns are *uuid, hubmap id, group name, Sex, Age, Body Mass Index, Race,* and *last modified timestamp*

    -   donor_detail(uuid)

    -   donor_derived(uuid, entity_type = c("Dataset", "Sample"))

        The returned tibble(s) for derived dataset(s)/sample(s) has same selected columns.

    ![Example from HBM666.XRTH.688 (uuid = a0bea4ae0ce4e03efd04c22cc38db644)](images/Screenshot%202024-07-31%20at%2019.53.29.png){width="242"}

-   collection: `collections.R`

    -   collections(size = 10000L, from = 0L)

    -   collections\_\_default_columns(as = c("tibble", "character")): the selected default columns are *uuid, hubmap id, title,* and *last modified timestamp*

    -   collection_information(uuid): print out the textual descriptions of specific collection

    -   collection_datasets(uuid): return the related datasets in one collection. The selected default columns are *uuid, hubmap id, data types, dataset types, last modified timestamp, organ, created by user display name,* and *status*

    -   collection_contacts(uuid): return the contact persons' *name, affiliation,* and *orcid id* in tibble

    -   collection_contributors(uuid): return the contributors *name, affiliation,* and *orcid id* in tibble

-   publication: `publications.R`

    -   publications(size = 10000L, from = 0L)

    -   publications\_\_default_columns(as = c("tibble", "character")): the selected default columns are *uuid, hubmap id, title, publication venue, publication date, publication status,* and *last modified timestamp*

    -   publication_information(uuid)

    -   publication_data(entity = c("Dataset","Sample","Donor"))

    -   publication_authors(uuid)

The complete list of data and visualizations available to download are mostly saved in Globus HuBMAP Collection, and `files.R` has the function files_globus_url(uuid) to download/access them. There are three different scenarios while accessing data and visualizations:

1.  UUID with data available via Globus: jump into Globus page

2.  UUID with data available via others (dbGAP or SRA):

    dbGAP stands for "database for Genotypes and Phenotypes"

    SRA stands for "Sequence Read Archive"

    1.  dbGAP yes + SRA yes

    2.  dbGAP yes + SRA no

print out message to notify that the datasets are not available via Globus and give them dbGAP and SRA (if available) for them to explore

3.  UUID not available: print out the message to notify the unavailability

`template.R` has functions language_template(language, name, ...) and json_template(name, ...) functions to run the json queries saved in inst/json.

`utilities.R` contains many internal functions.
