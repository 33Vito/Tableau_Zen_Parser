# Tableau_Zen_Parser
Shiny app to parse a Tableau workbook (.twb) file to show data source as well as visualise dependency between calculated fields, parameters and raw data.

shinyapps.io url: https://tony-yanjun.shinyapps.io/Tableau_zen_parser/ (performance depend on traffic)

## Client-side web app (no R dependency)
This repo now includes a static HTML/JS version that runs entirely in the browser.

Run it locally from a simple static server (recommended so the demo file can load):

```sh
cd /Users/yanjunliu/Documents/Tableau_Zen_Parser
python3 -m http.server 8000
```

Then open: http://127.0.0.1:8000

Files:
- `index.html`
- `app.js`
- `styles.css`

## R Shiny app (original)
To run locally,
1. Downlad [R](https://cran.r-project.org/bin/windows/base/) and [Rstudio](https://www.rstudio.com/products/rstudio/download/);
2. Install required packages: shiny, shinycssloaders, tidyverse, rvest, xml2 and visNetwork;
3. Download the app.R script from this repo, open the script in Rstudio, click 'Run App' in the top right corner of Rstudio editor.
