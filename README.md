# Zoology

This Shiny graphic provides the lecturer an environment to explain the pH effect on the oxygen affinity of blood haemoglobin step by step. When invoking the interactive graphic, the oxygen-haemoglobin dissociation curve is displayed for a physiological pH environment. The lecturer can now choose whether to acidify or alkalize the environment, which in return leads to changes in the dissociation curve. These changes in haemoglobin oxygen affinity can now be discussed with the students.
For further interactive teaching, different explanations that clarify the shift of oxygen affinity are implemented. Thus, the lecturer can first discuss possible answers with the students and then select the answers from a selection list. Furthermore, users may obtain additional information about oxygen carriers by clicking on a second tab located on the top of the application.

### Installation

Copy all folders and files in one folder named "Zoology" on your desktop PC or server.
Make sure that the R-package Shiny and further R-packages are installed:
```r
install.packages(stringi)
install.packages(shiny)
install.packages(shinythemes)
install.packages(markdown)
install.packages(shinyBS)
```

The App can be run from R using the following code:

```r
library(shiny)
folder = "..\\Zoology" ### specify the path on your desktop PC to the Zoology folder
runApp(folder)
```
