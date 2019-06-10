DIY ANOVA is a Shiny app that guides the user through the process of checking assumptions for ANOVA and performing the actual analysis, plus checking pairwise differences by means of a section dedicated to post hoc tests.

It was developed by Danilo Pecorino (danilo.pecorino@gmail.com), who would love to receive feedbacks and comments on how to improve and make it more and more user friendly.

---

You can run this app locally by running R. Then type the following

library(shiny)
runApp("path to the DIY_ANOVA directory")

---

Requirements

DIY_ANOVA requires the following R libraries

shiny, shinyWidgets, shinydashboard, DT, GAD, multcompView, lsmeans

They can be installed by typing the following at the R console

install.packages(c("shiny", "shinyWidgets", "shinydashboard", "DT", "GAD", "multcompView", "lsmeans"))