# Interactive web service of PCA and CA analysis by Shinyapp

#### Name: [your name in Chinese or English]
#### Student ID: [your student ID]
#### ShinyApps link: [<https://yourname.shinyapps.io/NCCU_DS2023_hw4_studentID/>]

## Description

<p align="center">
 <img src="/images/PCA.png" width="48%" height="48%" >
 <img src="/images/CA.png" width="48%" height="48%" >
<p/>


- Make the interactive web service of PCA and CA analysis by Shinyapp
- You might start by integrating the following example (pcr.R) into shiny app. Of course, feel free to find other appropriate packages for PCA.

### pca.R

```R
data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
```
ggbiplot is not on CRAN, it is only available on GitHub. It can be installed by following cmmand.
```
install.packages('remotes', dependencies = TRUE)
remotes::install_github("vqv/ggbiplot")
```

### How to publish your work on shinyapps.io? 🤔
At the beginning, you have to sign up for [shinyapps.io](https://www.shinyapps.io/) account first
 
In RStudio IDE, you can manage your shinyapps.io accounts by going to `Tools → Global Options → Publishing`. 

<p align="center">
 <img src="/images/shinyapp_on_rstudio.png" width="70%" height="70%" >
<p/>

After connecting your account you can start making your shinyapp. Please comply with the following folder structure. where `110753xxx`need to change to your student number.
```
110753xxx
   |-- app.R
```
After finishing your shinyapps code, you can preview your website in the console with the following command.  
```
library(shiny)
runApp("110753xxx")
```
Then, you can see the following screen, and there is a `publish` button in the upper right corner, click it to upload the website.
Please note that you need to make sure your title conforms to the format → `NCCU_DS2023_hw4_studendID`

<p align="center">
 <img src="/images/shinyapp_on_rstudio_2.png" width="60%" height="60%" >
<p/>

## Scores: By Peer Evaluation
### Base Task (80 pts)
- [ ] [20 pts] Basic information: `name`、`department`、`student number`
- [ ] [30 pts] Make a shiny interactive web site to show PCA analysis (as the following picture) for the iris data such that users can specify which component to show (i.e., PC1, PC2, PC3 ...)
- [ ] [30 pts] Make a shiny interactive web site to show correspondence analysis (CA) analysis for the iris data

### Subjective (20 pts)
- [ ] [5 pts] Aesthetic 🌷
- [ ] [5 pts] Interactivity 🖥️
- [ ] [5 pts] Content rich 📖
- [ ] [max 5pts, each for 2 pts] Extra visualizations or tables to show more information 
  * input data
  * PCA result (i.e., amount of variances ... )
  * ...

### Penalty: -5 points of each classmate works 🙅‍♂️ 
Points will be deducted if you do not help grade other students' work!!

## Notes
* Please use R version 4
* This assignment does not accept make up. 👀
* About Peer Evaluation
  * Each student's work will be evaluated by other classmates (randomly selected 6-8), and the final score of the project will be determined by taking the average of the remaining scores. 
  * Each student is also required to evaluate other people's work, and if they fail to evaluate one, points will be deducted.
  * Peer Evaluation will start within one week after the assignment is due. We will be notified to students through mail.
* About ShinyApps
  * Please share your shinyapp link & student ID on top of Readme.md
  * You must publish your work on [shinyapps.io](https://www.shinyapps.io/)，so that you can get the public link. Please make sure `your link is available`, and title conforms to the format → `NCCU_DS2023_hw4_studendID`
  * Please comply with the following folder structure.  where `110753xxx`need to change to your student number.
   ```
  110753xxx
     |-- app.R
   ```

## Example
#### https://changlab.shinyapps.io/ggvisExample/
#### https://smalleyes.shinyapps.io/NCCU_DS2023_hw4_110753202/
