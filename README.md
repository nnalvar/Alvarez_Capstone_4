# Alvarez_Capstone_4
Capstone project for IBS 538

### Alvarez_Natalie_Capstone

```
library(tidyverse)
library(ez)
library(viridis)
```

## 1) 
Provide a brief background and significance about a specific research problem that interests you. It could be project you’re involved with now, or a rotation project, or something you’d like to work on. The reader will need to understand enough background to make sense of the experiment you propose below. Keep it brief. In one short paragraph.

**Many treatments have been considered for the novel SARS-CoV2, which causes COVID-19. Some of the treatments include: NHC, remdesivir, and chloroquine, some of which have have had dose and cytotoxicity experiments in cell lines but it is important to do an in vivo experiment before considering clinical trials. One of the main reasons that SARS-CoV2 can have such a detrimental affect on the body is because of the immune response that it causes. It is actually your own immune system which causes tha damage in the body. Due to that, in vivo experiments are needed to have a complete picture on the efficacy of potential drugs. Viral load measured by plaque assay in plaque forming units/ml. Placebo control with no drug is also used in experiment. Mice will be infected with same SARS-CoV2 strain, given treatment at the same time and then 5 days post infection viral loads in the lung will be measured.**


## 2) 
Briefly state something that is unknown about this system that can be discovered through, and leads to, an experiment.  For example, "It is not known whether....."

**Previous studies have only looked at the effect of the best dose of the drugs and its cytotoxicity (NHC, remdesivir, and chloroquine) in cell lines. This experiment will be able to compare all 3 drugs in the same experiment and will use an in vivo murine model so that the immune systems response is also accounted for when viral load is measured.**

## 3)
Make an “if” “then” prediction that is related to item #2. It should be of the general form, “if X is true, then Y should happen”.

**If mice are given a drug (NHC, remdesivir, or chloroquine), then viral load will be different compared to if the mice were just given a placebo.**

## 4) 
What dependent variable will be observed to test this prediction in item #3? What predictor variable will be used to manipulate the system experimentally? Define the inherent properties of these variables (eg, are they sorted, ordered or measured).

**Dependent variable is viral titers measured in PFU/ml (plaque forming units per ml). Independent/predictor variable is the type of drug either: NHC, remdesivir, chloroquine or placebo control. Inherent properties are measured for the viral titers since the calculation of PFU/ml can result in decimals. For the drug treatment it is sorted.**

## 5) 
Write a statistical hypothesis.  There should be a null and alternate. These should be explicitly consistent with the prediction in item #3 and the response variable in #4. In other words, make sure the statistical hypotheses that you write here serves as a test of the prediction made in item #3. Ch10.3

**Null hypothesis is if the viral loads of the drug treatments are equal to the placebo. Alternate hypothesis is if the viral loads of the drug treatments are not equal to the placebo.**

## 6) 
What is the statistical test you would use to test the hypothesis in item #5? Briefly defend what makes this appropriate for the hypothesis and the experimental variables. If there are alternatives, why is this approach chosen instead? Points will not be awarded if the justification involves something like "because everybody does it this way".

**One way completely randomized ANOVA because the outcome variable of average viral titers in PFU/ml is random, continuous, and measured. Also because each mice only gets one of the drug treatments so it would not be related measures.** 

## 7) 
List the procedures and decision rules you have for executing and interpreting the experiment. These procedures range from selection of experimental units, to randomization to primary endpoint to threshold decisions. Define (and defend) what you believe will be the independent replicate.

**A one way randomized ANOVA will be used to look at the average viral titers. The sample size will be based on a power of at least 90% so tolerance for type2 error will be 10%. The decision threshold for type1 error will be 5% so the null will be rejected with a p-value less than 0.05. The experimental unit of treatment (which drug/placebo) will be randomly assigned- I will draw it out of a hat. Some constants within experiment include the type of mouse and its age at the start of the experiment will be the same. Each drug treatment/placebo group will have the same number of mice in the study. The n required to get a power of at least 90% will be determined below. The primary endpoint is if there is a statistical different between viral load in placebo vs a specific treatment group. An independent replicate is each group of mice per drug treatment.**

## 8) 
Produce a graph of a simulation for the expected results. Create a dataMaker-like function in R to create and plot the data. Label and scale any axis. The graph should illustrate the magnitude of the expected response, or the level of response that you expect to see and would be minimally scientifically relevant. Be sure to illustrate any variation that is expected.

```
b = 1000 
a = 1.9 
f = 1.25 
sd = 200 
n = 3 
sims = 100 

CRdataMaker <- function(n, b, a, f, sd) { 
  
  ##NHC/EIDD-2801, Remdesivir, and chloroquine,
  a1 <- rnorm(n, b, sd) #basal or negative ctrl
  placebo <- rnorm(n, (b*a), sd) 
  NHC<- rnorm(n, (b*f), sd) #treatment effect
  Remdesivir <- rnorm(n, (b*f), sd) #treatment effect
  chloroquine <- rnorm(n, (b*f), sd) #treatment effect
    
    Outcome  <- c(placebo, NHC, Remdesivir, chloroquine)
    Predictor <- c(rep(c("placebo", "NHC", "Remdesivir", "chloroquine"), each = n))
    ID <- as.factor(c(1:length(Predictor)))
    df <-data.frame(ID, Predictor, Outcome )
    }

dat <- CRdataMaker(n,b,a,f,sd)

ggplot(dat, aes(Predictor, Outcome))+
  geom_jitter(width=0.1,size = 4, alpha=0.5, color="blue")+
  labs(y="Viral titer in PFU/ml",
       x= "Drug Treatment Group")+
  ggtitle ("Viral Titer vs Drug Treatment Simulation")
```

 <img width="666" alt="Screen Shot 2020-04-27 at 6 29 48 PM" src="https://user-images.githubusercontent.com/64442826/80427610-25edfa80-88b6-11ea-8b44-ced26ea6be12.png">
 
 ## 9) 
Write and perform a Monte Carlo analysis to calculate a sample size necessary to test the hypothesis. This Monte Carlo must test the primary endpoint. 

**n=3 will give you 92% power. I envision this experiment having at least 3 mice/ treatment group to make it work which is very doable.**

```

b = 1000 
a = 1.9 
f = 1.25 
sd = 200 
n = 3 
sims = 100 

pval <- replicate(
  sims, {
 
    sample.df <- CRdataMaker(n, b, a, f, sd)
    
    sim.ezaov <- ezANOVA(
            data = sample.df, 
            wid = ID,
            dv = Outcome,
            between = Predictor,
            type = 2
            )
  
  pval <- sim.ezaov$ANOVA[1,5]
    
    }
  )

pwr.pct <- sum(pval<0.05)/sims*100
paste(pwr.pct, sep="", "% power. Change 'n' in your initializer for higher or lower power.")

ggplot(data.frame(pval))+
  geom_histogram(aes(pval), fill="red")+
  labs(x="p-value")
```

 <img width="670" alt="Screen Shot 2020-04-27 at 6 32 22 PM" src="https://user-images.githubusercontent.com/64442826/80427643-38683400-88b6-11ea-9f23-c006e8b26fee.png">
 
 ## 10) 
Write up it all in RMarkdown. Code chunks to illustrate specific points are welcome other than for the Monte Carlo code. Knit and submit and upload the html document by the due data. If it is readable to your best friend, it is readable to us.


EXTRA CREDIT: Github is an important resource for sharing data/analyses and collaborating.  In fact, 3 different Github repositories were used for the material in this course. We encourage you to submit your capstone as a Github page, as a gentle way to play with Github. See the directions Jessie wrote up: GitHub_pages_IBS538.pdfPreview the document

If you submit your capstone on Github, we'll need you to knit and submit a different document on Canvas. For that, just open up a blank Rmd. Only put the link to your github page in it. Knit and submit its html on canvas. The markdown code for a link is like this:

Thanks for such an exciting semester Austin, Jess, Lauren & even you, TJ! 
