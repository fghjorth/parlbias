---  
header-includes:
  - \usepackage{graphicx}
  - \usepackage{dcolumn}
  - \usepackage{booktabs}
  - \usepackage{tgtermes}
  - \usepackage[margin=1.2in]{geometry}
title: Revision notes for "Intergroup Bias in Parliamentary Rule Enforcement"
author: Frederik Hjorth  
date: February 5, 2016
---  

# Revision memo

Dear Editors of Political Research Quarterly,

Thank you for giving me the opportunity to revise my manuscript "Intergroup Bias in Parliamentary Rule Enforcement" (Manuscript ID PRQ-2015-0341). I greatly appreciate the time and effort taken by yourself and the four reviewers to review the manuscript. The comments and suggestions by yourself and the reviewers have been extremely helpful and I have made my best effort to accommodate the suggestions in the revised version of the manuscript. I believe the changes have made for a stronger, clearer, and more persuasive paper.

Below I address the specific comments made by each of the reviewers in turn. These sections are labeled 1.1 for R1's first comment, 1.2 for R1's second comment, and so on.

In response to several of the reviewers' comments, I have added new figures and tables to the manuscript, some in the main text, some in the appendix. For ease of reading, I reproduce these tables and figures below, along with other material only shown in this note.

## Reviewer 1

### 1.1: Data to justify 'as if' randomness

R1 requests more empirical data in support of the claim that assignment of chairpersons to debates is "as if" random. Specifically, R1 asks for a graph showing the percentage of seats controlled by the chairperson on the x-axis and the percentage of times chairing a debate on the y-axis, noting that the points in such a graph should fall along a 45 degree line.

I thank R1 for this suggestion. However, I do not believe the predicted pattern would constitute evidence in support of "as if" random assignment. The reason for this is that members of the leadership (with some exceptions for the head chairperson) are treated equally, with no consideration given to their party's share of seats in parliament. Hence, a chairperson from the 5th largest party has the same obligations as the chairperson from the largest party (again, with some exceptions for the head chairperson). Seat share should thus not be a predictor of chairperson activity. I want to stress that the manuscript was not sufficiently clear on this point, and I have added a paragraph on p. XXX making this point more explicitly.

As additional evidence for the "as if" random assignment of chairpersons to debates, I have added section A.1, "Predicting chairman activity", to the appendix. Here I present results from regression models predicting how many debates chairpersons control. The key result from these models is that the main predictor of number of debates controlled is how long a chairperson has been a member of the leadership. There is an additional small premium for being the head chairperson, which likely reflects the fact that by convention, the president always takes the first shift controlling a debate. In contrast, number of seats has no significant association. The results are shown below.

\input{../tables/parlbias_remarksregtab.txt}

The results complement the qualitative evidence from correspondence with the parliament leadership that chairperson activity is largely driven by 'supply-side' factors, i.e. chairpersons' availability on the day of the debate, and not by party specific factors such as their parliamentary power.

### 1.2: Abrupt conclusion

R1 notes that the conclusion is abrupt and should include more discussion of the implications of the finding.

I have extended the conclusion section with more discussion of the findings's implications. For the specific wording of the discussion, I refer to the manuscript. In short, I argue that the findings imply that intergroup bias can occur even in institutional and cultural settings that should work strongly against bias (the same reasons for which I argue the setting is a 'least likely' case). At the same time, the observed effect is fairly small, suggesting that the aforementioned factors do go some way towards mitigating bias. Lastly, and perhaps most importantly, they illustrate the importance of balancing institutions. As added to the discussion in section 5.2, the effects on speaking time for each chairperson roughly wash out within each debate due to the rotation of chairpersons. This does not nullify the finding per se, but illustrates how the principle of rotating chairperson roles can result in (approximate) equity in the aggregate even in the presence of bias at the individual level (see also the discussion in section 3.3 below). I think this added discussion has strenghtened the manuscript, and I thank R1 for the suggestion.

<!-- What does it imply about how legislative rules and institutions are formed?

What are plausible things that might mitigate this bias?

Does the answer to the question depend on the mechanism? If so, how? -->

### 1.3: Minor points

Lastly, R1 mentions some minor points. I have fixed the word in footnote 1 to 'modal', as was indeed intended. R1 also notes that section 5.2 seems unnecessary, which I agree with in principle. However, section 5.2, which discusses the magnitude of the effect, speaks to a criticism sometimes raised against the finding: namely, that the observed effect is fairly small. This is indeed the case, and in section 5.2, I make an effort to be explicit about this point. Hence, I have opted to keep section 5.2, though I agree that it could be omitted if need be.

## Reviewer 2

### 2.1: Data to justify 'as if' randomness

R2 also suggests additional evidence justifying the assumption of "as if" randomness. Specifically, R2 expresses concern that chairmen might self-select into particular issues of high importance to the party. If party members also take more time discussing these issues, this self-selection would produce a spurious finding of own-group bias. As a way of countering this concern, R2 suggests showing that the issues debated are similar in copartisan and non-copartisan matches.

This is indeed a relevant concern, and I thank R2 for pointing it out. [HVORFOR MAN IKKE KAN MÅLE INDHOLD]

However, two aspects of how debates are organized indicate that issue-based self-selection is not likely. First of all, opening or closing debates are not about any one issue, but cover a multitude of issues, mostly based on issue disagreements between parties. MP's may debate one issue for a few remarks, and then shift to another issue. Second of all, the schedule of which chairmen oversee the debate is set in advance of the debate. Hence, chairmen are not able to opt in to the role when a particular issue comes up in the debate. The combination of these two aspects suggests self-selection based on issues is unlikely. I have added a paragraph in section 3.1 of the manuscript making this point explicitly.

For additional evidence in favor of the "as if" randomness assumption, see section 1.1 in this note.

### 2.2: Debate-specific fixed effects

As additional evidence in favor of the "as if" randomness assumption, R2 suggests employing debate-specific fixed effects. As mentioned, each debate covers several issues, so debate fixed effects should not be interpreted as holding issue content constant. Nevertheless, debate-specific fixed effects is a valuable robustness check of the results. Table 7 in section A.2 in the appendix shows the main results with debate-specific fixed effects added in every specification. The results are robust to including debate-specific fixed effects. The table is also shown below.

\input{../tables/parlbias_regtabdfe.txt}

### 2.3: More information about speaking assignment rules

TJEK OM DEBATSYSTEMET ER ELEKTRONISK

add in some stuff from forretningsordenen

do chairmen call on more people from his/her own party? add discussion about this

### 2.4: Discuss the odd shape of Figure 2

Lastly, R2 points to the somewhat puzzling bimodal shape of the distribution of non-copartisan response times shown in Figure 2. The shape already receives some discussion in the text, but as R2 rightly points out, it is not quite the shape one would expect given just some average bias by chairmen.

To explore this issue further, I have changed the figure to present the distribution for non-copartisans separately for members of the chairman's own parliamentary bloc and for members of the other bloc. The revised figure is shown below.

\includegraphics[scale=.7]{../figures/parlbias_dens.pdf}

The pattern suggests some of the difference reflects self-censorship.

## Reviewer 3

### 3.1: Analysis of distinction between rational choice and social identity processes

R3 argues that while there is some discussion of the distinction between rational choice and social identity processes, there is no analysis shedding light on this distinction. I do not agree with this assessment. Section 5.3 of the manuscript, testing the role of political moderators, has the purpose of exploring this distinction. As I argue, the fact that the bias is diminshed when looking only within the chairman's own bloc is suggestive of a rational mechanism, though the test is not conclusive. I appreciate the comment, and I have rewritten section 5.3 to make the purpose of the section clearer.

### 3.2: Partisanship as a 'soft' test

R3 notes that relative to a minimal group experiment, demonstrating an effect of partisanship is a relatively 'soft' test of social identity. As R3 writes, "parties for MPs are not minimal groups".

This point is well taken, and the test here is indeed softer than the test in Tajfel et al.'s original minimal group experiments. However, the contribution of this study is to test for intergroup bias in an ecologically valid setting. In contrast to experimentally assigned minimal groups, this study shows discrimination in naturally occurring groups. It is in the context of naturally occurring groups that I argue the setting constitutes a 'hard' test, insofar as it is characterized by transparency, limited political polarization and low levels of corruption. Hence, the study should be seen as a complement to minimal group experiments.

### 3.3: Pattern of queues of brief remarks

R3 argues that "the fact that certain talks - most probably on not bi-partisan but polarizing issues - leads to a queue of brief remarks point to another impact not accounted for". I am unsure of which aspect of the debates R3 refers to here, but on the question of chairman self-selection into issues, see the discussion in section 2.1 above.

### 3.4: Cancelling out of co-partisanship effect in the aggregate

R3 notes that the (small) impact of copartisanship cancels out in the aggregate because the chairmanship role rotates between parties. I thank R3 for highlighting this point, and I have added a mention of this to section 5.2 of the manuscript. I also revisit this issue in the conclusion, cf. section 1.2 above.

### 3.5: Generalizability

R3 takes issue with the claim of generalizability of the result. I appreciate this point, and I fully agree that the result is not generalizable in the sense of representing in any way a random sample of theoretically relevant settings. Instead, the result allows for 'least likely' case inference because intergroup bias is *ex ante* relatively unlikely in the setting studied here. I have rewritten the conclusion to make it clearer that I refer to the latter sense and not the former. Specifically, the revised version of the conclusion does not use the word 'generalizability'. I thank R3 for pointing out this potential confusion.

### 3.6: Intentionality of social identity theory

R3 notes that social identity theory does not require an "honest" wish to behave in line with rules, which my discussion on p. 4 indicates. I thank R3 for pointing this out. The purpose of the sentence in question was to highlight that social identity theory predicts intergroup bias even if *chairmen* may have an honest wish to treat speakers neutrally. It was not meant to imply that this is a specific theoretical requirement of social identity theory. I have rewritten the sentence in question to make this distinction clearer.

### 3.7: Part 5 vs. Social Identity Theory

R3 argues that part 5 of the manuscript is not compatible with the social identity perspective. Part 5 of the manuscript is the entire results section, and it is unclear to which part R3 is specifically referring. However, section 5.3 in the manuscript does explore the distinction between rational choice and social identity processes (see also section 3.1 in this note). Though the test is not conclusive, it is indeed the case that the evidence is somewhat suggestive of a rational (as opposed to a social identity) mechanism. I would argue that the manuscript as it is is reasonably clear on this point.

### 3.8: Intergroup bias vs. bias

R3 argues that though the manuscript claims to show intergroup bias, "it doesn't, it shows only bias". I am unsure of precisely the distinction referred to here, but I read this comment as interpreting "intergroup bias" to refer specifically to a social identity process. That is not the intended meaning. "Intergroup bias" does not specifically imply a social identity process, but is perfectly compatible with a rational process. In other words, a chairman who fully rationally discriminates against non-copartisan speakers (say, with the purpose of political gain) can also be said to show intergroup bias. It is my understanding that this represents a conventional use of the phrase "intergroup bias".

### 3.9: Reporting model significance

R3 requests that Table 2 and others report model significance. I omitted model significance statistics in the original version only for presentational reasons, since including them made the results tables too wide to fit on the page. In the revised version, I have now squeezed the tables together a bit, and all the regression tables now report model significance (a $\chi^2$ statistic). As shown, all model significance tests reject the null of no improvement in fit compared to an intercept-only model.

### 3.10: Significance of 'prime minister' variable

R3 asks about the meaning of the significance of the "Prime Minister" (PM) variable. The question is very reasonable, and I should have elaborated on the variable in the original version of the manuscript. In short, the PM variable captures remarks given by the sitting PM. The variable was included in the original manuscript in order to show that the effect was not driven by PMs, who may by virtue of their office be able to self-select into speaking slots (as opposed to other MP's, cf. section 2.3 in this note). As is clear from the original results, the PM variable is not a plausible confounder, as the copartisan variable remains significant throughout, and the PM variable is itself not consistently significant.

In the revised version of the manuscript, I have chosen not to include the PM variable. I have done so because upon further consideration I believe it is not theoretically relevant to distinguish between PM's and MP's in the context of this study: outside of their formal PM speeches (which are already excluded from the data), PM's have no special role or privileges in debates, and so they partake in debates only in their capacity of MP. For this reason, I have decided not to distinguish between PM's and other MP's. Regardless of the status of MP's, they do not drive the results of the paper. In order to show this, Table XXX.YYY of the appendix presents the results when PM remarks are excluded. As shown, the results are substantively unchanged. I thank R3 for directing my attention to this variable, which was insufficiently explained in the original version of the manuscript.

## Reviewer 4

### 4.1: Interpretation

R4 discusses my interpretation of the observed effect, noting (entirely correctly) that I do not provide direct evidence that chairmen gavel non-copartisans faster than co-partisans. As R4 notes, excluding this possibility leaves two possible mechanisms:

1. MP's self-select strategically into the debate schedule
2. Copartisan chairmen induce an unconscious bias on part of the speaker by means of subtle signals such as body language

R4 argues that the second interpretation "is [not] equivalent to debates being enforced unequally -- the authors' interpretation".

I thank R4 for these considerations, though I do disagree with them in a few specific respects. First of all, while it is true that

R4 says (2) does not imply unequal enforcement. discuss.

### 4.2: Unexploited information about effect heterogeneity

is bias consistent across parties? coefficient masks a lot of heterogeneity

this is cool, I should just do this

\begin{figure}
\includegraphics[scale=.55]{../figures/parlbias_chairranefs.pdf}
\caption{Estimates from a random effects model with varying slopes by chairman. Effects are ordered by party position from left (top) to right (bottom) and by descending coefficient value within parties.}
\end{figure}
