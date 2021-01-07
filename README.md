
<!-- README.md is generated from README.Rmd. Please edit that file -->

All code is available in [this GitHub
repo](https://github.com/tonyelhabr/bdb2021). One can install this R
package with `remotes::install_packages('tonyelhabr/bdb2021')` and run
[this
script](https://github.com/tonyelhabr/bdb2021/blob/master/data-raw/99_analyze_treatments.R)
to reproduce all figures. (I certainly don’t claim that this package is
well written; it’s just there to ensure that this analysis is
reproducible).

# 0. Introduction

I attempt to quantify whether there is a significant relationships
between play success, as measured by expected points added (EPA), when
exposed to one or both of the following “treatments”:

1.  Whether the targeted receiver on a given pass play is involved in a
    pick route combination or not.

2.  Whether the defender of the targeted receiver at the time of the
    throw is or is not the same as the defender at the time of the
    snap.[1]

Both when using a [causal
approach](https://en.wikipedia.org/wiki/Causal_analysis) and a more
traditional predictive approach (with
[{xgboost}](https://cran.r-project.org/web/packages/xgboost/index.html)
and [SHAP
values](https://christophm.github.io/interpretable-ml-book/shap.html)),
I find no evidence for targeted pick route combinations having a
significant effect on play success. This contrasts with the result found
from simply comparing the difference in EPA means with
[t-tests](https://en.wikipedia.org/wiki/Student%27s_t-test), i.e. a
[Frequentist
approach](https://en.wikipedia.org/wiki/Frequentist_inference).

Likewise, when investigating the role the targeted defender in pick
plays, I find that the causal and predictive approaches agree with one
another, but disagree with the un-adjusted Frequentist implication. The
causal and predictive approaches indicate that there may be a
significant relationship between EPA and the type of coverage on the
targeted receiver given that the pass is completed, but otherwise
suggest that the type of coverage doesn’t have a significant
relationship with EPA. On the other hand, the Frequentist approach more
generally implies that EPA is significantly different depending on the
coverage played on the targeted receiver.

## Motivation

> “Offensive coordinators call them rub concepts. Defensive coordinators
> call them illegal pick plays. Whatever you call them, they are hard to
> defend… offenses are designing these plays and training their
> receivers to essentially set screens to get their teammates wide open.
> Technically, it’s against the rule to set a pick deliberately, but
> referees have a hard time judging intentions and they rarely penalize
> these plays”. - [Ted Nguyen, USA Football,
> 2017](https://blogs.usafootball.com/blog/4177/rub-concepts-how-to-defend-them-and-how-offenses-can-counteract-it)

To illustrate the impact pick plays can have, below is the pick play
that resulted in the highest expected EPA in the 2018 season.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/highest_epa_pick_play.gif)

Likewise, below is the pick play that resulted in the lowest EPA (good
for the defense).

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/lowest_epa_pick_play.gif)

Just from these two extreme plays, one can get a sense for the high
amount of leverage that these plays can have on game flow and,
ultimately, outcome. In the best case (for the offense), pick plays can
be a great way for the offense to quickly get a receiver a good amount
of separation, setting him up for lots of yards after the catch (even on
quick plays). In the worst case, defenders can “jump” such routes,
leading to an interception and a relatively clear path towards the
opposing end zone.

(I’ve always thought that pick plays are harder to defend than your
average play, and that man coverage is likely worse than zone against
pick plays.)

## Outline

1.  Discuss and implement a methodology to detect pick route
    combinations from the tracking data.

2.  Discuss and implement a methodology for detecting defensive schemes
    on pick plays.

3.  Perform analyses of targeted picks plays and individual coverage of
    targeted receivers using (1) a Frequentist approach, (2) a causal
    approach, and a more traditional predictive approach.

4.  Quantify individual defensive back success when involved in pick
    routes.

## Actionable Outcomes

While the final evaluation is somewhat holistic, I believe that there
are several actionable learning outcomes:

1.  Teams could use the methodology for identifying pick route
    combinations for the purpose of offensive play classification
    (i.e. “Did the offense run a pick route?”), likely coming in the
    form of tagging of video analysis. Perhaps teams already do this or
    have a third-party do this for them, but if not, it could very
    useful for providing clear, reproducible classification of pick
    plays, not subjected to the biases of human video re viewers.

2.  Teams could use this analysis to supplement their evaluation of
    defensive schemes when covering pick route combinations, perhaps on
    a weekly basis (preparing for the next week’s game). Likewise, they
    may choose to run more or less pick route combinations on offense
    depending on their evaluation of their opponent’s ability to cover
    them. In particular, teams could evaluate the skill of specific
    defenders in covering such plays. I mostly rely on EPA, which,
    admittedly, is probably not granular enough to tease out individual
    impact in a robust, reliable manner; nonetheless, the measure of
    skill/success—whether it be something like reduction of separation
    distance, change in target probability, etc.–could be swapped out
    relatively easily. Individual evaluation is, of course, important
    for identifying weaknesses to target in practice sessions with
    existing team players, but it could also be a point of interest in
    recruitment of players (i.e. to identify free agents who tend to
    cover pick plays particularly well).

A caveat: the proposition that one can adequately evaluate defensive
skill from tracking data is debatable. What do I mean? Well, we do not
know for certain whether individual defenders were subjected to
“non-optimal” defensive play calls in certain situation since we do not
know the team defensive coverages for all plays. And it seems impossible
to account for the manner in which a given team may have coached
defenders to react dynamically in response to certain offensive actions.
This analysis cannot, and does not try to, quantify these unknown
factors. I simply focus on what can be quantified.

# 1. Identifying Pick Route Combinations

## Methodology

Perhaps the simplest data-driven way to identify pick combinations is to
derive it from (1) initial offensive player alignment at the time of the
snap—number of receivers on one side of the ball, order of receivers
relative to a specified sideline, distance between receivers—and (2) the
routes run by the receivers. The former can be inferred from the `x`
and`y` fields in the tracking data, and the latter is provided
explicitly. Upon visual investigation, I found that this method is much
too sensitive to some strict definition of pick route combinations
(e.g. `SLANT` + `OUT` for two receivers, `SLANT` + `{route}` + `OUT` for
3 receivers, etc.) and too naive to the fact that the same type of route
(e.g. a SLANT) can be run in a myriad of styles. Below is an example.

A second way to identify pick plays is to again (1) start with offensive
alignment and then (2) track the order of the receivers relative to the
sideline as the play progresses. One might say that a pick is detected
if the receivers change order relative to the sideline within some
pre-defined number of frames after the ball is snapped. While I did
evaluate this method, it too often flagged route combinations that might
better be described as a “mesh” or some kind of clear-out, where one
receiver runs much more down-field than the other. Below is an example.

Finally, I settled upon a methodology in which I traced out receiver
paths, identifying “intersections” between routes within 2 seconds (20
frames) of the snap. (While this is by no means a perfect method, it
seems to be relatively robust to false positives, e.g. deeper mesh
concepts.) Some additional criteria is applied:

1.  Receivers out of the backfield (i.e. mostly RBs) were disregarded.
    (Very few route intersections occurred for players coming out of the
    backfield anyways.)
2.  Each receiver’s path was traced out 1 yard back from their position
    at the snap. The intention of this is to capture stacked/bunched
    pairs of routes.

Why within 2 seconds of the snap? Well, I began by counting how frequent
intersections occurred up through `n` seconds after the
snap—specifically, 0.5 seconds, 1 second, 1.5 seconds, …, 3 seconds.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_intersections_after_n_sec.png)

This was certainly helpful to get some numbers in mind, but things
ultimately came down to me visually inspecting plays at each threshold
and identifying 2 seconds as the maximum that I felt comfortable with.

## Play Examples

Below is an example of a play where the intersection occurs between 2.5
and 3 seconds after the ball is snapped. Note how Cooper Kupp (18) and
Robert Woods (17) line up on different sides of the offense line and
cross paths. The intention of this example is to illustrate why the
choice of 2 seconds after the snap is used as the maximum time after
which a pair of crossing routes is not considered a pick route
combination. An experienced video analyst would surely say that this is
a more traditional crossing scheme.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/highest_epa_3.0s_pick_play.gif)

Likewise, the example below illustrates a route intersection—between the
routes of Jamison Crowder (80) and Vernon Davis (85)—that occurs between
2 and 2.5 seconds after the snap. Subjectively, this seems much closer
to being a true pick play—and a video analyst may agree; nonetheless,
the methodology does not classify it as so (since it occurs more than 2
seconds after the snap).

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/highest_epa_2.5s_pick_play.gif)

Below is an example in which the receiver paths did not actually
intersect, yet the play is still classified as a pick play due to the 1
yard backwards extension. The intention of this example is to illustrate
the usefulness of extending receiver paths one yard back for the purpose
of capturing legitimate pick actions. Note how Antonio Brown (84)
crosses just under Jesse James (81) nearly immediately after the snap.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/y_buffer_pick_play.gif)

## Team and Player Offensive Trends

As a sanity check on the pick route identification methodology, I
continue with some descriptive charts, illustrating larger trends.

Below is a summary of which teams ran pick route combinations most
often. The Los Angeles Rams are at top of the list, which makes sense
given what is empirically observed from Sean McVay’s offense. The
Washington Football Team’s place as second on this list is perhaps not
too surprising given Jay Gruden’s ties with McVay. Interestingly, the
New England Patriots have the lowest frequency, which may be surprising.
Without having watched every play of their season, my guess is that they
tend to run a lot of mesh and/or high-low route concepts that may be
classified as “downfield” picks; however, such route combinations often
don’t fit my criteria.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_pick_play_frac.png)

Given the above chart, perhaps it’s not surprising to find a Los Angeles
Rams receiver (Robert Woods) at the top of the list of receivers
involved in the most pick route combinations.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_picks_by_receiver.png)

Again, given the team breakdown, it’s no surprise to find 3 Rams players
(Robert Woods, Cooper Kupp, and Josh Reynolds) and 2 Football Team’s
players (Jordan Reed and Jamison Crowder) among the receivers with the
highest pick route involvement relative to their total number of routes
run.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_frac_by_receiver.png)

Julio Jones happens to be the one targeted the most when involved in a
pick route combination. Anecdotally, one can make sense of this. Jones
is known as one of the top receivers in the game, having a pretty
diverse route tree. 2018 was his personal second-best season in terms of
yards received and receptions.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_picks_by_receiver_target.png)

# 2. Identifying Defensive Coverage

## Methodology

Perhaps the simplest way for defensive backs to cover a pick route
combination is to “stay with their man” in a pure man-to-man coverage.

![](https://assets.usafootball.com/cms/inline-images/Nguyenpic3.jpg)

Another common way is to play a hybrid-man coverage (perhaps also called
matchup zone), where defensive backs play man-to-man on the receiver
that releases in their area. ([Nguyen describes this as a “banjo”
scheme.](https://blogs.usafootball.com/blog/4177/rub-concepts-how-to-defend-them-and-how-offenses-can-counteract-it))

![](https://assets.usafootball.com/cms/inline-images/Nguyenpic2.jpg)

Like the hybrid approach, a zone approach would also not involve the
defenders crossing paths along with the receivers. One might say that it
would be difficult to distinguish from said hybrid coverage without
labels.

Thus, for the sake of this analysis, I’ll simplify things to just: “Did
the defenders have the same receiver assignments at the time of the
throw compared to at the time of the snap?” If “yes”, I’ll say that they
played man-to-man coverage. (Maybe it wasn’t really man-to-man coverage,
but it’s certainly distinct.) If not, then I’ll just refer to it as “not
man-to-man”, or just “non-man”. (It could be the banjo type of coverage,
or some kind of more traditional zone coverage.)

For identifying defender “assignments”, I use [bipartite min-distance
matching, employing the Hungarian
method](https://en.wikipedia.org/wiki/Hungarian_algorithm). I make
assignments for all plays and all receivers, although we’ll be focusing
on the targeted receiver. (We don’t have a complete set of team coverage
labels, and we assert that it is not necessary for the study at hand, in
which we only care about the coverage of the targeted receiver.) This
assignment methodology overcomes the issue of having a single defender
assigned to more than a single offensive player, which can happen with a
simple closest defender approach. While a more complex methodology could
be employed to identify coverage schemes, I’d argue that the chosen
approach strikes a good balance between complexity and simplicity.

## Play Examples

The play below illustrates a “high EPA” instance (where the outcome was
very positive for the offense) in which the initial defender on the
targeted receiver involved in a pick route combination differs compared
to the initial defender. Jonathan Jones (31) starts as the assigned
defender on Dede Westbrook (12) and stays with him for about a second
into the play; but just before the throw, Devin McCourty (32), who looks
to be playing either zone or loose man on Austin Seferian-Jenkins (88),
becomes assigned to Westbrook.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/highest_epa_w_diff_defender_pick_play.gif)

The above play does a good job of illustrating how the receiver-defender
pairing capability of bipartite matching can clarify the most likely
defender at a given moment when a single player happens to be the
closest defender to more than one receiver. In this case, as Dede
Westbrook (12) motions across the formation, Jonathan Jones (31) stays
assigned to him from before the snap, through the beginning of the play.
If using a simple nearest defender assignment scheme, Stephon Gilmore
(24) would have been assigned to Westbrook at the time of the snap (and
in other frames) because he was physically the closest defender to
Westbrook. One can infer how this can introduce bias in defensive skill
evaluation that is heavily dependent on individual assignments.

Next is a low EPA example where the defenders of the pick route
receivers change. It’s evident that the pass gets picked off by a
lineman (D. Lawrence) very shortly after the snap, highlighting one of
the dangers of quick pick plays for the offense. One can see that Josh
Doctson’s (18) initial defender, Anthony Brown (30), becomes re-assigned
to Trey Quinn (14), as Brown drifts back.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/lowest_epa_w_diff_defender_pick_play.gif)

Next is a high EPA example in which the assigned defender on a target
pick route combination is the same at both the snap and the throw.
Without a clip of the actual play, it seems to me that the success of
this play is probably attributable to a juke performed by Cooper Kupp
(18) and/or over-committing from Chidobe Awuzie (24), not space created
as a result of his pick off the line with Robert Woods (17).

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/highest_epa_w_same_defender_pick_play.gif)

Finally, below is a low EPA example where the assigned defender does not
change. The interception doesn’t seem directly attributable to the
assigned defender—Parry Nickerson (43)—nor a consequence of the type of
close, man-to-man defense he played (although, subjectively, he does
guard Chester Rogers (80) pretty closely). Rather, the low EPA seems
mostly attributable to a bad decision made by Andrew Luck (12).

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/lowest_epa_w_same_defender_pick_play.gif)

# 3. Analysis

The task at hand is to quantify whether or not there is a significant
difference between the success of plays when exposed to one of the
following treatments:

1.  Whether the targeted receiver was in a pick route combination
    (`"Targeted Pick? Y"`) or not (`"Targeted Pick? N"`).

2.  Whether the initial defender of the targeted receiver is the same at
    the time of the throw (`"Has Same Defender? Y"`, i.e. man coverage)
    or not (`"Has Same Defender? Y"`, i.e. “non-man” coverage).

Of course, while these are both binary conditions (meaning their yes/no
states are mutually exclusive), the conditions themselves aren’t exactly
independent of one another. For example, a defense may be playing a
matchup scheme, so they may “pass off” receivers if the receivers cross
paths shortly after the snap in response to the receiver pick route
combination.

For the most part, I’ll treat these conditions as if they are
independent, because, otherwise, the analysis becomes more complex. I do
attempt to quantify interaction effects where it make sense to do so.

## Prelude

We need to be careful to consider some additional factors. Specifically,
from the offensive point of view, there are at least three things to
note about pick route combinations that I haven’t already explicitly
mentioned:

1.  The targeted receiver on a play where there is a pick route
    combination may not be one of the receivers involved in the
    intersection.
2.  Given that the receiver on a play with a pick route combination,
    does it matter whether the receiver runs the “underneath” route or
    not? (The underneath route is often the route in the combination
    that is intended to gain the advantage.)
3.  More than one pick route combination can occur on a given play.

(From the defensive point of view, the only feature that we focus is on
is whether the initial defenders for a given receiver changed after the
snap.)

Regarding (1), one could argue that we could disregard whether the
empirical targeted receiver was involved in the pick route combination
on a given play (given that there is a pick route combination) when
diagnosing the relationship between picks and a measure of play success
(e.g. EPA), saying that the pick route combination serves to draw the
attention of the defense and, indirectly, makes success more likely.
While such an argument could certainly be true in some situations, it’s
difficult to say whether such a statement is generally true. As such, in
the tables that follow, we only consider plays as pick plays if the
targeted receiver is involved in the pick route intersection. (This
means that plays ending in sacks or where the targeted receiver is
unknown for some other reason, such as plays where the QB throws the
ball away, are not categorized as pick plays.

Regarding (2), one can make a similar argument about the attention of
the defense being misdirected. That is, one might argue that it does not
matter whether the targeted receiver is the high or low receiver
(determined by relative position downfield after the route intersection)
in the pick route combination when isolating the impact of the pick
because, while the pick itself may be designed to free up the low
receiver, the defense may overcompensate and leave the high receiver
more open than he might be otherwise. In fact, this is the stance that
we take. Thus, for the following analysis, we don’t differentiate
between the high and low receiver in a pick route combination; we only
care that a targeted receiver is involved in the combination.

1.  is not problematic for our analysis simply because we focus on the
    targeted receiver.

## Frequentist Approach

I think it makes sense to first try to tackle the questions using a
Frequentist approach, to serve as a baseline with which we can compare
our other findings.

The tables that follow describe the frequency and success of targeted
pick plays—our first treatment of interest—across all teams in the 2018
season.[2] Data is broken out in two manners: (1) by play success and
(2) by our second treatment of interest—whether the target receiver was
covered by the same defender as their initial defender. Note that EPA
has a non-normal, pseudo-bimodal distribution that can be conflated by a
simple comparison of means; this is why distinguishing by pass success
can be particularly helpful. Conditioning on type of coverage is useful
as a means of trying to capture the non-independent nature of targeted
picks and coverage on the targeted defender (although by no means would
I say that is a very robust means of evaluating this relationship).

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/tab_t_test_epa_is_target_picked_unadjusted.png)

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/tab_t_test_n_is_target_picked_unadjusted.png)

(The `N` that stands by itself represents count, while the `N` that
follows a question mark `?` indicates that the condition is not true.)

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_epa_swarm.png)

Only 2 of the 9 t-tests show that there is a lack of a significant
difference in means at a threshold of 0.05.

-   The insignificant difference for `"EPA | Same Defender? Y"`
    (i.e. EPA when the targeted receiver is covered by their initial
    defender at the time of the throw is not significantly different on
    pick plays versus all other passes) implies that man-to-man coverage
    on the targeted receiver does an equally sufficient job of stopping
    pick plays as compared to all other passes, while non-man coverage
    (`"EPA | Same Defender? N"`) does a significantly worse job at
    stopping pick plays. (Note that `"EPA | Same Defender? N"` has a
    statistically significant difference in means, and the mean for the
    targeted pick plays is higher.)
-   The insignificant difference for
    `"EPA | Pass Successful? N & Same Defender? N"` (i.e. given an
    unsuccessful pass, EPA when the targeted receiver is not covered by
    their initial defender is not significantly different on pick plays
    compared to all other passes) is best interpreted by comparing it to
    the significant difference for its direct counterpart,
    `"EPA | Pass Successful? N & Same Defender? Y"`. The former implies
    that non-man coverage does not have a significant relationship with
    EPA when the pass is not successful, while the latter result implies
    that man coverage does.

Let’s go on and break down EPA and frequency in an analogous manner,
this time basing our t-tests on our second treatment of interest—the
type of coverage on the targeted receiver. (Note the change in the
column headers and `Characteristic` labels.)

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/tab_t_test_epa_has_same_defender_unadjusted.png)

And, for reference, here are the sample sizes.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/tab_t_test_n_has_same_defender_unadjusted.png)

There are more insignificant results this time, 5 of the 9 tests. To
highlight a few of the significant results:

-   There is a significant difference in EPA given that the pass is
    successful (`"EPA | Pass Successful? Y`). The higher EPA value of
    `"Same Defender? Y"` relative to `"Same Defender? N"` suggests that
    man coverage on the targeted receiver is disadvantageous for the
    defense in these cases. Anecdotally, this may imply that man
    coverage is more likely to give up big plays, which most people
    would say without ever looking at the numbers. We’ll come back to
    this idea later.

-   There is a significant difference given that the pass is successful
    and the targeted receiver is not involved in a pick route
    combination (`"EPA | Pass Successful? Y & Target Picked? N`). (This
    is like a subset of the prior result, but specific to non-pick
    plays.) Again, the implication is that man coverage is worse in
    these cases, which, from a defensive perspective, is not great since
    this is a fairly common scenario.

Aside from the t-test implications, I wanted to point out that the
sample sizes of man coverage and non-man coverage are roughly 1/3 and
2/3 of all plays. This roughly corresponds to the 33.7%/66.3% split
found in [this
notebook](https://www.kaggle.com/jdruzzi/pass-coverage-classification-80-recall),
as well as the split implied by the [week 1 team coverage
labels](https://www.kaggle.com/tombliss/additional-data-coverage-schemes-for-week-1).
This is re-asuring in some way—perhaps our simplistic individual
coverage identification is not so bad!

## Causal Analysis

When analyzing the relationship of our first exposure—whether the
targeted receiver is involved in a pick—with a non-causal approach, we
were assuming the following [directed acyclic graph
(DAG)](https://en.wikipedia.org/wiki/Directed_acyclic_graph).

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/dag_is_target_picked_t-test.png)

Likewise, the DAG when focusing on the second exposure—whether the
targeted defender at the time of the throw is the same initial defender
covering the assigned receiver—is basically the same, simply swapping
out the label of the exposure node (blue). (Thus, there’s no need to
show it.)

In the Frequentist approach, we split out plays by pass success and the
exposure that was not of primary concern, but this pretty simplistic.
With our causal approach, we’ll need to add some more complexity to
account for [confounding](https://en.wikipedia.org/wiki/Confounding),
i.e. we need to account for the fact that our exposure has a
relationship with the features for the EPA model, which can bias
inference.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/dag_is_target_picked_wo_player_tracking.png)

And, since we have tracking data, we can actually do better than that—we
can account for the un-observed effects of tracking-based features on
EPA. These serve as “controls” in our causal approach.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/dag_is_target_picked.png)

(Again, the DAG for the causal effect of our second exposure is
identical, with the exception of the label of the exposure node.)

For now, we’ll assume these exposures are independent, although we know
that that isn’t quite true. We’ll come back to this in a bit.

To account for confounding, I’ll build a propensity score model to
quantify the probability of a play involving a targeted pick play. For
features, I’ll use [the features that the `{nflfastR}` expected points
(EP) model
uses](https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/#ep-model-features),
with the exception of the `era` variable (since we only have one season
of data. [3]

-   seconds remaining in half (`half_seconds`)
-   yard line (`yardline_100`)
-   whether possession team is at home (`is_home`)
-   `roof` type (retractable, dome, outdoors)
-   down (`down`)
-   yards to go (`yards_to_go`)
-   timeouts remaining (`off_timeouts` and `def_timeouts`)

(I’m going under the assumption that the Big Data Bowl model uses
similar, if not identical features .)

Altogether, these features constitute the EPA predictors node in the
DAG.

For tracking features, I’ll include only a handful:

-   x coordinate of the target receiver (`x`), the nearest offensive
    player to the targeted receiver (`x_o`), and the nearest defender
    (`x_d`) at the time of the snap
-   `y` coordinate (along the plane of `yardline_100`) of the targeted
    receiver
-   distance from the nearest offensive and defensive players to the
    ball at the time of the snap (`dist_o` and `dist_d`).

The coordinates are standardized relative to the position of the ball at
the snap. Altogether, with `yardline_100` accounting for relative
position of the ball on the field, we encode a significant amount of
positional information. Admittedly, probably more could be done, but I
think this is sufficient for capturing information about the offensive
and defensive players that have an impact on the play.\[^4\] [4]

I’ll use a logistic regression model for the purpose of finding the
probability of a play having a targeted pick route. This model
effectively serves as a propensity score model.[5] I deal with the
correlation between some of the covariates (e.g. `down` and
`yards_to_go`) by including appropriate interaction terms.

The propensity scores are fed into a nearest neighbor matching algorithm
to find similar plays, effectively reducing the confounding noted
earlier in the DAG. The figure below illustrates the matching of
treatment and control groups relative to the whole data set. The
original data set has many more observations where the probability for a
targeted pick play are lower (hence the gray shade falling out of the
bounds of the chart).

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_prop_probs_is_target_picked.png)

Below is a love plot, showing how the matching has reduced bias among
each of the covariates. Evidently the tracking features have the
greatest variance between the un-adjusted and adjusted data sets.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_love_is_target_picked.png)

Finally, I fit a linear regression model with EPA as the response
variable using the same features as those in the propensity model, only
adding an indicator for whether the targeted receiver was picked or not
(`is_target_picked`). The big thing to note is that `is_target_picked1`
does not show up as statistically significant.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_epa_is_target_picked.png)

Thus, we might conclude that whether the targeted receiver is involved
in a pick route combination has no causal effect on EPA. This is
contrary to the deduction made before using just a Frequentist approach,
but our causal approach is certainly more robust, so I think we should
place more weighting on the result found here.

To provide a more apples-to-apples comparison with the t-tests performed
before, below is an analogous table to the one shown before, only this
time the matched data is used.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/tab_t_test_epa_is_target_picked_adjusted_is_target_picked.png)

Now we find that all t-tests indicate lack of significance. (Previously
there were 7 significant results.)

For the other exposure of interest—whether the initial defender on the
targeted receiver is the same at the time of the throw—I used the same
causal approach. Below are the same plots for propensity score matching,
standardized bias, and final regression coefficients.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_prop_probs_has_same_defender.png)

Note that the propensity score matching has a different distribution due
to the change in response variable.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_love_has_same_defender.png)

The love plot has a similar look to the same plot for target pick plays,
but the magnitude of the x-axis is notably smaller.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_epa_has_same_defender.png)

The regression coefficient plot implies that the type of coverage on the
targeted receiver does not have a causal relationship with EPA.

Lastly, the apples-to-apples t-test table shows lack of significance
across most conditions, with the exception of 2. The significant results
indicate that EPA is higher when man-to-man coverage is played on the
targeted receiver (’`Same Defender? Y"`) and the pass is successful
(`"EPA | Pass Successful? Y"`), and also when man-to-man coverage is
played on a successful passes that do not target picks
(`"EPA | Pass Successful? Y & Target Picked? N"`), even after
adjustment. Recall that the Frequentist approach also flagged these as
significant.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/tab_t_test_epa_has_same_defender_adjusted_has_same_defender.png)

Overall, I would say that there is sufficient evidence against the type
of coverage on the targeted receiver having a causal effect on EPA,
although it is interesting that there is some evidence for man-to-man
coverage on the targeted receiver leading to higher EPA when the pass is
completed. The overall lack of causal effect was perhaps foreshadowed by
some of the defender play examples. In one case, a defensive lineman
made a great individual play in intercepting the pass at the line of
scrimmage. In another case, the QB evidently made a bad decision.
Neither should be attributed to the type of coverage on the target.

## Predictive Approach

If we were to continue with the causal approach, the appropriate DAG is
as follows.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/dag_simultaneous.png)

The robustness of a causal approach that attempts to account for
multiple backdoor pathways given a final response variable that is
non-linear (EPA) is a little suspect to me, hence why I now resort to
treating this as a “traditional” machine learning problem.

I’ll create an `{xgboost}` model that mirrors the `{nflfastR}`
specification, adding in our terms for the two exposures and the
tracking control features discussed before. For model interpretability,
we can plot SHAP values.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/shap_swarm.png)

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/shap_agg.png)

These suggest that whether a play involves a target pick and the type of
coverage of the targeted defender are, for the most part, not very
important for predicting EPA. I’ll note that `has_same_defender1`
finishes middle-of-the-pack among features in terms of SHAP, and that
it’s observation-level values skew slightly above 0. This weakly agrees
with what we found in part of the Frequentist analysis and at the end of
our causal analysis—that man-to-man coverage may indeed have a
non-trivial relationship with EPA, given that the pass is successful.

Overall, I would say that the `{xgboost}` results agree with the causal
findings. In the causal case, we say that there is no causal
relationship. Here, we make a slightly weaker conclusion, simply saying
that there is no significant relationship. Nonetheless, despite the
slight difference in how we state our findings, both undeniably contrast
with the un-adjusted Frequentist findings of significant differences in
EPA due to the two treatments of interest.

# 4. Application: Quantifying Individual Defensive Skill

While the causal and predictive approaches suggest that breaking out EPA
by targeted pick play and/or type of coverage when targeted may only
lead to noisy deductions, I think it might still be useful to do so, if
only for descriptive purposes.

Below is a plot of EPA for individual defenders when they are the
assigned defender (at the time of the throw) on targeted receivers.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_defenders_intersect_adj.png)

We see some familiar names among the best in covering non-pick plays
(red), such as Stephon Gilmore, Marlon Humphrey, and Johnathan Joseph.
Each finished top 20 in 2018 among cornerbacks and safeties in terms of
pass breakups, according to [Sports Info
Solutions](https://www.sisdatahub.com/leaderboards/CB).

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/sis-cb-top20-2018.png)

Among those best at covering pick plays (blue), there seems to be more
noise. Only Derwin James seems to match empirical expectations. On the
other end of the spectrum, Robert Alford and Chidobe Awuzie pass the eye
test for defenders who were not as great in coverage in general (green).
Regarding the EPA judgment of those who did not do so well covering pick
plays (orange), Jonathan Jones certainly did not have his best year in
2018, and Josh Jackson, as a rookie in 2018, certainly could have been
the target of offenses on pick plays.

If we differentiate by whether the player was both the initial defender
(at the time of the snap) and the defender at the time of the throw
(`"Same Defender? Y"` if so, and `"Same Defender? N"` if not), we see
that there is more separation in the scatter when the defender is the
same at both instances. Intuitively, I think this make sense—I expect
there to more noise when in the EPA assignment when the initial defender
is different at the time of the throw (`"Same Defender? N"`) simply
because it is less clear who should have been responsible for covering
the targeted receiver.

![](https://github.com/tonyelhabr/bdb2021/raw/master/inst/viz_defenders_intersect_adj_by_coverage.png)

So, even if our assessment is that there is no significant relationship
(or causal relationship, in the case of our causal analysis) between
targeted pick plays and type of coverage on targets, I think these EPA
scatters at least tell us that the methodologies for identifying picks
and coverage are somewhat sound.

# 5. Conclusion

While t-tests show statistically significant magnitudes of EPA for two
treatments—(1) target pick plays and (2) coverage on the targeted
receiver—adjusting for game situation and player tracking-derived
features with a causal approach shows that these differences are mostly
not significant, except for perhaps that man-to-man coverage on the
targeted receiver is more likely to result in higher EPA (good for the
offense) given that the pass is completed. The implications of a
traditional predictive approach agree with the causal findings. Finally,
despite the generally insignificant relationships between EPA and the
two treatments, the individual descriptive results (for both offensive
and defensive players) derived from identifying pick routes and
defensive coverage seemed to agree with anecdotal knowledge.

[1] As discussed later, I refer to individual coverage of the targeted
receiver as “non-man” if the assigned defender for a given receiver at
the time of the throw is not the same as the assigned defender at the
time of the snap. Put another way, it’s basically anything that is not
distinctly man coverage, which certainly includes zone, but which may
also include “matchup” man coverage.

[2] I also experimented with using EPA and win probability added (WPA)
from the `{nflfastR}` package, but I ultimately found them to be
redundant since they lead to the same results.

[3] EPA is derived from EP; it’ s simply the difference between EP
values for plays.

[4] I think I have to stay away from using intra-play features
(e.g. location of receiver at the time of throw) since the original EPA
model features are at the play-level (unless I were to build out an
intra-play, continuous EPA model). On the other hand, the starting
position of players, as well as the other tracking features I’ve
includes, seem like fair game to me.

[5] Of course, EPA is non-linear, so my choice of model may be
questionable. My justification is that I’ll ultimately end up using
linear regression with the same features after adjusting for the
propensity scores (fitted probabilities from the propensity score
model), so it makes sense to me to use a linear framework.
