
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

## Abstract

When using a causal approach, I find no evidence for targeted pick route
combinations having a significant effect on play success, as measured by
expected points added (EPA). This contrasts with the result found from
comparing the difference in EPA means with
[t-tests](https://en.wikipedia.org/wiki/Student%27s_t-test), i.e. a
[Frequentist
approach](https://en.wikipedia.org/wiki/Frequentist_inference)).
Likewise, I also find that whether the defense uses a man-to-man
coverage on the targeted receiver or any other coverage\[1\] does not
have a causal effect on the play’s success. Finally, despite the lack of
causal effects of pick plays and individual coverage on the targeted
receiver, I identify defenders who have relatively good success in
defending targeted pick plays.

## Motivation

> “Offensive coordinators call them rub concepts. Defensive coordinators
> call them illegal pick plays. Whatever you call them, they are hard to
> defend… offenses are designing these plays and training their
> receivers to essentially set screens to get their teammates wide open.
> Technically, it’s against the rule to set a pick deliberately, but
> referees have a hard time judging intentions and they rarely penalize
> these plays”. - [Ted Nguyen, USA
> Football, 2017](https://blogs.usafootball.com/blog/4177/rub-concepts-how-to-defend-them-and-how-offenses-can-counteract-it)

The table below provides some evidence for the success of pick plays
relative to all other passing plays. A student’s t-test indicates that
there is a significant difference in EPA averages when subsetting pass
result based on whether it was successful (completed) or not (incomplete
or intercepted). \[2\] The averages seem to indicate that EPAs are
larger in magnitude for pick plays. The t-test does account for sample
counts, so attributing the differences to noise is probably not
justified.

![tab\_epa\_unadjusted\_nosubtitle](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/tab_epa_unadjusted_nosubtitle.png)

For context, the table below delineates the frequencies of the pass
outcomes by whether the target receiver was involved in a pick route
combination.

![tab\_n\_unadjusted\_nosubtitle](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/tab_n_nonadjusted_nosubtitle.png)

To provide an illustration of the impact pick plays can have, below is
the pick play that resulted in the highest expected points added (EPA)
in the 2018 season.

![highest\_epa\_pick\_play](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/highest_epa_pick_play.png)

Likewise, below is the pick play that resulted in the lowest EPA (good
for the defense).

![lowest\_epa\_pick\_play](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/lowest_epa_pick_play.png)

Just from these two extreme plays, one can get a sense for the high
amount of leverage that these plays can have on game flow and,
ultimately, outcome. In the best case (for the offense), pick plays can
be a great way for the offense to quickly get a receiver a good amount
of separation, setting him up for lots of yards after the catch (even on
quick plays). In the worst case, defenders can “jump” such routes,
leading to an interception and a relatively clear path towards the
opposing end zone.

## Outline

1.  Discuss and implement a methodology to detect pick plays from the
    tracking data.

2.  Discuss and implement a methodology for detecting defensive schemes
    on pick plays.

3.  Perform analyses of the causal effects of targeted picks plays and
    individual coverage of targeted receivers.

4.  Quantify individual defender success in covering pick routes.

## Actionable Outcomes

While the final evaluation is somewhat wholistic, I believe that there
are several actionable learning outcomes:

1.  Teams could use the methodology for identifying pick route
    combinations for the purpose of offensive play classification
    (i.e. “Did the offense run a pick route?”), likely coming in the
    form of tagging of video analysis. Perhaps teams already do this or
    have a third-party do this for them, but if not, it could very
    useful for providing clear, reproducible classification of pick
    plays, not subjected to the biases of human video reviewers.

2.  Teams could use this analysis to supplement their evaluation of
    defensive schemes when covering pick route combinations, perhaps on
    a weekly basis (preparing for the next week’s game). Likewise, they
    may choose to run more or less pick route combinations on offense
    depending on their evaluation of their opponent’s ability to cover
    them. In particular, teams could evaluate the skill of specific
    defenders in covering such plays. I mostly rely on Expected Points
    Added (EPA), which, admittedly, is probably not granular enough to
    tease out individual impact in a robust, reliable manner;
    nonetheless, the measure of skill/success—whether it be something
    like reduction of separation distance, change in target probability,
    or something else along those line—could be swapped out relatively
    easily. Individual evaluation is, of course, important for
    identifying weaknesses to target in practice sessions with existing
    team players, but it could also be a point of interest in
    recruitment of players (i.e. to identify free agents who tend to
    cover pick plays particularly well).

I’ll caveat the prospect of doing skill evaluation by noting that we do
not know for certain whether individual defenders were subjected to
“non-optimal” defensive play calls in certain situation since we do
not know the team defensive coverages for all plays (and also how teams
have coached defenders to adapt dynamically in response to certain
offensive actions), so this analysis is limited in that sense.

# 1\. Identifying Pick Plays

## Methodology

Perhaps the simplest data-driven way to identify pick combinations is to
derive it from (1) initial offensive player alignment at the time of the
snap—number of receivers on one side of the ball, order of receivers
relative to a specified sideline, distance between receivers—and (2) the
routes run by the receivers. The former can be inferred from the `x`
and`y` fields in the tracking data, and the latter is provided
explicitly. Upon visual investigation, I found that this method is much
too sensitive to some strict definition of pick route combinations
(e.g. SLANT + OUT for two receivers, SLANT + x + OUT for 3 receivers,
etc.) and too naive to the fact that the same type of route (e.g. a
SLANT) can be run in a myriad of styles. Below is an example.

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
concepts.) Some additional criteria was applied:

1.  Receivers out of the backfield (i.e. mostly RBs) were disregarded.
    (Very few route intersections occurred for players coming out of the
    backfield anyways.)
2.  Each receiver’s path was traced out 1 yard back from their position
    at the snap. The intention of this is to capture stacked/bunched
    pairs of routes.

Why within 2 seconds of the snap? I began by counting how frequent
intersections occurred up through `n` seconds after the
snap—specifically, 0.5 seconds, 1 second, 1.5 seconds, …, 3 seconds.

![intersections\_after\_n\_sec](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/viz_intersections_after_n_sec.png)

This was certainly helpful to get some numbers in mind, but things
ultimately came down to me visually inspecting plays at each threshhold
and identifying 2 seconds as the maximum that I felt comfortable with.

## Play Examples

Below is an example of a play where the intersection occurs between 2.5
and 3 seconds after the ball is snapped. Note how Cooper Kupp (18) and
Robert Woods (17) line up on different sides of the offense line and
cross paths.

![pick-play-3.0-sec](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/is_pick_play%3DY-sec%3D3.0-pass_complete%3DY-is_lo%3DY-target_is_intersect%3DY-high_epa%3DY-2018092700-1148.png)

The intention of this example is to illustrate indirectly why the choice
of 2 seconds after the snap is used as the maximum time after which a
pair of crossing routes is not considered a pick route combination. An
experienced video analyst would probably say that this is a more
traditional crossing scheme.

Likewise, the example below illustrates a route intersection—between the
route of Jarvis Landry (86) and that of Rashard Higgins (84)—that occurs
between 2 and 2.5 seconds after the snap. Subjectively, this seems
certainly much closer to being a true pick play—and a video analyst may
agree; nonetheless, the methodology does not classify it as so (since it
occurs more than 2 seconds after the snap). On the other hand, the
crossing of David Njoku’s (85) path with that of Jarvis Landry’s path
occurs within 1 second of the snap, so their pair of routes does count
as a pick route combination.

![pick-play-2.5-sec](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/is_pick_play=Y-sec=2.5-pass_complete=N-is_lo=Y-target_is_intersect=Y-high_epa=N-2018090901-3718.png)

Below is an example in which the receiver paths did not actually
intersect, yet the play is still classified as a pick play due to the 1
yard backwards extension. The intention is to illustrate the usefulness
of extending receiver paths one yard back for the purpose of capturing
legitimate pick actions. Note how Zach Ertz (86) crosses just under
Nelson Agholor’s (13) starting position, ultimately receiving a pass
thrown to him less than 1.5 seconds after the snap.

![y\_buffer\_pick\_play](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/y_buffer_pick_play.png)

## Team and Player Trends

As a sanity check on the pick route identification methodology, I
continue with some descriptive charts, illustrating larger trends.

Below is a summary of which teams ran pick route combinations most
often. The Los Angeles Rams are at top of the list, which makes sense
given what is empirically observed from Sean McVay’s offense. The
Washington Football Team’s place as second on this list is perhaps not
too surprising given Jay Gruden’s ties with McVay.

![pick\_play\_frac](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/viz_pick_play_frac.png)

Given the above chart, perhaps it’s not surprising to find a Los Angeles
Rams receiver (Woods) at the top of the list of receivers involved in
the most pick route combinations.

![picks\_by\_receiver](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/viz_picks_by_receiver.png)

Again, given the team breakdown, it’s no surprise to find 3 Rams players
(Woods, Cupp, and Reynolds) and 2 Football Team’s players (Reed and
Crowder) among the receivers with the highest pick route involvement
relative to their total number of routes run.

![frac\_by\_receiver](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/viz_frac_by_receiver.png)

Julio Jones happens to be the one targeted the most when involved in a
pick route combination. Andecdotally, this makes sense. Jones is known
as one of the top receivers in the game, having a pretty diverse route
tree. 2018 was his personal second best season in terms of yards
received and receptions.

![picks\_by\_receiver\_target](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/viz_picks_by_receiver_target.png)

# 2\. Identifying Defensive Coverage

## Methodology

Enough with the offense\! The point of this whole thing is to quantify
defensive backs/coverage after all, so let’s attempt to do that.

Perhaps the simplest way for defensive backs to cover a pick route
combination is to “stay with their man” in a pure man-to-man coverage.

![man-to-man-scheme](https://assets.usafootball.com/cms/inline-images/Nguyenpic3.jpg)

Another common way is to play a hybrid-man coverage (perhaps also called
matchup zone), where defensive backs play man-to-man on the receiver
that releases in their area. ([Nguyen describes this as a “banjo”
scheme.](https://blogs.usafootball.com/blog/4177/rub-concepts-how-to-defend-them-and-how-offenses-can-counteract-it))

![banjo-scheme](https://assets.usafootball.com/cms/inline-images/Nguyenpic2.jpg)

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

The play below illustrates the receiver-defender pairing capability of
bipartite matching, clarifying the most likely defender at a given
moment in time for a given receiver when a given defender happens to be
the closest defender to more than one receiver. In this case, defender ?
is assigned to receiver ? and defender is assigned to receiver ?. If not
for the bipartite matching, defender ? would have been assigned to both.

![bipartite-matching](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/viz_diff_defender.png)

Below is an example where the initial defenders covering a pick route
combination do not change.

![epa\_w\_same\_defender\_pick\_play](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/highest_epa_w_same_defender_pick_play.gif)

While the above is a “high EPA” play (where the outcome was very
positive for the offense), below is a low EPA example.

![lowest\_epa\_w\_same\_defender\_pick\_play](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/lowest_epa_w_same_defender_pick_play.gif)

Next is a high EPA example where the defenders assigned to the receivers
in a pick route combination differ compared to the initial defenders.
(In this case, the assignments swap.)

![highest\_epa\_w\_diff\_defender\_pick\_pla](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/highest_epa_w_diff_defender_pick_play.gif)

Finally, below is a low EPA example where the defenders of the pick
route receivers change.

![lowest\_epa\_w\_diff\_defender\_pick\_play](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/lowest_epa_w_diff_defender_pick_play.gif)

# 3\. Causal Analysis

As we go forward with quantifying the impact of pick route combinations,
we need to be careful to consider some additional factors. Specifically,
from the offensive point of view, there are are at least three things to
note about pick route combinations that haven’t already been explicitly
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
the defense being mis-directed. That is, one might argue that it does
not matter whether the targeted receiver is the high or low receiver
(determined by relative position downfield after the route intersection)
in the pick route combination when isolating the impact of the pick
because, while the pick itself may be designed to free up the low
receiver, the defense may overcompensate and leave the high receiver
more open than he might be otherwise. In fact, this is the stance that
we take. Thus, for the following analysis, we don’t differentiate
between the high and low receiver in a pick route combination; we only
care that a targeted receiver is involved in the combination.

3)  is not problematic for our anlaysis simply because we focus on the
    targeted receiver.

## Frequentist Analysis

The tables that follow describe the frequency and success of pick plays
across all teams in the 2018 season.\[3\] These are actually the same
tables shown in the Motivation section, now with a subtitle emphasizing
that no matching adjustment has been made.

![tab\_epa\_unadjusted](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/tab_epa_unadjusted.png)

For context, the table below delineates the frequencies of the pass
outcomes by whether the target receiver was involved in a pick route
combination.

![tab\_n\_unadjusted](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/tab_n_nonadjusted.png)

I conduct t-tests to quantify whether there is a statistically
significant difference between the means of paired groups of EPA.
Specifically, I examine two binary conditions.

1.  Whether the targeted receiver was in a pick route combination (`"Is
    Pick Play? Y"`) or not (`"Is Pick Play? N"`).

2.  Whether the initial defender of the targeted receiver at the time of
    the throw changes sometime after the snap (`"Has Same Initial
    Defender? N"`) compared to when the defender stays the same (`"Has
    Same Initial Defender? Y"`). Note that the former equates to
    “non-man” coverage and the latter equates to man coverage.

First, we observe that there is a statistically significant difference
for both EPA measures (but not WPA) when the initial defender of the
targeted receiver at the time of the throw changes sometime after the
snap (`"Has Same Initial Defender = N"`) compared to when the defender
stays the same (`"Has Same Initial Defender = Y"`).

![t-test-same-init-defender](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/new_t_test_same_init_defender.png)

This breakdown implies that man coverage (`"Has Same Initial Defender =
Y"`) on the target defender is not as good as non-man coverage (`"Has
Same Initial Defender = N"`). Of course, this is not taking other
contextual factors into consideration, and the man-to-man identification
methodology (i.e. whether the initial defender changes or not) may be
flawed.

Other than this, we observe that the sample sizes of man coverage and
non-man coverage are roughly 1/3 and 2/3 of all plays. This roughly
corresponds to the 33.7%/66.3% split found in [this
notebook](https://www.kaggle.com/jdruzzi/pass-coverage-classification-80-recall),
as well as the split implied by the [week 1 team coverage
labels](https://www.kaggle.com/tombliss/additional-data-coverage-schemes-for-week-1).
This is re-assuring in some way—perhaps our simplistic individual
coverage identification is not so bad\!

Anyways, the purpose of showing the prior table was to contextualize the
same numbers when only looking at plays with pick routes.

![t-test-same-init-defender-w-intersect](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/new_t_test_same_init_defender_w_intersect.png)

We can look at the numbers from a different point of view, this time
comparing based on whether the play was a pick play or not.

![t-test-intersect](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/new_t_test_intersect.png)

We can look at the numbers from a different point of view, this time
comparing based on whether the play was a pick play or not.

![t-test-intersect](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/new_t_test_intersect.png)

When we filter down the numbers just to the plays where man coverage was
played on the targeted receiver, there is no evidence of a difference
between pick plays and other plays.

![t-test-intersect-w-same-defender](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/new_t_test_intersect_w_same_defender.png)

However, when we filter down to plays where non-man coverage was played
on the targeted receiver, we see relatively strong evidence of a
difference between pick plays and other plays.

![t-test-intersect-w-diff-defender](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/new_t_test_intersect_w_diff_defender.png)

Finally, for those who hate tables, the chart below captures the
information outlined in the tables.

![t-test](https://github.com/tonyelhabr/bdb2021-data/raw/master/figs/viz_t_test.png)

One can clearly see the gap between the EPA medians when there is
non-man coverage on pick plays (upper-left and mid-left facets), which
our frequentist analysis (based on the Sutdent’s t-test) has indicated
is statistically significant.

## Causal Analysis

The simplest approach to evaluating whether the man-to-man coverage
worked best on pick plays is to create a model with EPA as the response
variable and a binary indicator of man-to-man coverage (1) or not (0) as
the sole predictor. Of course, such an approach is prone to bias. In
particular, it’s prone to confounding.

We might find from this model that EPA tends to be lower with man-to-man
coverage, but there could be something about game context that is not
being captured. For example, it could be that the defense plays
man-to-man coverage on pick plays more often in the red zone, backed up
against their own end zone. The offense is more likely to score in such
plays (compared to being on their own half of the field, perhaps backed
up closer to their own end zone) and EPA is likely to be higher (since
the drive outcome is more likely to lead to points). Pick plays might
also be more common on third and fourth downs, which are high leverage
plays that play a role in the magnitude of EPA (since they can
immediately lead to a change of possession). Yards to the goal-line and
down, the factors hinted at these two examples, are independent
variables in the EPA model used by the [`{nflfastR}` package]() and are
very likely included in the EPA model used by the NFL to produce the
values provided in the given data.

## Conclusion

While t-tests show statistically significant magnitudes of EPA—more
negative when the pass is not successful and more positive when the pass
is successful—adjusting for game situation and player tracking-derived
features with a nearest neighbor matching method shows that these
differences are not caused by the pick play action.

1.  As discussed later, I refer to coverage as “non-man” if the assigned
    defender for a given receiver at the time of the throw is not the
    same as the assigned defender at the time of the snap. Put another
    way, it’s basically anything that is not distinctly man coverage,
    which certainly includes zone, but which may also include “matchup”
    man coverage.

2.  A completed pass does not always have a positive EPA, nor does an
    incomplete pass always have a negative EPA.

3.  I also experimented with using EPA and win probability added (WPA)
    from the `{nflfastR}` package, but I ultimately found them to be
    redundant since they lead to the same results.
