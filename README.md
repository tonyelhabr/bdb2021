
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Motivation

> “Offensive coordinators call them rub concepts. Defensive coordinators
> call them illegal pick plays. Whatever you call them, they are hard to
> defend… offenses are designing these plays and training their
> receivers to essentially set screens to get their teammates wide open.
> Technically, it’s against the rule to set a pick deliberately, but
> referees have a hard time judging intentions and they rarely penalize
> these plays”. - [Ted Nguyen, USA
> Football, 2017](https://blogs.usafootball.com/blog/4177/rub-concepts-how-to-defend-them-and-how-offenses-can-counteract-it)

To provide some motivation to the reader, below is the pick play that
resulted in the highest EPA of all plays in the 2018 season.

![highest-epa-pick-play](https://github.com/tonyelhabr/bdb2021-data/blob/master/figs/is_pick_play%3DY-sec%3D1.5-pass_complete%3DN-is_lo%3DY-target_is_intersect%3DY-high_epa%3DN-2018112200-3677.png)

Below is the pick play that resulted in the lowest EPA.

![lowest-epa-pick-play](https://github.com/tonyelhabr/bdb2021-data/blob/master/figs/is_pick_play%3DY-sec%3D1.5-pass_complete%3DN-is_lo%3DY-target_is_intersect%3DY-high_epa%3DN-2018112200-3677.png)

Just from these two extreme plays, one can get a sense for the high
amount of leverage that these plays can have on game flow and,
ultimately, outcome.

## Outline

1.  Discuss and implement a methodology to detect pick plays from the
    tracking data.

2.  Identify whether their really is an advantage to running the pick
    plays and quantifying it if so.

3.  Discuss and implement a methodology for detecting defensive schemes
    on pick plays.

4.  Use basic causal principles to evaluate how good certain defensive
    schemes are at stopping pick plays.

## Objectives

While the final evaluation is wholistic, the author believes that there
are several actionable learning outcomes:

1.  Teams could adapt their coverages in situations that are prime for
    pick plays (i.e. bunch formations, near goal-line plays)
2.  Teams could evaluate the actions of their defensive players on a
    weekly basis in similar circumstances.

## 1\. Detecting Pick Plays

Perhaps the simplest data-driven way to identify pick combinations is to
derive it from initial offensive player alignment at the time of the
snap—number of receivers on one side of the ball, order of receivers
relative to a specified sideline, distance between receivers—and the
routes run by the receivers. The former can be inferred from the `x`
and`y` fields in the tracking data, and the latter is provided
explicitly. Upon visual investigation, it was found that this method is
much too sensitive to the author’s definition of pick route combinations
(e.g. SLANT + OUT for two receivers, SLANT + x + OUT for 3 receivers,
etc.) and too naive to the fact that the same type of route (e.g. a
SLANT) can be run in a myriad of styles. Below is an example.

A second way to identify pick plays is to again start with offensive
alignment and then to track the order of the receivers relative to the
sideline as the play progresses. One might say that a pick is detected
if the receivers change order relative to the sideline within some
pre-defined number of frames after the ball is snapped. While this
method was evaluated, it too often flagged route combinations that might
better be described as a “mesh” or some kind of clear-out, where one
receiver runs much more down-field than the other. Below is an example.

Finally, the methodology that was settled upon was one in which receiver
paths were traced out and “intersections” between routes were identified
within 2.5 seconds (25 frames) of the snap. (While this is by no means a
perfect method, it seems to be relatively robust to false positives,
e.g. deeper mesh concepts.) Some additional criteria was applied:

1.  Receivers out of the backfield (i.e. mostly RBs) were disregarded.
    (Very few route intersections occurred for players coming out of the
    backfield anyways.)
2.  Each receiver’s path was traced out one yard back from their
    position at the snap. The intention of this is to capture
    stacked/bunched pairs of routes.

Why within 2.5 seconds of the snap? We evaluated various half-second
values for `n`—specifically, 0.5 seconds, 1 second, 1.5 seconds, … 3.5
seconds. The figure below illustrates the frequency of receiver
intersections `n` seconds into the play.

Below is a visual example of a play where the intersection occurs more
between 2.5 and 3 seconds after the ball is snapped. The intention is to
illustrate the lack of persuasiveness in asserting that this is a pick
play. One could argue that this is just a more traditional crossing
scheme.

Below is an example of a pick play identified for a route combination in
which the receiver paths did not actually intersect, yet the play is
still classified as a pick play due to the 1 yard backwards extension.
The intention is to illustrate the usefulness of extending receiver
paths one yard back for the purpose of capturing legitimate pick
actions.

Here’s a summary of which teams ran pick route combinations most often.
The Los Angeles Rams and San Francisco 49ers being among the top of the
list here is a nice sanity check for what is empirically observed about
the offenses of Sean McVay and Kyle Shanahan.

We can run a simple linear regression with play-level descriptors to
learn a little bit more about pick plays.

Relative to all plays, pick plays seem to be run more often on third and
fourth down, as well as near the goal-line.

  - which receivers are most often involved?

## 2\. Quantifying the Benefit of Pick Plays

The table below describes the frequency and success (quantified by EPA)
of pick plays across all teams in the 2018 season. Note that a receiver
may involved in a pick route combination may not actually be targeted,
hence `is_targeted`. Also, in the case that one of the pick route
receivers is targeted, it’s useful to distinguish whether the targeted
receiver is the one running the “underneath” route, which is often the
route thought to gain the advantage as a result of the pick.

This table provides some evidence for the significance of pick plays. A
basic t test shows significance at a 0.05 level of significance. (Ew,
frequentist statistics\!)

## 3\. Identifying Defensive Coverage

Perhaps the simplest way for defensive backs to cover a pick route
combination is to “stay with their man” in a pure man-to-man coverage.

![](https://assets.usafootball.com/cms/inline-images/Nguyenpic3.jpg)

Another common way is to play a hybrid-man coverage (perhaps also called
matchup zone), where defensive backs play man-to-man on the receiver
that releases in their area. ([Ngyuen describes this as a “banjo”
scheme.](https://blogs.usafootball.com/blog/4177/rub-concepts-how-to-defend-them-and-how-offenses-can-counteract-it))

![](https://assets.usafootball.com/cms/inline-images/Nguyenpic2.jpg)

Like the hybrid approach, a zone approach would also not involve the
defenders crossing paths along with the receivers. Thus, it would be
difficult to distinguish from said hybrid coverage without labels.

For the sake of this analysis, we’ll simplify things to just: “Did the
defenders have the same receiver assignments at the time of the throw
compared to at the time of the snap?” If “yes”, we’ll say that they
played man-to-man coverage. (Maybe it wasn’t really man-to-man coverage,
but it’s certainly distinct.) If not, then we’ll just refer to it as
“not man-to-man” defense. (It could be the banjo type of coverage, or
some kind of more traditional zone coverage.)

For identifying defender “assignments”, we us bipartite min-distance
matching, employing the Hungarian method. We make assignments for all
pick plays irregardless whether the defense was actually playing a
man-to-man scheme. (We don’t have a complete set of team coverage
labels, and we assert that it is not necessary for the study at hand, in
which we only care about the coverage of two route runners.)

This assignment methodology overcomes the issue of having a single
defender assigned to more than a single offensive player, which can
result from a simple closest defender approach. While a more complex
methodology could be employed to identify coverage schemes, we argue
that it the chosen approach strikes a good balance between complexity
and simplicity. Additionally, it lends itself well to the subsequent
causal analysis where we can ask “Did the defense have greater success
against pick plays when the pick route defenders stayed with their
initial assignments?”

## 4\. Evaluating Defensive Coverage Against Pick Plays

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

A couple of other questions need to be tackled:

1.  Does it matter whether the targeted receiver on a play is one the
    receivers involved in the pick?

2.  
## Further Analysis

What about general crossing (“mesh”) plays? or clear-out plays? +
e.g. hi-lo crossers or stick routes:
<https://www.espn.com/nfl/story/_/id/18609488/illustrating-new-england-patriots-go-plays-josh-mcdaniels-super-bowl-li-2016-nfl-playoffs>
How does offensive and defensive pass interference come into play?
