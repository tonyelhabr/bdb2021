
# what are "pick" plays?

+ double slant 7: https://www.espn.com/nfl/story/_/id/18609488/illustrating-new-england-patriots-go-plays-josh-mcdaniels-super-bowl-li-2016-nfl-playoffs
+ diggs rub route: https://www.youtube.com/watch?v=BxvUlvWOvjI
+ explanatory vid: https://www.youtube.com/watch?v=pMPbNo7Oyyg

+ show high epa pick play
+ describe methodology
  + intersections (with a backward buffer). why not just general crossing (checking for re-ordering of receivers relative to a fixed y-place (e.g. sideline)
  + how many seconds into the play (2 sec?)


# how impactful are they?

+ epa break down... broken down by how long into the play?
+ basic t-test to show statistical significant (accounting for smaller sample size)?

# how often do they happen?

+ do they occur more frequently on a certain down? 
+ a certain part of the field?
+ late in the game?
+ in a close game?
+ when do the intersections occur (in terms of seconds into a play)?

+ do certain teams run them more often?
+ which receivers are most often involved?

# how do defenses guard them?

+ are certain teams "targeted"?
+

# do certain defensive schemes work better against them?


# extensions

+ what about general crossing ("mesh") plays? or clear-out plays?
  + e.g. hi-lo crossers or stick routes: https://www.espn.com/nfl/story/_/id/18609488/illustrating-new-england-patriots-go-plays-josh-mcdaniels-super-bowl-li-2016-nfl-playoffs
+ offensive/defensive pass itnerference

## Motivation

> "Offensive coordinators call them rub concepts. Defensive coordinators call them illegal pick plays. Whatever you call them, they are hard to defend... offenses are designing these plays and training their receivers to essentially set screens to get their teammates wide open. Technically, it’s against the rule to set a pick deliberately, but referees have a hard time judging intentions and they rarely penalize these plays". - [Ted Nguyen, USA Football, 2017](https://blogs.usafootball.com/blog/4177/rub-concepts-how-to-defend-them-and-how-offenses-can-counteract-it)

To provide some motivation to the reader, below is the pick play that resulted in the highest EPA of all plays in the 2018 season.


Below is the pick play that resulted in the lowest EPA.


Just from these two extreme plays, one can get a sense for the high amount of leverage that these plays can have on game flow and, ultimately, outcome.

## Outline

1. Discuss and implement a methodology to detect pick plays from the tracking data.

2. Identify whether their really is an advantage to running the pick plays and quantifying it if so.

3. Use basic causal principles to evaluate how good certain defensive coverages (pure man-to-man vs. something else) are at stopping pick plays.

## Objectives

While the final evaluation is wholistic, the author believes that there are several actionable learning outcomes:

1) Teams could adapt their coverages in situations that are prime for pick plays (i.e. bunch formations, near goal-line plays)
2) Teams could evaluate the actions of their defensive players on a weekly basis in similar circumstances.

## 1. Detecting Pick Plays

Perhaps the simplest data-driven way to identify pick combinations is to derive it from initial offensive player alignment at the time of the snap---number of receivers on one side of the ball, order of receivers relative to a specified sideline, distance between receivers---and the routes run by the receivers. The former can be inferred from the `x` and`y` fields in the tracking data, and the latter is provided explicitly. Upon visual investigation, the author found that this method is much too sensitive to the author's definition of pick route combinations (e.g. SLANT + OUT for two receivers, SLANT + x + OUT for 3 receivers, etc.) and too naive to the fact that the same type of route (e.g. a SLANT) can be run in a myriad of styles.

A second way to identify pick plays is to again start with offensive alignment and

Finally, the methodology that was settled upon was one in which receiver paths were traced out and "intersections" between routes were identified within `n` seconds of the snap. Various values for `n` were evaluated by the author---specifically, 0.5 seconds, 1 second, 1.5 seconds, ... 3.5 seconds---before settling upon the 


## 2. Identifying an Advantage

## 3. Evaluating Defensive Coverage

The goal of this notebook is to first (1) identify whether their really is an advantage to running these pick plays and (2) whether certain defensive coverages are better at stopping them.



Perhaps the simplest way for defensive backs is to "stay with their man" in a pure man-to-man coverage.

Another common way is to play a hybrid-man coverage, where defensive backs play man-to-man on the receiver that releases in their area. The USA Football article describes this as "banjo" technique. The author believes this type of coverage to somewhat close to a zone approach, hence