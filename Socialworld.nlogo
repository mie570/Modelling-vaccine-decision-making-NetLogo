extensions [table nw]

globals [
  total-links
  avg-clustering-coefficient
  avg-path-length
  avg-degree
  diameter
  density

  num-reliers
  num-searchers
  num-vaccinated
  activated-nodes
  new-good-activated-nodes
  new-bad-activated-nodes

  good-info-belief
  bad-info-belief

  willingness
  num-searcher-vac
  num-relier-vac
  ]

turtles-own
[
  decision-maker-type
  belief
  vaccinated?
  resistance-to-update-belief
  confidence
  resistance-to-social-pressure
  info-type-spread
]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to setup
  clear-all
  set-default-shape turtles "circle"
  network-generation total-population initial-network-size predefined-degree adjustable-param
  plot-clustering
  update-network-globals
  set activated-nodes no-turtles
  set new-good-activated-nodes no-turtles
  set new-bad-activated-nodes no-turtles
  set good-info-belief 1.0
  set bad-info-belief 0.1
  set willingness 0
  set num-searcher-vac 0
  set num-relier-vac 0
  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Reset Procedures while keeping the network structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to reset
  ask turtles [
    set vaccinated? false
    set belief random-float 1
    assign-color
    assign-decision-maker-type
    assign-resistance-values
  ]

  set num-reliers count turtles with [decision-maker-type = "relier"]
  set num-searchers count turtles with [decision-maker-type = "searcher"]

  set num-vaccinated 0
  set activated-nodes no-turtles
  set new-good-activated-nodes no-turtles
  set new-bad-activated-nodes no-turtles
  set willingness 0
  set num-searcher-vac 0
  set num-relier-vac 0

  set-current-plot "Vaccination Coverage"
  clear-plot

  set-current-plot "Willingness to Vaccinate"
  clear-plot

  set-current-plot "Number of Active Nodes Spreading Good/Bad Information"
  clear-plot

  set-current-plot "Number of Searcher/Relier Willing to Vaccinate"
  clear-plot

  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Generate Network
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to network-generation[N n0 g c]
  create-turtles n0
  [
    initialise-turtle
  ]
  ask turtles [
    create-links-with other turtles [set color grey]
  ]
  display

  repeat (N - n0) [
    create-turtles 1 [
      initialise-turtle
      let new-turtle turtle who
      let others other turtles with [not link-neighbor? new-turtle]
      let sorted-others sort-by [ [turtle0 turtle1] -> [distance turtle0] of new-turtle < [distance turtle1] of new-turtle] others

      create-links-with (get-choice sorted-others new-turtle g c) [set color grey]
      display
    ]
  ]
end

to-report get-choice[sorted-others new-turtle g c]
  let selected no-turtles
  foreach sorted-others [ [t] ->
    ifelse count selected = g [report selected]
        [ let p calculate-link-probability t new-turtle c
          if (p > random-float 1)
             [set selected (turtle-set selected t)]
      ]
  ]
  report selected

end

to-report calculate-link-probability [current new-turtle c]
  let k count [my-links] of current + count [my-links] of new-turtle
  let d [distance current] of new-turtle
  report exp (- d * k / c)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Setup individual attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to initialise-turtle
   setxy random-xcor random-ycor
   set size 5
   set vaccinated? false
   set belief random-float 1
   assign-color
   assign-decision-maker-type
   assign-resistance-values
end

to assign-resistance-values
  ifelse decision-maker-type = "relier"
     [ set confidence relier-confidence
       set resistance-to-update-belief relier-resistance-UpdateBelief
       set resistance-to-social-pressure relier-resistance-SocialPressure]
     [ set confidence searcher-confidence
       set resistance-to-update-belief searcher-resistance-UpdateBelief
       set resistance-to-social-pressure searcher-resistance-SocialPressure]
end

to assign-decision-maker-type

  set decision-maker-type one-of ["relier" "searcher"]
  if random-#searcher? = false
       [ if (count turtles with [decision-maker-type = "searcher"]) > (%-searcher / 100 * total-population)
            [ set decision-maker-type "relier"]

         if (count turtles with [decision-maker-type = "relier"]) > ( (1 - %-searcher / 100) * total-population)
            [ set decision-maker-type "searcher"]
       ]
end

to assign-color
  ifelse vaccinated? = true
    [set color green]
    [set color yellow]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Update the network variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to update-network-globals
  set total-links count links
  set num-reliers count turtles with [decision-maker-type = "relier"]
  set num-searchers count turtles with [decision-maker-type = "searcher"]
  set avg-clustering-coefficient precision (mean [nw:clustering-coefficient] of turtles) 3
  set avg-path-length precision (nw:mean-path-length) 3
  set avg-degree calculate-avg-degree
  set diameter calculate-diameter
  set density calcualte-density
end

to-report calculate-avg-Degree
  report precision (2 * count links / count turtles) 0
end

to-report calculate-diameter
  report max [ max [ nw:distance-to myself ] of other turtles ] of turtles
end

to-report calcualte-density
  report precision ((count links)/(total-population * (total-population - 1)/ 2)) 5
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Go Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  if ticks = 0
      [ decision-making
        set num-vaccinated count turtles with [vaccinated? = true]
        show type-of-info
        show "Begin"
        show "vaccination-coverage:"
        print count turtles with [vaccinated? = true] / count turtles

        show "willineness-to-vaccinate:"
        print willingness / count turtles

        plot-vaccination-coverage
        plot-willingness-to-Vaccinate
      ]

  ifelse count activated-nodes = 0
     [ set-seeds
       if type-of-info = "good" or type-of-info = "both"
          [update-belief new-good-activated-nodes good-info-belief]
       if type-of-info = "bad" or type-of-info = "both"
          [update-belief new-bad-activated-nodes bad-info-belief]
     ]

     [ if type-of-info = "good"
          [spread-good-info]

       if type-of-info = "bad"
          [spread-bad-info]

       if type-of-info = "both" and prioritise-good-info? = true
          [spread-good-info
           spread-bad-info]

       if type-of-info = "both" and prioritise-good-info? = false
          [spread-both-info]
      ]

   ifelse type-of-info = "both" and count new-good-activated-nodes = 0 and count new-bad-activated-nodes = 0
      [set activated-nodes no-turtles]

      [if type-of-info = "good" and count new-good-activated-nodes = 0
          [set activated-nodes no-turtles]

       if type-of-info = "bad" and count new-bad-activated-nodes = 0
          [set activated-nodes no-turtles]
      ]

   if ticks mod 7 = 0 and ticks != 0
      [ decision-making
        set num-vaccinated count turtles with [vaccinated? = true]
        plot-vaccination-coverage
        plot-willingness-to-Vaccinate

       ]

   if run-indefinitely? = false and ticks = 365
      [
        decision-making
        show "Final"
        show "vaccination-coverage:"
        print count turtles with [vaccinated? = true] / count turtles

        show "willineness-to-Vaccinate:"
        print willingness / count turtles

        ;export-plot "Vaccination Coverage" "/....csv"
        ;export-plot "Willingness to Vaccinate" "/....csv"
        ;export-plot "Number of Active Nodes Spreading Good/Bad Information" "/....csv"

        stop
      ]

  tick

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Decision making process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to decision-making
  set willingness 0
  set num-relier-vac 0
  set num-searcher-vac 0

  foreach [self] of turtles [ t ->

    ifelse [decision-maker-type] of t = "searcher" and [belief] of t > 0.75
       [ ask t [ set vaccinated? true
                 set color green ]
         set willingness willingness + 1
         set num-searcher-vac num-searcher-vac + 1
       ]

       [ let w-vac 0
         let w-non 0
         let vac [link-neighbors with [(decision-maker-type = "relier" and belief > 0.5) or (decision-maker-type = "searcher" and belief > 0.75)]] of t
         let non [link-neighbors with [(decision-maker-type = "relier" and belief <= 0.5) or (decision-maker-type = "searcher" and belief <= 0.75)]] of t
         let c-vac count vac
         let c-non count non

         ifelse ([decision-maker-type] of t = "relier" and [belief] of t > 0.5) or ([decision-maker-type] of t = "searcher" and [belief] of t > 0.75)
            [ set c-vac c-vac + 1 ]
            [ set c-non c-non + 1 ]

         foreach [self] of vac [ n ->
           set w-vac w-vac + ([confidence] of n / [distance n] of t ^ 2) / c-vac
         ]

         foreach [self] of non [ n ->
           set w-non w-non + ([confidence] of n / [distance n] of t ^ 2) / c-non
         ]

         let impact-vac (sqrt c-vac) * w-vac
         let impact-non (sqrt c-non) * w-non

         ifelse w-non = 0
           [ if [decision-maker-type] of t = "relier"
                [ ask t [ set vaccinated? true
                          set color green ]
                  set willingness willingness + 1
                  set num-relier-vac num-relier-vac + 1
                ]
            ]

            [ if (impact-vac / impact-non) > (1 + [resistance-to-social-pressure] of t)
                 [ ask t [ set vaccinated? true
                           set color green ]
                   set willingness willingness + 1
                   ifelse [decision-maker-type] of t = "relier"
                      [ set num-relier-vac num-relier-vac + 1]
                      [ set num-searcher-vac num-searcher-vac + 1]
                  ]
            ]
       ]
    display
  ]

end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Randomly select active nodes at the initial campaign
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to set-seeds
   if type-of-info = "good" or type-of-info = "both"
      [ set new-good-activated-nodes n-of #-good-seeds turtles
        ask new-good-activated-nodes [set info-type-spread "good"]
;       ask new-good-activated-nodes [set color blue]
        set activated-nodes new-good-activated-nodes
      ]

    if type-of-info = "bad" or type-of-info = "both"
       [ set new-bad-activated-nodes n-of #-bad-seeds turtles with [not member? self activated-nodes]
         ask new-bad-activated-nodes [set info-type-spread "bad"]
;        ask new-bad-activated-nodes  [set color red]
         set activated-nodes (turtle-set activated-nodes new-bad-activated-nodes)
       ]
    display
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Activate new nodes to spread information and update the influenced's beliefs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to spread-good-info
   let good-activated no-turtles
   ask new-good-activated-nodes
       [set good-activated (turtle-set good-activated (spread-information self good-info-belief))]
   set new-good-activated-nodes good-activated

end

to spread-bad-info
   let bad-activated no-turtles
   ask new-bad-activated-nodes
       [set bad-activated (turtle-set bad-activated (spread-information self bad-info-belief))]
   set new-bad-activated-nodes bad-activated

end

to spread-both-info
  let good-activated no-turtles
  let bad-activated no-turtles

  let new-both-activated-nodes turtle-set (shuffle sentence [self] of new-good-activated-nodes [self] of new-bad-activated-nodes)
  ask new-both-activated-nodes [
      ifelse [info-type-spread] of self = "good"
         [set good-activated (turtle-set good-activated (spread-information self good-info-belief))]
         [set bad-activated (turtle-set bad-activated (spread-information self bad-info-belief))]
      ]

  set new-good-activated-nodes good-activated
  set new-bad-activated-nodes bad-activated

end

to-report spread-information [seed info-belief]
  let activated no-turtles
  foreach [self] of link-neighbors [ n ->
      if ((random-float 1 < calculate-spread-probability seed n 50) and not member? n activated-nodes)
         [set activated (turtle-set activated n)
          set activated-nodes (turtle-set activated-nodes n)
          ask n [update-belief n info-belief
                 set info-type-spread [info-type-spread] of seed
                ]
         ]
    ]

  report activated
end

to-report calculate-spread-probability [current new-turtle c]
  let k count ([link-neighbors] of new-turtle) with [member? self activated-nodes]
  let d [distance current] of new-turtle
  report exp (- d * k / c)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Update individual's belief about vaccines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:::
to update-belief [influenced info-belief]
  ask influenced
      [set belief belief * resistance-to-update-belief + (1 - resistance-to-update-belief) * info-belief]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;:::
to plot-clustering
  set-current-plot "Clustering Distribution"
  clear-plot
  let table table:counts [precision nw:clustering-coefficient 2 ] of turtles
  foreach table:to-list table [ t ->
     plotxy item 0 t item 1 t
  ]
  display
end

to plot-vaccination-coverage
  set-current-plot "Vaccination Coverage"
  plotxy ticks count turtles with [vaccinated? = true] / count turtles
  display
end

to plot-willingness-to-Vaccinate
  set-current-plot "Willingness to Vaccinate"
  plotxy ticks willingness / count turtles
  display
end
@#$#@#$#@
GRAPHICS-WINDOW
1147
10
1548
420
-1
-1
0.65
1
12
1
1
1
0
0
0
1
-200
200
-200
200
0
0
1
ticks
30.0

BUTTON
1022
334
1085
367
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
874
334
940
367
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
648
210
828
243
%-searcher
%-searcher
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
648
134
820
167
total-population
total-population
0
2000
1210.0
1
1
NIL
HORIZONTAL

SLIDER
873
225
1068
258
#-good-seeds
#-good-seeds
0
100
10.0
1
1
NIL
HORIZONTAL

PLOT
1148
430
1460
667
Degree Distribution
Degree
num-of-nodes
0.0
1.0
0.0
1.0
true
false
"" "set-plot-x-range 0 (max [count my-links] of turtles)"
PENS
"default" 1.0 2 -16777216 true "" "histogram [count my-links ] of turtles"

MONITOR
646
10
821
55
avg-clustering-coefficient
avg-clustering-coefficient
17
1
11

PLOT
1473
430
1778
667
Clustering Distribution
node-clustering-coefficient
num-of-nodes
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 2 -955883 true "" ""

MONITOR
646
58
762
103
avg-path-length
avg-path-length
17
1
11

MONITOR
829
10
896
55
NIL
diameter
17
1
11

MONITOR
902
10
969
55
NIL
density
17
1
11

MONITOR
770
58
855
103
NIL
avg-degree
17
1
11

MONITOR
862
58
975
103
NIL
num-vaccinated
17
1
11

MONITOR
974
10
1042
55
NIL
total-links
0
1
11

SLIDER
873
262
1067
295
#-bad-seeds
#-bad-seeds
0
100
10.0
1
1
NIL
HORIZONTAL

MONITOR
1047
10
1134
55
NIL
num-reliers
17
1
11

MONITOR
1026
58
1135
103
NIL
num-searchers
17
1
11

SWITCH
874
186
1063
219
prioritise-good-info?
prioritise-good-info?
1
1
-1000

CHOOSER
874
134
1012
179
type-of-info
type-of-info
"good" "bad" "both"
2

SWITCH
648
172
867
205
random-#searcher?
random-#searcher?
1
1
-1000

TEXTBOX
648
114
871
144
Network creation parameter settings:
12
0.0
1

SLIDER
650
328
822
361
adjustable-param
adjustable-param
1
2000
200.0
1
1
NIL
HORIZONTAL

SLIDER
650
289
822
322
predefined-degree
predefined-degree
1
20
5.0
1
1
NIL
HORIZONTAL

SLIDER
649
250
823
283
initial-network-size
initial-network-size
1
20
5.0
1
1
NIL
HORIZONTAL

BUTTON
949
334
1012
367
NIL
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
651
380
818
408
Resistance to social pressure:\n
11
0.0
1

SLIDER
650
403
872
436
relier-resistance-SocialPressure
relier-resistance-SocialPressure
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
651
440
880
473
searcher-resistance-SocialPressure
searcher-resistance-SocialPressure
0
1
0.5
0.1
1
NIL
HORIZONTAL

TEXTBOX
891
379
1125
407
Resistance to update belief:
11
0.0
1

SLIDER
890
403
1123
436
relier-resistance-UpdateBelief
relier-resistance-UpdateBelief
0
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
890
441
1124
474
searcher-resistance-UpdateBelief
searcher-resistance-UpdateBelief
0
1
0.9
0.05
1
NIL
HORIZONTAL

TEXTBOX
653
488
850
516
Individual belief confidence:
11
0.0
1

SLIDER
652
509
824
542
relier-confidence
relier-confidence
0
100
50.0
10
1
NIL
HORIZONTAL

SLIDER
652
546
834
579
searcher-confidence
searcher-confidence
0
100
75.0
10
1
NIL
HORIZONTAL

PLOT
5
188
638
410
Number of Active Nodes Spreading Good/Bad Information
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Pro-vaccine" 1.0 0 -13345367 true "" "plot count new-good-activated-nodes"
"Anti-vaccine" 1.0 0 -2674135 true "" "plot count new-bad-activated-nodes"

PLOT
6
417
640
606
Number of Searcher/Relier Willing to Vaccinate
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Reliers-willing" 1.0 0 -13345367 true "" "plot num-relier-vac"
"Searchers-willing" 1.0 0 -2674135 true "" "plot num-searcher-vac"
"Reliers-vaccinated" 1.0 0 -13840069 true "" "plot count turtles with [decision-maker-type = \"relier\" and vaccinated? = true]"
"Searchers-vaccinated" 1.0 0 -2064490 true "" "plot count turtles with [decision-maker-type = \"searcher\" and vaccinated? = true]"

PLOT
320
11
636
183
Willingness to Vaccinate
NIL
NIL
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

PLOT
6
10
314
183
Vaccination Coverage
NIL
NIL
0.0
0.0
0.0
0.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SWITCH
874
298
1033
331
run-indefinitely?
run-indefinitely?
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
