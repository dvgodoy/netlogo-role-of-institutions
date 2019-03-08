extensions [array matrix r]

__includes["geo_functions.nls" "social_functions.nls" "r_functions.nls" "resource_functions.nls"]


globals [
  all-links
  attrib-max
  char-bits
  average-path-length
  transitivity
  num-generations
  total-generations
]

patches-own [
  blevel
  bmax
]

turtles-own [
  bbits
  vbits
  boldness
  vengefulness
  payoff
  defector
  punisher
  meta-defector
  meta-punisher
  punished-defections
  punished-meta-defections
  was-punished-defecting
  was-punished-meta-defecting 
]

undirected-link-breed [geo-links geo-link]
undirected-link-breed [social-links social-link]

social-links-own [
  rewired?
]

to setup  
  clear-all
  set char-bits 3
  set attrib-max 7
  r-load
  r-init-vars
  ;random-seed -339421936
  resize-world 0 torus-size - 1 0 torus-size - 1
  set-patch-size floor (400 / torus-size)
  ;set attrib-max (2 ^ char-bits - 1)
  make-patches
  make-turtles
  set all-links (link-set geo-links social-links)
  reset-ticks
  set num-generations 0
  set total-generations 10 ^ generations-log10
end

to go
 if (num-generations < total-generations) [
    run-generation
    ; Te
    imitation-evolving
    ; Ta
    if (num-generations mod social-strategy-ratio = 0) [      
      ; social evolving degenerates into eliminating all links
      ;social-evolving
      if (endogenous-institution) [
        r:put "endog.b" median [boldness] of turtles
        r:put "endog.v" median [vengefulness] of turtles
        r:eval "inst.attr <- list(boldness = endog.b, vengefulness = endog.v)"
      ]
    ]
    set num-generations num-generations + 1
    r-init-vars
    tick
  ]
end

to run-generation
  ask turtles [
    set payoff 0
  ]
  let num-ticks 0
  while [num-ticks < ticks-per-generation] [
    reset-actions
    define-roles
    make-actions
    r-put-coord
    make-observations
    calculate-payoffs
    if (walk-away-rule) [
      walk-away
    ]
    regrow-patches
    set num-ticks num-ticks + 1
  ]
end

to walk-away
  foreach ([who] of turtles with [not defector]) [
    walk-to neighborhood-type neighborhood-radius (turtle ?) (turtles with [defector]) 0.05
  ]
end

to randomize-attribs
  ask turtles [
    set bbits array:from-list n-values char-bits [random 2]
    set vbits array:from-list n-values char-bits [random 2]
    set boldness calc-attrib bbits
    set vengefulness calc-attrib vbits
  ]
end

to make-turtles
  create-ordered-turtles torus-size * torus-size * population-density / 100
  r:put "pop.size" count turtles
  ask turtles [
    set shape "person"
    set size 0.25
    set color yellow
    set defector false
    set punisher false
    set meta-defector false
    set meta-punisher false
    ifelse (population-density = 100) [
      move-to patch who floor (who / torus-size)
    ][
      move-to one-of patches with [not any? turtles-here]
    ]
  ]
  ifelse (random-attribs) [
    randomize-attribs
  ] [
    ask turtles [
      set boldness initial-boldness
      set vengefulness initial-vengefulness
    ]
  ]  
  r-init-attrs
  r-put-coord
  r-make-ba-social ba-power
  links-from-matrix r:get "soc.mat"
end

to-report code-attrib [bits]
  let n 0
  let attrib 0
  let len array:length bits
  let code ""
  while [n < len] [
    set code word code array:item bits n
    set n n + 1
  ]
  report code
end

to-report calc-attrib [bits]
  let n 0
  let attrib 0
  let len array:length bits
  while [n < len] [
    set attrib attrib + array:item bits n * (2 ^ n)
    set n n + 1
  ]
  report attrib
end


to reset-actions
  ask turtles [
    set punished-defections 0
    set punished-meta-defections 0
    set was-punished-defecting 0
    set was-punished-meta-defecting 0  
  ]  
end

to define-roles
  ask turtles [
    set defector false
    set punisher false
    set meta-defector false
    set meta-punisher false
  ]
  r-define-roles
  r-get-roles
  ask turtles with [defector] [ set color red ]
  ask turtles with [not defector and not punisher] [ set color blue ]
  ask turtles with [punisher] [ set color yellow ]
end

to-report define-observers [observed]
  let observers []
  let patch-neighbors patch-here
  let geo-neighbors []
    
  ifelse (neighborhood-type = "Complete Graph") [
    ask observed [
      set geo-neighbors other turtles
    ]
  ] [
    repeat neighborhood-radius [
      ask patch-neighbors [
        ifelse (neighborhood-type = "Moore") [
          set patch-neighbors (patch-set patch-neighbors neighbors)
        ] [
          set patch-neighbors (patch-set patch-neighbors neighbors4)
        ]
      ]
    ]  
    set geo-neighbors turtles-on patch-neighbors
  ]
  
  if (defection-observing = "Geo only") [
    ask observed [
      set observers geo-neighbors
    ]    
  ]
  if (defection-observing = "Social only") [
    ask observed [
      set observers social-link-neighbors
    ]    
  ]
  if (defection-observing = "Geo and Social") [
    ask observed [
      set observers (turtle-set geo-neighbors social-link-neighbors)
    ]    
  ]  
  report observers
end

to make-actions
  cooperate-harvest
  defect-harvest
end

to make-observations
  r-make-obs
  r-get-actions
end

to calculate-payoffs
  if (not metanorms) [
    ask turtles [
      set punished-meta-defections 0
      set was-punished-meta-defecting 0
    ]
  ]
  ask turtles [
    set payoff payoff - cost-of-effort - (punished-defections + punished-meta-defections) * enforcement-cost - (was-punished-defecting + was-punished-meta-defecting) * punishment-cost
  ]
  r-put-payoff
end

to imitation-evolving
  r-imitation
  r-get-attribs
end

to social-evolving
  r-social
  links-from-matrix r:get "soc.new"
  r:eval "soc.mat[soc.mat == -1] <- 0"
end

to cooperate-harvest
  ask turtles with [not defector] [
    ifelse (blevel > low-effort) [
      set payoff payoff + low-effort
      set blevel max (list (blevel - low-effort) 0)
    ] [
      let empty-patches neighbors with [not any? turtles-here]
      let destination empty-patches with [blevel > low-effort]
      ifelse (any? destination) [
        move-to one-of destination
      ] [ if (any? empty-patches) [
            move-to one-of empty-patches
          ]
      ]
    ]
  ]
end

to defect-harvest
  ask turtles with [defector] [
    ifelse (blevel > high-effort) [
      set payoff payoff + high-effort
      set blevel max (list (blevel - high-effort) 0)
    ] [
      set payoff payoff + blevel
      let empty-patches neighbors with [not any? turtles-here]
      let destination empty-patches with [blevel > high-effort]
      ifelse (any? destination) [
        move-to one-of destination
      ] [ if (any? empty-patches) [
            move-to one-of empty-patches
          ]
      ]
    ]
  ]
end

to-report average-boldness
  report mean [boldness] of turtles
end

to-report average-vengefulness
  report mean [vengefulness] of turtles
end

to-report average-payoff
  report mean [payoff] of turtles
end

to-report defector-agents
  report turtles with [defector]
end

to-report cooperator-agents
  report turtles with [not defector and not punisher]
end

to-report punisher-agents
  report turtles with [punisher]
end

to-report count-defectors
  report count defector-agents
end

to-report count-cooperators
  report count cooperator-agents
end

to-report count-punishers
  report count punisher-agents
end

to-report mean-degree
  report mean [count social-link-neighbors] of turtles
end

to-report mean-degree-defectors
  report mean [count social-link-neighbors] of defector-agents
end

to-report mean-degree-punishers
  report mean [count social-link-neighbors] of punisher-agents
end

to-report mean-degree-cooperators
  report mean [count social-link-neighbors] of cooperator-agents
end

to-report resource-level
  report mean [blevel] of patches
end

to-report mean-payoff
  report mean [payoff] of turtles / ticks-per-generation
end
@#$#@#$#@
GRAPHICS-WINDOW
388
330
798
761
-1
-1
40.0
1
10
1
1
1
0
1
1
1
0
9
0
9
0
0
1
ticks
30.0

BUTTON
18
553
190
609
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
16
148
188
181
torus-size
torus-size
2
15
10
1
1
NIL
HORIZONTAL

BUTTON
18
618
190
675
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

CHOOSER
15
231
189
276
neighborhood-type
neighborhood-type
"Von Neumann" "Moore" "Complete Graph"
1

SWITCH
17
439
189
472
metanorms
metanorms
0
1
-1000

CHOOSER
18
316
193
361
defection-observing
defection-observing
"Geo only" "Social only" "Geo and Social"
2

SLIDER
14
35
186
68
ticks-per-generation
ticks-per-generation
1
10
4
1
1
NIL
HORIZONTAL

SLIDER
14
75
186
108
generations-log10
generations-log10
0
10
4
1
1
NIL
HORIZONTAL

PLOT
389
16
659
167
Attributes
Rounds
NIL
0.0
10.0
0.0
7.0
true
true
"" ""
PENS
"Boldness" 1.0 0 -16777216 true "" "plot average-boldness"
"Vengefulness" 1.0 0 -2674135 true "" "plot average-vengefulness"

SWITCH
18
477
190
510
must-obs-orig-defection
must-obs-orig-defection
0
1
-1000

PLOT
389
176
661
326
Population
Rounds
%
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"Cooperators" 1.0 0 -13345367 true "" "plot 100 * count-cooperators / count turtles"
"Punishers" 1.0 0 -1184463 true "" "plot 100 * count-punishers / count turtles"
"Defectors" 1.0 0 -2674135 true "" "plot 100 * count-defectors / count turtles"

SLIDER
679
217
851
250
high-effort
high-effort
0
1
0.3
0.01
1
NIL
HORIZONTAL

SLIDER
679
282
851
315
low-effort
low-effort
0
1
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
857
178
1029
211
cost-of-effort
cost-of-effort
0
1
0.05
0.01
1
NIL
HORIZONTAL

SLIDER
861
244
1033
277
enforcement-cost
enforcement-cost
0
1
0.2
0.01
1
NIL
HORIZONTAL

SLIDER
860
283
1032
316
punishment-cost
punishment-cost
0
1
0.9
0.01
1
NIL
HORIZONTAL

SLIDER
671
53
842
86
initial-resource-level
initial-resource-level
0
100
80
1
1
%
HORIZONTAL

SLIDER
671
92
843
125
resource-growth-rate
resource-growth-rate
0
100
50
1
1
%
HORIZONTAL

SLIDER
16
189
188
222
population-density
population-density
0
100
50
1
1
%
HORIZONTAL

SLIDER
21
367
193
400
neighborhood-radius
neighborhood-radius
1
5
1
1
1
NIL
HORIZONTAL

SLIDER
203
399
375
432
institutional-boldness
institutional-boldness
0
7
0
1
1
NIL
HORIZONTAL

SLIDER
203
436
376
469
institutional-vengefulness
institutional-vengefulness
0
7
7
1
1
NIL
HORIZONTAL

SLIDER
203
502
378
535
institutional-weight
institutional-weight
0
100
5
1
1
%
HORIZONTAL

SLIDER
203
539
378
572
institutional-minimum
institutional-minimum
0
100
2
1
1
%
HORIZONTAL

SLIDER
203
247
375
280
imitation-strength
imitation-strength
0
10
3
1
1
NIL
HORIZONTAL

SLIDER
204
603
377
636
institutional-strength
institutional-strength
0
10
2
1
1
NIL
HORIZONTAL

SLIDER
203
286
375
319
wrong-imitation
wrong-imitation
0
100
10
1
1
%
HORIZONTAL

SLIDER
1282
77
1454
110
adjust-strength
adjust-strength
0
10
3
1
1
NIL
HORIZONTAL

SLIDER
1283
116
1455
149
rewire-prob
rewire-prob
0
100
0
1
1
%
HORIZONTAL

SLIDER
1284
235
1456
268
tryad-strength
tryad-strength
0
10
3
1
1
NIL
HORIZONTAL

SLIDER
1285
275
1457
308
tryad-basic-prob
tryad-basic-prob
0
100
0
1
1
%
HORIZONTAL

SLIDER
1283
156
1455
189
geo-strength
geo-strength
0
10
3
1
1
NIL
HORIZONTAL

SLIDER
1284
195
1456
228
geo-basic-prob
geo-basic-prob
0
100
0
1
1
%
HORIZONTAL

PLOT
854
10
1239
168
Resource
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"Level" 1.0 0 -14439633 true "" "plot resource-level"

PLOT
815
431
1120
571
Degree Distribution
NIL
NIL
0.0
70.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "let max-degree max [count social-link-neighbors] of turtles\nplot-pen-reset  ;; erase what we plotted before\nset-plot-x-range 1 ((max (list 1 max-degree)) + 1)  ;; + 1 to make room for the width of the last bar\nhistogram [count social-link-neighbors] of turtles"

SLIDER
206
727
378
760
social-strategy-ratio
social-strategy-ratio
1
10
4
1
1
NIL
HORIZONTAL

SLIDER
203
205
375
238
mutation-rate
mutation-rate
0
100
3
1
1
%
HORIZONTAL

SLIDER
815
387
987
420
ba-power
ba-power
-3
3
-1
1
1
NIL
HORIZONTAL

SLIDER
202
98
374
131
initial-boldness
initial-boldness
0
7
7
1
1
NIL
HORIZONTAL

SLIDER
203
135
375
168
initial-vengefulness
initial-vengefulness
0
7
0
1
1
NIL
HORIZONTAL

SWITCH
203
60
374
93
random-attribs
random-attribs
1
1
-1000

SWITCH
1281
37
1452
70
walk-away-rule
walk-away-rule
1
1
-1000

SWITCH
206
665
377
698
endogenous-institution
endogenous-institution
0
1
-1000

MONITOR
815
579
914
624
Avg. Path Length
r-average-path-length
2
1
11

MONITOR
918
578
1021
623
Clustering Coef.
r-clust-coeff
2
1
11

MONITOR
1024
578
1122
623
Avg. Degree
mean-degree
2
1
11

TEXTBOX
17
12
167
31
Simulation
16
0.0
1

TEXTBOX
20
123
170
142
Geographic
16
0.0
1

TEXTBOX
675
10
825
48
Common-Pool Resource
16
0.0
1

TEXTBOX
207
10
357
29
Agents
16
0.0
1

TEXTBOX
206
36
356
54
Initial Attributes
12
0.0
1

TEXTBOX
204
184
354
202
Imitation
12
0.0
1

TEXTBOX
682
174
832
193
Payoffs
16
0.0
1

TEXTBOX
678
259
828
277
Cooperator
12
0.0
1

TEXTBOX
679
195
829
213
Defector
12
0.0
1

TEXTBOX
860
220
1010
238
Punisher
12
0.0
1

TEXTBOX
18
288
168
307
Observation
16
0.0
1

TEXTBOX
17
411
167
429
Metanorms
16
0.0
1

TEXTBOX
206
346
356
365
Institutions
16
0.0
1

TEXTBOX
211
378
361
396
Initial Attributes
12
0.0
1

TEXTBOX
204
480
354
498
Influence
12
0.0
1

TEXTBOX
204
581
354
599
Imitation
12
0.0
1

TEXTBOX
208
644
358
662
Origin
12
0.0
1

TEXTBOX
211
707
361
725
Voting
12
0.0
1

TEXTBOX
816
337
966
356
Social
16
0.0
1

TEXTBOX
817
363
979
393
Barabási-Albert algorithm
12
0.0
1

TEXTBOX
1283
12
1433
31
UNUSED
16
15.0
1

PLOT
1040
175
1240
316
Average Payoff
NIL
NIL
0.0
10.0
-1.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [payoff] of turtles / ticks-per-generation"

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
NetLogo 5.0.5
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Exp 1 - p(80),w(5,10), m(0,1,2,5), str(0,1,2)" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <enumeratedValueSet variable="population-density">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp 2 - p(80),w(20), m(0,1,2,5), str(0,1,2)" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <enumeratedValueSet variable="population-density">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp 3 - p(20),w(20), m(0,1,2,5), str(0,1,2)" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <enumeratedValueSet variable="population-density">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp 4 - p(80),w(20), m(0), str(1)" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <enumeratedValueSet variable="population-density">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp 2 - p(80),w(20), m(0,1,2,5), str(0,1,2) - torus 7" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <enumeratedValueSet variable="population-density">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp Exog Inst only" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <metric>r-average-path-length</metric>
    <metric>mean-degree</metric>
    <enumeratedValueSet variable="population-density">
      <value value="20"/>
      <value value="50"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="5"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp Exog Inst only Compl" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <metric>r-average-path-length</metric>
    <metric>mean-degree</metric>
    <enumeratedValueSet variable="population-density">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp Endog Inst" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <metric>r-average-path-length</metric>
    <metric>mean-degree</metric>
    <enumeratedValueSet variable="population-density">
      <value value="20"/>
      <value value="50"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="5"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp Exog Inst only GeoSocial" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <metric>r-average-path-length</metric>
    <metric>mean-degree</metric>
    <enumeratedValueSet variable="population-density">
      <value value="20"/>
      <value value="50"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="5"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo and Social&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Exp Endog Inst" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <metric>r-average-path-length</metric>
    <metric>mean-degree</metric>
    <metric>r-inst-b</metric>
    <metric>r-inst-v</metric>
    <enumeratedValueSet variable="population-density">
      <value value="20"/>
      <value value="50"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="5"/>
      <value value="20"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
      <value value="&quot;Geo and Social&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment4" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>average-boldness</metric>
    <metric>average-vengefulness</metric>
    <metric>count-cooperators</metric>
    <metric>count-punishers</metric>
    <metric>count-defectors</metric>
    <metric>resource-level</metric>
    <metric>mean-payoff</metric>
    <enumeratedValueSet variable="population-density">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-weight">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-minimum">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-strength">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wrong-imitation">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-rate">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="walk-away-rule">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rewire-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tryad-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-growth-rate">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-radius">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="random-attribs">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resource-level">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="high-effort">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="low-effort">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ba-power">
      <value value="-1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="enforcement-cost">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endogenous-institution">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks-per-generation">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="imitation-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="defection-observing">
      <value value="&quot;Geo only&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metanorms">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="must-obs-orig-defection">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-basic-prob">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="adjust-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="social-strategy-ratio">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-boldness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-of-effort">
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="geo-strength">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vengefulness">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-boldness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="torus-size">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="generations-log10">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="punishment-cost">
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="institutional-vengefulness">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="neighborhood-type">
      <value value="&quot;Moore&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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

geo-link
0.2
-0.2 0 0.0 1.0
0.0 1 2.0 2.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

social-link
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
