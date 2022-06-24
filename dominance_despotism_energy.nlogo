extensions [csv matrix]
;;much of the below code has been borrowed from Marcy's testing environment for primate patch movement
globals [
  ;; Landscape parameters
  quality-max
  average-clump-size
  extraction-rate-average
  digestion-rate-average
  regrowth-rate

  ;;;;;; THESE ARE HERE FOR THE TESTING ENVIRONMENT, INSTEAD OF SLIDERS
  starting-pop-primates

  go-tests-on?
  resource-tests-on?

  ;; patchsets and other mutable landscape vars
  empty-patches
  resource-patches
  energy-left



  ;; vision

  other-primate-detection-radius
  resource-detection-radius




  countdown-to-longer-processes
  total-starved

  folder-path
  attacks-saved
  avoids-saved
]

;directed-link-breed [mothers mother] ;; link from offspring to mother
;directed-link-breed [fathers father];; link from offspring to father
;directed-link-breed [children child]  ;; link from parent to offspring

;undirected-link-breed [affiliates affiliate]
;undirected-link-breed [members member] ;;link between primagent and group

breed [primates primate]
;breed [groups group] ;;groups will actually be a kind of turtle "under the hood"
;breed [predators predator]

directed-link-breed [fightsavoided fightavoided] ;; this is a link from the one who avoided TO the opponent they decided not to fight
directed-link-breed [attacks attack] ;; this is a link from the approaching agent to the opponent that they did decide to fight
directed-link-breed [victories victory] ;;this is a link from the winner to the loser
directed-link-breed [defeats defeat] ;; this is a link from the loser to the winner

;;; as an alternative to the old way, i'm trying to do a different storage system for the interaction data
;; a contest link to be made for any interaction
undirected-link-breed [contests contest]

primates-own[
  sex
  age ;; in years
  ageclass
  stored-energy
  rhp ;; RHP is the resource holding potential - basically, how "strong" an individual is in a fight
  vision
  nearest-primates
  visible-resources ;; list of patches, sorted by penergy
  patch-picked

  wns
  lsses
  fights
  wns_r
  lsses_r

  dom-score ;; current value of estimated fighting ability, 0.0-1.0
  dom-delta-list ;; list recording every time the dom-score was changed by a win or loss
  running-dom-avg ;; average of the dom-delta-list

  daily-distance-traveled ;; distance traveled, reset every 12 ticks
  distance-traveled ;; TOTAL distance traveled by primate
  total-energy-gained ;; total energy gained over time
]

;;;now for each of the distinct link breeds I created, I am creating a counter
fightsavoided-own [avoid-counter]
attacks-own [attack-counter]
victories-own [victory-counter]
defeats-own [defeat-counter]


contests-own[ ;; i can't make it so that each interaction is its own link, because there cannot be
  ;;multiple links of the same breed between the same 2 agents
  escalated? ;; false if the actual fight was avoided; true if the actor decided to fight
  loser ;; primate who lost; either backed down if not escalated, or was beaten in fight
  timestamp ;;ticks at which the contest took place

]

patches-own [
  penergy
  quality
  digestibility
  extraction-rate
]

to setup
  ca
  file-close-all

  (ifelse resource-dist = "clumped" [
    set quality-max 24
    create_clumped_resources
  ]

  resource-dist = "uniform" [
    set quality-max 3
    create_uniform_resources
  ]
  [error "No resources!"])

  color_patches

  set resource-patches (patches with [quality != 0])
  set empty-patches (patches with [quality = 0])
  ;; now we have patch-sets of resource patches and patches without resources
  ;; note that a resource patch's penergy could be 0, but the QUALITY cannot be

  ;; make an initial population of primates
  set starting-pop-primates 10
  create_starting_pop

  vision_settings

  ;ask turtles [create-decidenottofight-to other turtles]
  ask primates [ ;; you're on the right track
   create-fightsavoided-to other turtles
    create-attacks-to other turtles
    create-victories-to other turtles
    create-defeats-to other turtles
  ]

  set regrowth-rate (quality-max / 3)

  reset-ticks

  set_folder_path
  set avoids-saved false
  set attacks-saved false
end

to vision_settings

  set other-primate-detection-radius 3
  set resource-detection-radius 4

end

to color_patches
  ask resource-patches [
    (ifelse penergy = 0 [set pcolor white]
      penergy = 3 [set pcolor 68]

      [set pcolor scale-color green penergy 0 24]
      )

    ]
end

to create_clumped_resources ;; picks a new center-point and then generates a clump around it

  set resource-patches (patch-set patch 0 0) ; patch 5 5 patch -5 5 patch -5 -5 patch 5 -5)

  ask resource-patches [
    set resource-patches (patch-set patches in-radius 5 resource-patches)
    ;set empty-patches other empty-patches
  ]

  ask resource-patches [

    set quality one-of (range 10 quality-max)
    set penergy quality
    set extraction-rate (quality / 3)
  ]
end


to create_uniform_resources

   set resource-patches patches

  ask resource-patches [

    set quality quality-max ;; all patches set to quality 3 (approximate average calculated across several runs in conflic_dev_2 model [Summed Patch Qualities excel file])
    set penergy quality

    set extraction-rate 1
  ]


end



to create_starting_pop ;; creates a beginning population of primate agents, of varialbe age and sex, and places them randomly on the landscape
  create-primates starting-pop-primates [
    set label who
    ;;set label return_rhp ;each turtle displays their return-rhp value
    set size 1 ; size of turtles to 5
    set vision 20
    set stored-energy 10
    set dom-score 0.50
    set dom-delta-list (list 0.50)
    set running-dom-avg 0.50

    ;; sex
    set sex "F"

    ;; age and ageclass
    set age 10
    set ageclass "adult"

    ;; rhp
    set rhp one-of [1 2 3 4 5 6 7 8] ;; rhp = 0 --> none
                     ;; rhp = 1 --> low
                     ;; rhp = 2 --> mid-low
                     ;; rhp = 3 --> mid-high
                     ;; rhp = 4 --> high
    ;set rhp one-of (range 1 4)

    ;; use function return_rhp to set a value for rhp as the outcome here
    if assessment-who = "opponent" [ ;;need to do this as before (when 1 simply added to top and bottom) individuals never fought as prob of fighting was always 0
      set wns 1
      set lsses 1
    ]
    setxy random-xcor random-ycor
 set distance-traveled 1
  ]
  ask primates [
    set color scale-color red rhp 1 8
  ]
end


to go ;; put main schedule procedures for readability

  tick
  if not any? primates [stop]

  ask primates [
    find_nearest-primates
    find_visible-resources

    (ifelse penergy > 0 [eat] ;; if there is energy in the current patch, eat - eventually need to allow for leaving patches even when food remaains
      [move] ) ;; if there is no food here, move

  ;ask patches in-cone resource-detection-radius visual-angle [set pcolor pcolor + 1]


  ]

  color_patches
  decay_dom



  if ticks mod 20 = 0 [
  ask patches with [penergy < quality] [;if there are any empty patches let them regrow
    if NOT any? turtles-here [
      regrow
      ;;if go-tests-on? [type "this patch regrew! \n"]
    ]
   ]
  ]

  if ticks mod 24 = 0 [
    ask primates [
      if daily-distance-traveled = 0 [
        error (word who " hasn't moved")
        stop
      ]
      set distance-traveled distance-traveled + daily-distance-traveled
      set daily-distance-traveled 0
    ]

  ]


  if ticks = 5000 [
    ask victories [set victory-counter 0]
    ask defeats [set defeat-counter 0]
    ask fightsavoided [set avoid-counter 0]
    ask attacks [set attack-counter 0]
  ]


 if burn-in-complete [
    if attacks-complete and not attacks-saved [
      make_attack_output
    ]

    if avoids-complete and not avoids-saved [
      make_avoid_output

    ]
  ]



end

to find_nearest-primates
      set nearest-primates other primates in-radius other-primate-detection-radius
  if (count nearest-primates > 3) [
    set nearest-primates min-n-of 3 nearest-primates [distance myself]
  ]

end

to find_visible-resources
  set visible-resources patches in-radius resource-detection-radius with [penergy > 0] ;; find all resource patches, into an agent-set

;  if member? patch-here visible-resources [
;    ask patch-here [
;      let not-me-patches other [visible-resources] of myself
;      ask myself [set visible-resources not-me-patches]
;      ]
;  ]



  set visible-resources (sort-on [(- penergy)] visible-resources) ;; sort by penergy descending; now this is a list

end

to eat
  ;;show "I'm chowin down!"
  move-to patch-here
  set stored-energy stored-energy + extraction-rate
  set penergy penergy - extraction-rate
  set total-energy-gained total-energy-gained + 1

  ask patch-here [
      (ifelse penergy = 0 [set pcolor black]
        [set pcolor scale-color green penergy 0 quality-max])
    ]


end




to move  ;; this is something already asked of primates, so don't ask primates again within this func!
  ;; movement will be a compromise between resources, movement cohesion, avoidance


  ;; first, change direction via either "wander" or "forage"
  (ifelse
    stored-energy = 0 [;; this is effectively a placeholder - agents started with 10 energy and never lost any, so this was never called.
      wander
    ]
    (not empty? visible-resources) [
      forage
    ]
    [
      wander
      ])

  ;; then, check that path ahead is clear; if someone is there, decide whether to attack or not
  (ifelse any? other primates-on patch-ahead 1
    [decide_to_attack]
    [
      fd 1
      set daily-distance-traveled daily-distance-traveled + 1
  ])
;

  if any? other primates-here [
    move-to min-one-of (patches with [not any? primates-here]) [distance myself]
    ;show "I had to jump away because someone was there!"
  ]
end

to forage ;;
  set heading new_headings
  ;;type "changing heading to " type max-one-of patches in-cone resource-detection-radius visual-angle [penergy] type "\n"
end

to wander ;; this is mutually exclusive with forage -

  set heading heading + ((random 30) - 15) ;; totally random turns


end


to-report heading-towards-nearest-primates  ;; turtle procedure
  ;; "towards myself" gives us the heading from the other turtle
  ;; to me, but we want the heading from me to the other turtle,
  ;; so we add 180
  let x-component mean [sin (towards myself + 180)] of nearest-primates
  let y-component mean [cos (towards myself + 180)] of nearest-primates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]

end

to-report new_headings
  let change-in-heading 0 ;;; i think this might be causing an issue

  ;;agent responds to either conspecifics or resources

  carefully [
    (ifelse (any? nearest-primates AND (distance min-one-of nearest-primates [distance myself]) > 7) [
      ;;agent is too far from other primates, so ignore resources and point towards nearest-primates
      set change-in-heading heading-towards-nearest-primates
      ;show "heading towards " show nearest-primates
      set patch-picked no-patches
  ]
  [ ;;; nearest-primates are close enough, so pick the best visible patch - or you have no nearest-primates at all
     (ifelse (is-patch? patch-picked) and (member? patch-picked visible-resources) [
          set change-in-heading towards patch-picked
          ;show "still heading towards" show patch-picked
        ]
        [
             ;;;;need to pick the patch from visible-resources
       set patch-picked first visible-resources
       set change-in-heading towards patch-picked
            ;;; this should be replaced with a way for the priamte to choose alternatively if another primate is already on that
       ;show "new patch picked: " show patch-picked
       ])
  ])]
  [
    error error-message
    set change-in-heading towards one-of neighbors with [not any? primates-here]
  ]
  ;show change-in-heading
  report change-in-heading
end

to-report am-i-the-winner [opponent]
  let self-winner? one-of list true false

  if asymmetry = "deterministic" [
    set self-winner? ([rhp] of opponent < rhp)
    ;show "Deterministic. Opponent: " type [rhp] of opponent type " my own rhp: " type rhp
  ]

  if asymmetry = "probabilistic" [
      let j random-float 1.0 ;; j is a random decimal-number that is used below to determine if an individual wns a fight or not (fight influenced by rhp value)
      let prob_self (rhp / (rhp + [rhp] of opponent)) ;; prob_self denotes the probability that an individual will win a fight (calculated using rhp values of the individuals involved and compared to j)

      set self-winner? (j < prob_self)

    ;show "Probabilistic. the die-roll was " type j
    ;show "opponent: " type [rhp] of opponent type " my own rhp: " type rhp
  ]
  ;show self-winner? type " should be true if self wins and false if opp wins"
  report self-winner?
end

to fight [opponent] ;; function to have individuals (self and opponent) fight each other
    if opponent != nobody [ ;; if the opponent does not have a value of "nobody", then run the code

      ask opponent [set fights ([fights] of opponent + 1)] ;; have the opponent increase their count of fights by 1
      set fights (fights + 1) ;; have self increase their count of fights by 1

      (ifelse am-i-the-winner opponent [

      ;; I WON
        ;show "self won"
        update_winner_against opponent
        ask opponent [update_loser_against myself]
        fd 1

        ask out-victory-to opponent [set victory-counter victory-counter + 1] ;;increases the counter on the victory link going from self of opponent by 1
        ask in-defeat-from opponent [set defeat-counter defeat-counter + 1] ;; increases the counter on the defeat link going from opponent to self by one
      ]
      [
        ;; I LOST
        ;show "self lost :("
        ask in-victory-from opponent [set victory-counter victory-counter + 1]
        ask out-defeat-to opponent [set defeat-counter defeat-counter + 1]

        update_loser_against opponent
        ask opponent [update_winner_against myself]
      ])

    update_dom_list
    ask opponent [update_dom_list]
    ]
end


to update_winner_against [losing]
  set wns wns + 1 ;; opponent recalculates their wns counter to increase it by 1
          ;;show "I win! \n"

          ifelse dom-score > 0.98 [
            set dom-score 1.0
          ][
            set dom-score dom-score + 0.01
          ]

        ;;show "I win!"
        set wns_r (wns / fights) ;; self recalculates their wns ratio to reflect updated fights and wns
        set lsses_r (lsses / fights) ;; self recalculates their lsses ratio to reflect updated fights and lsses

end


to update_loser_against [winner]
  set lsses (lsses + 1) ;; opponent recalculates their lsses counter to increase it by 1
   ;;;; also dom-score stuff here!!!!
          ifelse dom-score > 0.02 [
             set dom-score dom-score - 0.01
             ][
             set dom-score 0.01
             ]


          set heading (towards winner + one-of (range 90 270)) ;; opponent turns around
          fd 3 ;; opponent runs away



              if any? other primates-here [
                 move-to min-one-of (patches with [not any? primates-here]) [distance myself]
                 ;show "I had to jump away because someone was there!"
  ]

          ;;show "run away!"
          set wns_r (wns / fights) ;; opponent recalculates their wns ratio to reflect updated fights and wns
          set lsses_r (lsses / fights) ;; opponent recalculates their lsses ratio to reflect updated fights and lsses
end


to update_dom_list
      set dom-delta-list fput dom-score dom-delta-list

    ifelse length dom-delta-list > 4 [
      set running-dom-avg mean sublist dom-delta-list 0 5 ;;;;; the running average is only from the 10 most recent wins or losses
    ][
      set running-dom-avg mean dom-delta-list
    ]
end



to-report cost-estimation [opponent]
  let cost-estimate 0

      ;;;;;;;;;;;;;;;
    ;; ESTIMATE COSTS FOR EACH STRATEGY
    (ifelse assessment-info = "history" [ ;; linked to history switch; if this on, primates make decision based on dom-score

      ;; mutual assess switch allows primates to make decisions based on their and their opponent's dom-score
      (ifelse assessment-who = "mutual" [

        set cost-estimate (([dom-score] of opponent - dom-score) + 1) / 2 ;;; this puts the difference in scores back on a 0.0-1.0 scale
        ]

        assessment-who = "self" [
          set cost-estimate (1.0 - dom-score) ;; have to do the + 1s to avoid dividing by 0
        ]

        assessment-who = "opponent" [
          set cost-estimate ([dom-score] of opponent);; the opponents ratio of wins to losses, 1 - to make it the same direction as others
        ]
        [error "You need to turn on the mutual-assess, self-only, or opponent-only switch."])
      ;;show prob-decision type " probability of deciding to fight \n"
      ]


    assessment-info = "knowledge" [ ;; linked to knowledge switch; if this on then primates make conflict decisions based on their and their opponent's rhp values

      (ifelse assessment-who = "mutual" [  ;; mutual assess switch allows primates to make decisions based on their and their opponent's rhp values
        set cost-estimate (([rhp] of opponent - rhp) + 7) / 14 ;; changed to 14 and 7 becuse starting rhp values at 1 rather than 0 (1-8)
      ]

      assessment-who = "self" [
        set cost-estimate (1.0 - (( rhp - 1 ) / 7))
      ]

      assessment-who = "opponent" [
          set cost-estimate ((([rhp] of opponent) - 1) / 7)
      ]
          [error "You need to turn onthe mutual-assess, self-only, or opponent-only switch."])
      ]
      [error "Neither history nor knowledge switch was on!"])
  report cost-estimate
end


to-report benefit-estimation
  let bene 0.01

  if sum [penergy] of neighbors > 0 [
    set bene (([penergy] of patch-ahead 1) / sum [penergy] of neighbors) ;; the benefit assessment is the energy of patch in dispute as proportion of highest possible qualit
  ]

  report bene
end


to decide_to_attack ;; function to inform turtles on how to decide to fight

  let opponent one-of other turtles-on patch-ahead 1 ;; define i as one of the other primates on the next patch ahead (and not self as opponent)
  ;;show i type "is my opponent \n\n"

  ifelse [penergy] of patch-ahead 1 > 0 [


  if opponent != nobody [;; if i (the opponent) is not "nobody", then do the following function (this should be the very first thing checked)

    set running-dom-avg mean dom-delta-list
    let bene benefit-estimation
    let cost-est 0.01 ;; estimate costs will be calculated below, depending on current assessment strategy
    let probr 1.0 ;; prob of making the right choice. default to 1, so that agents will make the optimal choice if something goes wrong

    ;; then we can do a die roll
    let roll random-float 1.0

set cost-est cost-estimation opponent

    ;;;;;;;;;;;;;
    ;; USE COSTS TO DETERMINE BEST DECISION
  (ifelse bene > cost-est [
          set probr (bene / (bene + cost-est))

          (ifelse probr > roll [
              ask out-attack-to opponent [set attack-counter attack-counter + 1]
              fight opponent
          ][
              ask out-fightavoided-to opponent [set avoid-counter avoid-counter + 1]
        rt one-of (range 90 270)
        fd 1
          ])

        ]
        cost-est >= bene [
          ;;show "we decided not to fight \n" ;; if the fight does not occur, then self says, "we decided not to fight"
          ;let correct-choice avoid

          set probr (cost-est / (bene + cost-est))
          (ifelse probr > roll [
               ask out-fightavoided-to opponent [set avoid-counter avoid-counter + 1]
          rt one-of (range 90 270)
          fd 1
           ][
               ask out-attack-to opponent [set attack-counter attack-counter + 1]
               fight opponent
           ])
        ])
  ]
  ]
  [move-to one-of neighbors with [not any? primates-here]]
end

to decay_dom
  ask primates [
    if running-dom-avg < 0 [
    set running-dom-avg 0
  ]
    if running-dom-avg > 1 [
      set running-dom-avg 1
    ]
  ]
  ask primates with [dom-score != running-dom-avg] [
    ifelse dom-score > running-dom-avg [
      ;; dom-score is higher than the running average
      set dom-score dom-score - 0.01
    ][
      ;; dom-score is lower than the running average
      set dom-score dom-score + 0.005
    ]
  ]
end


to regrow
  ;; i need to add an if statement here so that penergy only goes back up to quality
  set penergy penergy + regrowth-rate ; this adds the regroth rate to the patches energy after it was eaten so that the patch can re-grow

end



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; OUTPUT PROCEDURES ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


to-report stop-condish

  ifelse attacks-saved AND avoids-saved [
    report true
  ][
    report false
  ]


end


to-report burn-in-complete
    ifelse ticks > 5000 [
    report true
  ][
    report false
  ]

end


to-report attacks-complete
    ifelse (sum [attack-counter] of attacks) >= 450 [
    report true
  ][
    report false
  ]

end

to-report avoids-complete
    ifelse (sum [avoid-counter] of fightsavoided) >= 450 [
    report true
  ][
    report false
  ]


end









to make_avoid_output
   let scenario "none"
  (ifelse assessment-info = "history" [
    (ifelse assessment-who = "mutual" [set scenario "mutual-hist"]
    assessment-who = "opponent" [set scenario "oppo-hist"]
    assessment-who = "self" [set scenario "self-hist"]
    [error "switch error"])
  ]
  assessment-info = "knowledge" [
    (ifelse assessment-who = "mutual" [set scenario "mutual-rhp"]
    assessment-who = "opponent" [set scenario "oppo-rhp"]
    assessment-who = "self" [set scenario "self-rhp"]
    [error "switch error"]  )
  ])

  let asym "none"
  ifelse asymmetry = "deterministic" [set asym "deter"][set asym "prob"]

  let resource-type "none"
  ifelse resource-dist = "clumped" [set resource-type "clump"][set resource-type "uni"]



  ;;AVOID

  set-current-directory (word folder-path "\\" "avoid")
  let file-name (word "avoidance-output-" asym "-" resource-type "-" scenario "-" behaviorspace-run-number ".csv")
  file-open file-name
  file-type "self, "
  file-type "opp, "
    file-print "fightsavoided"
    file-close

  file-open file-name
  ask fightsavoided [
      file-type (word ([who] of end1) ", ")
      file-type (word ([who] of end2) ", ")
      file-print avoid-counter
    ]
  file-close

  set avoids-saved true

end


to make_attack_output

  let scenario "none"
  (ifelse assessment-info = "history" [
    (ifelse assessment-who = "mutual" [set scenario "mutual-hist"]
    assessment-who = "opponent" [set scenario "oppo-hist"]
    assessment-who = "self" [set scenario "self-hist"]
    [error "switch error"])
  ]
  assessment-info = "knowledge" [
    (ifelse assessment-who = "mutual" [set scenario "mutual-rhp"]
    assessment-who = "opponent" [set scenario "oppo-rhp"]
    assessment-who = "self" [set scenario "self-rhp"]
    [error "switch error"]  )
  ])

  let asym "none"
  ifelse asymmetry = "deterministic" [set asym "deter"][set asym "prob"]

  let resource-type "none"
  ifelse resource-dist = "clumped" [set resource-type "clump"][set resource-type "uni"]



  ;;CONFLICT FILE

  set-current-directory (word folder-path "\\" "conflict")
  let file-name (word "victories-" asym "-" resource-type "-" scenario "-" behaviorspace-run-number ".csv")

  show file-name

  file-open file-name
  file-type "winner, "
  file-type "loser, "
  file-print "victorycount"
  file-close

  file-open file-name
  ask victories [
    file-type (word ([who] of end1) ", ")
    file-type (word ([who] of end2) ", ")
    file-print victory-counter
  ]
  file-close


;; ATTACK FILE (NEW)

  set-current-directory (word folder-path "\\" "attacks")
  let file-name2 (word "attacks-" asym "-" resource-type "-" scenario "-" behaviorspace-run-number ".csv")

  show file-name2

  file-open file-name2
  file-type "approaching, "
  file-type "opponent, "
  file-print "attackcount"
  file-close

  file-open file-name2
  ask attacks [
    file-type (word ([who] of end1) ", ")
    file-type (word ([who] of end2) ", ")
    file-print attack-counter
  ]
  file-close

set attacks-saved true


end

to set_folder_path
  set folder-path "C:\\Users\\Marcy\\Desktop\\dec 27"

  ;; folders should look like hm.c.d
  let scenario-folder "none"
  ifelse assessment-info = "history" [set scenario-folder "h"][set scenario-folder "k"]

  (ifelse assessment-who = "mutual" [set scenario-folder (word scenario-folder "m.")]
  assessment-who = "opponent" [set scenario-folder (word scenario-folder "o.")]
  assessment-who = "self" [set scenario-folder (word scenario-folder "s.")]
  [error "switch error"])


  ifelse resource-dist = "clumped" [set scenario-folder (word scenario-folder "c.")][set scenario-folder (word scenario-folder "u.")]
  ifelse asymmetry = "deterministic" [set scenario-folder (word scenario-folder "d")][set scenario-folder (word scenario-folder "p")]

  set folder-path (word folder-path "\\" scenario-folder)



end


to make_energy_output


  let scenario "none"
  (ifelse assessment-info = "history" [
    (ifelse assessment-who = "mutual" [set scenario "mutual-hist"]
    assessment-who = "opponent" [set scenario "oppo-hist"]
    assessment-who = "self" [set scenario "self-hist"]
    [error "switch error"])
  ]
  assessment-info = "knowledge" [
    (ifelse assessment-who = "mutual" [set scenario "mutual-rhp"]
    assessment-who = "opponent" [set scenario "oppo-rhp"]
    assessment-who = "self" [set scenario "self-rhp"]
    [error "switch error"]  )
  ])

  let asym "none"
  ifelse asymmetry = "deterministic" [set asym "deter"][set asym "prob"]

  let resource-type "none"
  ifelse resource-dist = "clumped" [set resource-type "clump"][set resource-type "uni"]

  ;;energy_efficiency
  set-current-directory (word folder-path "\\" "energy_efficiency")
  let file-name3 (word "foraging-eff-" asym "-" resource-type "-" scenario "-" behaviorspace-run-number ".csv")
  file-open file-name3
  file-type "who, "
  file-type "energy-over-distance, "
  file-print "energy-over-time"
  file-close

  file-open file-name3
  ask primates [
    file-type (word who ", ")
    file-type (word (total-energy-gained / distance-traveled) ", ")
    file-print total-energy-gained / ticks
  ]
 file-close

end

to-report foraging-efficiency-dist
  report mean [total-energy-gained / distance-traveled] of primates

end


to-report foraging-efficiency-time
  report mean [total-energy-gained / ticks] of primates
end
@#$#@#$#@
GRAPHICS-WINDOW
239
40
742
544
-1
-1
23.6
1
10
1
1
1
0
1
1
1
-10
10
-10
10
0
0
1
ticks
30.0

BUTTON
39
41
102
74
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

BUTTON
149
43
212
76
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
56
196
194
241
resource-dist
resource-dist
"uniform" "clumped"
1

CHOOSER
55
251
193
296
asymmetry
asymmetry
"deterministic" "probabilistic"
1

CHOOSER
55
141
193
186
assessment-info
assessment-info
"knowledge" "history"
0

CHOOSER
54
86
192
131
assessment-who
assessment-who
"self" "opponent" "mutual"
2

PLOT
891
112
1091
262
min and mean foraging efficiency
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
"default" 1.0 0 -16777216 true "" "plot mean [total-energy-gained] of primates / (ticks + 1)"
"pen-1" 1.0 0 -7500403 true "" "plot min [total-energy-gained] of primates / (ticks + 1)"

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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="conflict_test-defeat" repetitions="2" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>count [defeat-counter] of defeats</metric>
    <enumeratedValueSet variable="opponent-only?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="knowledge?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutual-assess?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="self-only?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="history-mutual" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="1000000"/>
    <exitCondition>stop-condish</exitCondition>
    <enumeratedValueSet variable="opponent-only?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="knowledge?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutual-assess?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="self-only?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="knowledge-mutual" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="1000000"/>
    <exitCondition>stop-condish</exitCondition>
    <enumeratedValueSet variable="opponent-only?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="knowledge?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutual-assess?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="self-only?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="history-self" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="1000000"/>
    <exitCondition>stop-condish</exitCondition>
    <enumeratedValueSet variable="opponent-only?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="knowledge?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutual-assess?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="self-only?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="knowledge-self" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="1000000"/>
    <exitCondition>stop-condish</exitCondition>
    <enumeratedValueSet variable="opponent-only?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="knowledge?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutual-assess?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="self-only?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="history-opp" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="1000000"/>
    <exitCondition>stop-condish</exitCondition>
    <enumeratedValueSet variable="opponent-only?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="knowledge?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutual-assess?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="self-only?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="knowledge-opponent" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="1000000"/>
    <exitCondition>stop-condish</exitCondition>
    <enumeratedValueSet variable="opponent-only?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="knowledge?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutual-assess?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="self-only?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="runtime-error-test-history-opp" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>type sum [avoid-counter] of fightsavoided</final>
    <timeLimit steps="10000"/>
    <enumeratedValueSet variable="opponent-only?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="knowledge?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="history?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutual-assess?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="self-only?">
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testerrorwnoforaging" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>mean [total-energy-gained] of primates</metric>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;clumped&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;probabilistic&quot;"/>
      <value value="&quot;deterministic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testerrorwnoforaging-prob-uni" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>mean [total-energy-gained] of primates</metric>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;probabilistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testerrorwnoforaging-prob-clump" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>mean [total-energy-gained] of primates</metric>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;clumped&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;probabilistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testerrorwnoforaging-deter-clump" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>min [total-energy-gained] of primates</metric>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;clumped&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;deterministic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testerrorwnoforaging-deter-uni" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>min [total-energy-gained] of primates</metric>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;deterministic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="testerrorwnoforaging-deter-clump" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3000"/>
    <metric>min [total-energy-gained] of primates</metric>
    <metric>mean [total-energy-gained] of primates</metric>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;clumped&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;deterministic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="oct-full-experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="150000"/>
    <exitCondition>stop-condish</exitCondition>
    <metric>ticks</metric>
    <metric>sum [victory-counter] of victories</metric>
    <metric>sum [avoid-counter] of fightsavoided</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;deterministic&quot;"/>
      <value value="&quot;probabilistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;clumped&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
      <value value="&quot;knowledge&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="oct-missing-one" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="20000"/>
    <exitCondition>stop-condish</exitCondition>
    <metric>ticks</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;probabilistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;uniform&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;knowledge&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="nov-full-experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_output_csv</final>
    <timeLimit steps="150000"/>
    <exitCondition>stop-condish</exitCondition>
    <metric>ticks</metric>
    <metric>sum [victory-counter] of victories</metric>
    <metric>sum [avoid-counter] of fightsavoided</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;deterministic&quot;"/>
      <value value="&quot;probabilistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;clumped&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
      <value value="&quot;knowledge&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <timeLimit steps="1"/>
    <metric>sum [penergy] of patches</metric>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;knowledge&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;clumped&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;probabilistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;mutual&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="dec-full-experiment" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>make_energy_output</final>
    <timeLimit steps="150000"/>
    <exitCondition>stop-condish</exitCondition>
    <metric>ticks</metric>
    <metric>sum [victory-counter] of victories</metric>
    <metric>sum [avoid-counter] of fightsavoided</metric>
    <metric>foraging-efficiency-time</metric>
    <enumeratedValueSet variable="asymmetry">
      <value value="&quot;deterministic&quot;"/>
      <value value="&quot;probabilistic&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="resource-dist">
      <value value="&quot;uniform&quot;"/>
      <value value="&quot;clumped&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-info">
      <value value="&quot;history&quot;"/>
      <value value="&quot;knowledge&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="assessment-who">
      <value value="&quot;self&quot;"/>
      <value value="&quot;opponent&quot;"/>
      <value value="&quot;mutual&quot;"/>
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
@#$#@#$#@
0
@#$#@#$#@
