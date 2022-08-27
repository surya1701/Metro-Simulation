extensions[bitmap csv]  ;; load extensions for image and csv files
breed [stations station]  ;; station breed
breed [passengers passenger] ;; passenger breed
breed [metros metro] ;; metro breed

links-own [ duration spd ]  ;; path between stations own speed and duration properties
stations-own [ distance-from-other-stations path ]  ;; each station has distance-from-other-stations and path which are used to determine optimal path for passengers
metros-own [capacity cap-cur cur-speed waitTicks startTick endTick myPath curi rev time-travel unit-test-time]  ;; each metro has capacity limit, current capacity, speed, etc as properties
passengers-own [path mySrc myDest myTick cur which-Metro my-speed totalwait ttime ] ;; each passengers has optimal path, source, destination, etc as properties


globals [ highlighted-vertices id-highlighted infinity temp speed all-line-colors metro-path-mouse metro-path-mouse-done today all-paths end-tick-day display?]

to setup  ;; sets up user determined image, and other properties for model
  ca
  let image-file image-file-name
  let image bitmap:import image-file
  let height bitmap:height image
  let width bitmap:width image
  resize-world 0 width - 1 0 height - 1
  set-patch-size 1
  set-default-shape stations "house ranch"
  set-default-shape metros "car"
  set-default-shape passengers "person"
  import-pcolors-rgb image-file
  set infinity 99999
  set speed 1
  set metro-path-mouse-done -1
  set metro-path-mouse []
  set all-line-colors (list 105 15 45 55 9.9)
  set today 1
  set end-tick-day 1080
  set display? false
  if display-setup [set display? true]
  reset-ticks
end

to-report get-color [link-color] ;; determine color for csv input
  if is-string? link-color [
    if link-color = "red" [ set link-color red ]
    if link-color = "blue" [ set link-color blue ]
    if link-color = "yellow" [ set link-color yellow ]
    if link-color = "green" [ set link-color green ]
    if link-color = "white" [ set link-color white ]
    if link-color = "orange" [set link-color orange ]
  ]
  report link-color
end

to-report convtick [time] ;; convert time to ticks (1 minute = 1 tick)
  let l length time
  let m read-from-string substring time (l - 2) l
  let h read-from-string substring time 0 (l - 3)
  ;trains start from 5:30 AM and ends at 11:30 PM
  let t ( (h * 60) + m ) - 330
  report t
end

to-report getlabel [stat] ;; get name(label) of turtle (station)
  let w -1
  ask stat[
    set w label
  ]
  report w
end

to-report getwho [a]  ;; get who of turtle (station)
  let w -1
  ask a [ set w who ]
  report w
end

to save-paths ;; save paths for travelling from one station to any other station
  let stats sort-on [who] stations
  set all-paths []
  foreach stats [s ->
    foreach stats [ss ->
        let pth print-path getwho s getwho ss
        set all-paths lput (list getwho s getwho ss pth) all-paths
    ]
  ]
end

to load-config ;; load details from csv file
  clear-turtles
  ifelse display? [ ;; for demo purpose only
    load-stations "data/stations-washington-sampled.csv"
    attatch-links "data/links-washington-sampled.csv"
    load-metros "data/metros-sampled.csv"
    display
  ][ ;; for washington data
    load-stations "data/stations.csv"
    attatch-links "data/links.csv"
    load-metros "data/metros.csv"
    no-display
  ]
end


to load-stations [filename]  ;; load all stations to correct positions on image
  file-open filename
  let stats []
  while [ not file-at-end? ] [
    let row csv:from-row file-read-line ;; read csv file row by row
    let s []
    set s lput item 0 row s ;; station id
    set s lput item 1 row s ;; station xcor
    set s lput item 2 row s ;; station ycor
    set s lput item 3 row s ;; station name
    create-stations 1 [
      setxy 0 0
      set color black
      set size 30
      set label-color black
    ]
    set stats lput s stats
  ]
  let iter 0
  foreach stats[
    [s] -> ask station item 0 s [ setxy item 1 s item 2 s  set label item 3 s ] ;; set proper values to correct stations according to id(who)
  ]
  file-close  ;; close file
end


to load-metros [filename]  ;; load all metros to correct positions wrt stations
  file-open filename
  let stats []
  while [ not file-at-end? ] [
    let row csv:from-row file-read-line  ;; read csv file row by row
    let path-temp []
    let n item 4 row  ;; number of stations in path
    let i 0
    while [i < n] [
      set path-temp lput one-of stations with [label = item (5 + i) row] path-temp  ;; get path
      set i i + 1
    ]
    ask first path-temp [
      ask patch-here [
        sprout-metros 1 [
          setxy 0 0
          set hidden? true
          set size 30
          set myPath path-temp
          set color item 0 row ;; color metro
          set capacity item 1 row ;; capacity of metro
          set startTick convtick item 2 row ;; starting time of metro (24 hour format)
          set endTick convtick item 3 row ;; ending time of metro (24 hour format)
          set cap-cur 0 ;; current number of passengers is 0
          set waitTicks 0 ;; train is not waiting at station
        ]
      ]
    ]
  ]
  file-close  ;; close file
end

to attatch-links [filename] ;; create link between two stations
  file-open filename
  while [ not file-at-end? ] [
    let row csv:from-row file-read-line
    let stat1 [who] of one-of stations with [label = item 0 row]
    let stat2 [who] of one-of stations with [label = item 1 row]
    let link-color get-color item 3 row
    ask station stat1[
      create-link-with station stat2
      let e1 0
      let e2 0
      let d [distance station stat1]  of station stat2
      ask link stat1 stat2
      [
        set color link-color
        set thickness 5
        set duration item 2 row
        set spd d / duration
      ]
    ]
  ]
  file-close
end

to spawn-passengers [tick-value] ;; create passengers from csv files
  find-path-lengths
  ifelse display?
  [ file-open "data/passengers-sampled.csv"]
  [ file-open "data/passengers.csv"]
  let result 0
  let ttt 0
  while [ not file-at-end? ] [
    let row csv:from-row file-read-line

    let time convtick item 2 row ;; spawn time for passenger
    let day item 3 row ;; spawn day for passenger
    if day = 0 or (day = 8 and today <= 5) or (day = 9 and today >= 6)  [
      set day today
    ]
    if day = today and time >= tick-value * 120 and time < (tick-value + 1) * 120 [ ;; spawn passenger if its spawn time is under 2 hours from current time
      create-passengers 1[ ;; create new passenger
        set hidden? true
          set size 25
          let srctemp 0
          ask stations with [label = item 0 row] [ set srctemp who] ;; source station
          let desttemp 0
          ask stations with [label = item 1 row] [ set desttemp who] ;; destination station
        if srctemp = desttemp [
          die
        ]
          set mySrc srctemp ;; source station
          set cur 0
          set myDest desttemp ;; destination station
          set path item 2 item ( (count stations * srctemp ) + desttemp ) all-paths ;; path of metro
          set which-Metro -1
          set myTick time
          set totalwait 0
          set ttime 0
          set color white
      ]
    ]
  ]

  file-close ;; close file
end


to go-metro ;; start simulation
  if ticks = 0 [ ;; at the start of each day
  find-path-lengths ;; find duration of travel between each station pair
  save-paths  ;; find optimal paths for travel between each station pair
  ]
  if ticks = 1080 [ ;; at the end of each day
    ask metros [ die ] ;; kill all metros
    ask passengers [ die ] ;; kill all passengers
    reset-ticks ;; reset ticks to 0
    ifelse display? [ ;; load metros
      load-metros "data/metros-sampled.csv"
    ][
      load-metros "data/metros.csv"
    ]
    set today today + 1 ;; go to next day
    if today = 8 [ ;; if sunday has passed, stop simulation
      stop
      set today 1
    ]
  ]
  if ticks mod 120 = 0 [ ;; load correct passengers every 2 hours
    spawn-passengers ticks / 120
  ]

  ask passengers with [myTick = ticks]  ;; spawn correct passengers to their correct locations at their spawn time
  [
    set myTick -1
    set hidden? false
    move-to station mySrc
  ]
  ask metros with [startTick = ticks]  ;; spawn correct metros to their correct locations at their spawn time
  [
    set startTick -1
    set hidden? false
    move-to first myPath
    set time-travel 0
    set unit-test-time 0
  ]
  ask stations [ set color black] ;; set color of each station to black
  ask metros with [startTick = -1 ][ ;; for each valid metro (which have already been spawned)
    set time-travel time-travel + 1  ;; for unit test
    let metro-Id who
    let ccap cap-cur
    let totcap capacity
    let curr-station one-of stations-here
    if curr-station != nobody [ ;; if there are any stations at current position
       if (endTick <= ticks ) and ( curr-station = first myPath )[ ;; check if station is first in path or train is still valid
        ask passengers with [ which-Metro = metro-Id ][ ;; passengers with desination as this station aloght from train
          set which-metro -1
          set my-speed 0
          if one-of stations-here = station myDest
          [
            die
          ]
        ]
        die
      ]
      set waitTicks waitTicks + 1 ;; train starts waiting at platform
      if waitTicks = station-stop-time [set waitTicks 0] ;; if train has waited enough
      if waitTicks = 0 [  ;; passengers with desination as this station get out of train
        ask passengers-here with [ myTick = -1 and ( which-Metro = metro-Id ) and ( station myDest = curr-station ) ][
          set ccap ccap - 1
          die
        ]
        if ( curr-station = last myPath ) [ ;; if train has reached last station in its path, it will start moving in reverse direction (reverse path)
          set rev 1
          set unit-test-time time-travel  ;; for unit test
          set time-travel 0  ;; for unit test
        ]
        if ( curr-station = first myPath ) [  ;; if train has reached first station in its path, it will start moving in correct direction (normal path)
          if ( first myPath = last myPath ) and ( curi + 1 = length myPath)[ ;; if train has a looped path, it will restart at first station in its path and go in correct order
            set curi 0
            set unit-test-time time-travel  ;; for unit test
            set time-travel 0  ;; for unit test
          ]
            set rev 0
            set unit-test-time time-travel  ;; for unit test
           set time-travel 0  ;; for unit test
        ]
        let next-station 0
        ifelse rev = 1 [ ;; get next station in metro's path
          set next-station item (curi - 1) myPath
        ][
          set next-station item (curi + 1) myPath
        ]
        let clr color
        let cspd 0
        ask curr-station[  ;; find link/path which goes to next station for metro
          let next-dest-link one-of my-links with [color = clr and (end1 = next-station or end2 = next-station )]
          ask next-dest-link [
            set cspd spd
          ]
        ]
        face next-station ;; metro will face in direction of next station
        set cur-speed cspd ;; metro will set its speed the same as speed on metro line it will be travelling on
        ask passengers-here with [ myTick = -1 and which-Metro = metro-Id ][  ;; all passengers inside the metro
          ifelse ( item ( cur + 1 ) path = next-station )[ ;; passengers will stay in train if the next station in their path is the same as metro's next station
            face next-station
            set my-speed cspd
            set cur cur + 1
          ][ ;; passengers will get off the metro
            set which-Metro -1
            set my-speed 0
            set ccap ccap - 1
          ]

        ]
        ask passengers-here with [ myTick = -1 and which-Metro = -1 ][ ;; passengers on the station
          if ( ( item ( cur + 1 ) path = next-station ) and ( ccap < totcap ) ) [ ;; if next station for them is same as train's next station, and there is enough space inside train, they will get on the train
            face next-station
            set my-speed cspd
            set which-Metro metro-Id
            set cur cur + 1
            set ccap ccap + 1
          ]
        ]
        ifelse rev = 0 [
          set curi curi + 1
        ][
          set curi curi - 1
        ]
        set cap-cur ccap
      ]
    ]
    if waitTicks = 0 [ ;; if train is not waiting, it is moving on track
      fd cur-speed
      ask passengers with [ myTick = -1 and which-Metro = metro-Id ][ ;; if train is moving, passengers onboard the train also move
        fd my-speed
      ]
    ]
  ]
  ask passengers with [myTick = -1][ ;; for graph
    ifelse which-Metro = -1 [
      set totalwait totalwait + 1
    ][
      set totalwait 0
    ]
    set ttime ttime + 1
  ]
  tick
  if ( ticks > end-tick-day)[ ;; if day ends, stop simulation
    stop
  ]
end

to save-config ;; save user settings and map details for stations, links and metros
  save-stations
  save-links
  save-metros
end

to save-stations  ;; save all stations entered by user to csv file
  let stats sort-on [who] stations
  let data []
  foreach stats [s ->
    ask s [set data lput (list who xcor ycor label) data]
  ]
  csv:to-file "data/stations.csv" data
end

to save-links ;; save paths to csv, saving the labels for each station pair, duration and color of link
  csv:to-file "data/links.csv" [ (list (getlabel end1) (getlabel end2) duration color ) ] of links
end

to save-metros   ;; save all metros entered by user to csv file
  let data-all []
  ask metros [ ;; for each metro
    let data (list color capacity "05:30" "23:30" length myPath) ;; default metro start time, end time and number of stations in path is set
    foreach myPath [stn ->
      set data lput [label] of stn data ;; path of metro is added
    ]
    set data-all lput data data-all
  ]
  csv:to-file "data/metros.csv" data-all ;; settings saved to csv file
end

to add-station  ;; create new station manually
  if mouse-down? [
    create-stations 1 [
      set size 30
      set color black
      set label station-name
      set label-color black
      setxy mouse-xcor mouse-ycor
      while [mouse-down?] [
        setxy mouse-xcor mouse-ycor
        display
      ]
      let c min-one-of other stations in-radius 2 [distance myself]
      if c != nobody[
        die
      ]
    ]
  ]
end

to add-link ;; create new link between stations manually
  if count stations < 2[
    stop
  ]
  if mouse-down? [
    let link-color get-color line-color
    let candidate min-one-of stations [distancexy mouse-xcor mouse-ycor] ;; find closest station
    ask candidate [
      if highlighted-vertices = 0 [set id-highlighted who]
      if color = black [
        set highlighted-vertices highlighted-vertices + 1
        set color link-color
        if highlighted-vertices = 2 [
          create-link-with station id-highlighted ;; create link
          let e1 0
          let e2 0
          ask link [who] of candidate id-highlighted
          [
            set color link-color ;; set color
            set thickness 5
            set duration duration-of-travel ;; set duration of travel on link
            set e1 end1
            set e2 end2
            let d [distance e1] of e2
            set spd d / duration ;; set speed of travel on link
          ]
          set highlighted-vertices 0
          ask stations with [color = link-color] [set color black]
        ]
      ]
      while[mouse-down?] [display]
    ]
  ]
end

to spawn-metro  ;; create new metro
  let link-color get-color metro-color
  if count links with [color = link-color]  < 1 [
    stop
  ]
  if mouse-down? [
    if metro-path-mouse-done = -1 or link-color = metro-path-mouse-done [
      set metro-path-mouse-done link-color
      let candidate min-one-of stations [distancexy mouse-xcor mouse-ycor]
      ask candidate [
        ifelse color = black or color = cyan [
          set color link-color
          show length metro-path-mouse
          ifelse length metro-path-mouse > 0 [
            if [color] of link-with last metro-path-mouse != link-color [
              ask stations [ set color black ]
              set metro-path-mouse []
              set metro-path-mouse-done -1
              stop
            ]
          ]
          [set color cyan]
        ]
        [
          if color = link-color [
            set metro-path-mouse-done -1
            set color red
          ]
        ]
      ]

      if metro-path-mouse-done != -1 [ set metro-path-mouse lput candidate metro-path-mouse ]
      if metro-path-mouse-done = -1 and length metro-path-mouse != 0 [
        ask first metro-path-mouse [
          ask patch-here [
            sprout-metros 1 [
              setxy 0 0
              set hidden? true
              set size 30
              set myPath metro-path-mouse
              set color link-color
              set capacity metro-capacity
              set startTick ticks
              set endTick end-tick-day
              set cap-cur 0
              set waitTicks 0
            ]
          ]
        ]
      ]
    ]
    if metro-path-mouse-done = -1 [
      ask stations [ set color black ]
      set metro-path-mouse []
    ]
    while[mouse-down?] [display]
  ]
end

to find-path-lengths ;;floyd warshall algorithm to get optimal path for passenger. time is the factor here
  ask stations
  [
    set distance-from-other-stations []
    set path []
  ]
  let i 0
  let j 0
  let k 0
  let stat1 one-of stations
  let stat2 one-of stations
  let stat-count count stations
  while [i < stat-count]
  [
    set j 0
    while [j < stat-count]
    [
      set stat1 station i
      set stat2 station j
      ifelse i = j
      [
        ask stat1 [
          set distance-from-other-stations lput 0 distance-from-other-stations
          set path lput -1 path
        ]
      ]
      [
        ifelse [ link-neighbor? stat1 ] of stat2
        [
          ask stat1 [
            let abcdf infinity
            ask links with [end1 = stat1 and end2 = stat2 ] [ set abcdf duration ]
            if abcdf = infinity [ ask links with [end1 = stat2 and end2 = stat1 ] [ set abcdf duration ] ]
            set distance-from-other-stations lput abcdf distance-from-other-stations
            set path lput stat2 path
          ]
        ]
        [
          ask stat1 [
            set distance-from-other-stations lput infinity distance-from-other-stations
            set path lput -1 path
          ]
        ]
      ]
      set j j + 1
    ]
    set i i + 1
  ]
  set i 0
  set j 0
  let d 0
  let tempstat 0
  while [k < stat-count]
  [
    set i 0
    while [i < stat-count]
    [
      set j 0
      while [j < stat-count]
      [
        set d ( (item k [distance-from-other-stations] of station i) + (item j [distance-from-other-stations] of station k))
        if d < (item j [distance-from-other-stations] of station i)
        [
          ask station i [ set distance-from-other-stations replace-item j distance-from-other-stations d ]
          set tempstat (item k [path] of station i)
          ask station i [ set path replace-item j path tempstat]
        ]
        set j j + 1
      ]
      set i i + 1
    ]
    set k k + 1
  ]

end

to-report print-path [ stat1 stat2 ] ;; print optimal path between two stations
  let s1 stat1
  let s2 stat2
  let pth []
  set pth lput station stat1 pth
  let iter 0
  while [ stat1 != stat2 ]
  [
    let p 0
    set p item stat2 [path] of station stat1
    set pth lput p pth
    ask p [ set stat1 who ]
    set iter iter + 1
    if ( iter > count stations )
    [
      set pth [ "np path" ]
      set stat1 stat2
    ]
  ]
  report pth
end

to-report psg-wait-time ;; graph for total passenger wait time
  let total-wait-time 0
  let count-psg 0
  ask passengers[
    if myTick = -1[
      set total-wait-time total-wait-time + totalwait
      set count-psg count-psg + 1
    ]
  ]
  ifelse count-psg > 0[
    report total-wait-time / count-psg
  ]
  [
    report 0
  ]
end

to-report total-avg-trip-time ;; graph for total average trip time for passengers
  let total-trip-time 0
  let count-psg 0
  ask passengers with [ myTick = -1 ][
    set total-trip-time total-trip-time + ttime
    set count-psg count-psg + 1
  ]
  ifelse count-psg > 0[
    report total-trip-time / count-psg
  ]
  [
    report 0
  ]
end

to-report station-wait-time [stat]  ;; graph for average wait time at stations for passengers
  let total-wait-time 0
  let count-psg 0
  ask stations with [label = stat ][
    ask passengers-here with [myTick = -1][
      set total-wait-time total-wait-time + totalwait
      set count-psg count-psg + 1
    ]
  ]
  ifelse count-psg > 0[
    report total-wait-time / count-psg
  ]
  [
    report 0
  ]
end

to-report average-train-congestion [clr]  ;; graph for average congestion in trains
  ifelse clr = 0 [
    let totm count metros with [startTick = -1 ]
    let totp count passengers with [myTick = -1 and which-metro != -1]
    if totm = 0[
      report 0
    ]
    report totp / totm
  ][
    let totm count metros with [startTick = -1 and color = clr]
    let who-list []
    ask metros with [startTick = -1 and color = clr] [ set who-list lput who who-list ]
    let totp count passengers with [myTick = -1 and member? which-metro who-list]
    if totm = 0[
      report 0
    ]
    report totp / totm
  ]
end

to unit-test  ;; unit tests
  let ff true
  ask metros[ ;; checking if trains are reaching destination within certain limits depending on station wait time
    let p myPath
    let l length myPath
    let i 0
    let tot 0
    while [i + 1 < l ] [
      let s1 item i myPath
      let s2 item (i + 1) myPath
      set s1 getwho s1
      set s2 getwho s2
      ask link s1 s2 [
        set tot tot + duration
      ]
      set tot tot + station-stop-time
      set i i + 1
    ]
    set tot tot - station-stop-time

    if ( tot - unit-test-time ) > 2 * station-stop-time [
      set ff false
    ]
  ]
  if ticks = 1080 [  ;; after day ends, check if all passengers and metros are not there
    if count passengers > 0 [
      set ff false
    ]
    if count metros > 0 [
      set ff false
    ]
  ]
  let stats sort-on [who] stations ;; check if all paths between stations are optimal or not
  foreach stats [s ->
    foreach stats [ss ->
      let pth print-path getwho s getwho ss
      let pth_test item 2 item ( (count stations * getwho s ) + getwho ss ) all-paths
      if pth != pth_test [
        set ff false
        ;let a 0
      ]
    ]
  ]
  ifelse ff [
    print "Unit Test Successful!"
  ][
    print "Unit Test Fail!"
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
4
10
805
779
-1
-1
1.0
1
10
1
1
1
0
0
0
1
0
792
0
759
1
1
1
ticks
30.0

BUTTON
980
75
1046
108
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
812
251
922
308
NIL
add-station
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
814
318
921
364
NIL
add-link
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
926
318
1056
363
line-color
line-color
"red" "blue" "green" "yellow" "white" "black" "orange"
6

SLIDER
1060
323
1232
356
duration-of-travel
duration-of-travel
1
100
1.0
1
1
NIL
HORIZONTAL

BUTTON
844
117
926
150
NIL
go-metro
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
814
374
924
417
NIL
spawn-metro
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
926
374
1059
419
metro-color
metro-color
"red" "blue" "green" "yellow" "white" "black" "orange"
0

INPUTBOX
841
11
1084
71
image-file-name
data/dcmap.jpg
1
0
String

SLIDER
1062
378
1234
411
metro-capacity
metro-capacity
0
100
10.0
1
1
NIL
HORIZONTAL

BUTTON
1090
263
1195
296
NIL
save-config
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
1054
75
1158
108
load-config
load-config
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
814
201
1235
248
To manually initialize stations and metro lines. Use functions below.
13
0.0
0

SLIDER
939
116
1111
149
station-stop-time
station-stop-time
1
10
9.0
1
1
ticks
HORIZONTAL

PLOT
3
789
400
1091
Average total wait time
Minutes
wait time
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -11085214 true "" "plot psg-wait-time"

PLOT
3
1099
400
1366
Average Station Congestion
Minutes
Count
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot (sum [count passengers-here] of stations)"

PLOT
864
792
1327
1092
Average Total Trip Time
Minutes
avg trip time
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-avg-trip-time"

PLOT
410
1097
848
1370
Average Per Station Wait Time
Minutes
wait time
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot station-wait-time station-metrics"

INPUTBOX
887
1223
1116
1283
station-metrics
L’Enfant
1
0
String

PLOT
411
790
846
1090
Average Train Congestion
Minutes
Congestion
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot average-train-congestion 0"
"pen-1" 1.0 0 -2674135 true "" "plot average-train-congestion red"
"pen-2" 1.0 0 -13840069 true "" "plot average-train-congestion green"
"pen-3" 1.0 0 -955883 true "" "plot average-train-congestion orange"
"pen-4" 1.0 0 -1184463 true "" "plot average-train-congestion yellow"
"pen-5" 1.0 0 -13345367 true "" "plot average-train-congestion blue"

INPUTBOX
926
250
1053
310
station-name
K
1
0
String

SWITCH
844
75
973
108
display-setup
display-setup
1
1
-1000

MONITOR
1088
24
1159
69
NIL
today
17
1
11

@#$#@#$#@
# ODD



## 1. Purpose and patterns

The purpose of this agent based modelling simulation is to serve as a tool to compare, study and understand metro-train systems. More specifically it aims to be a tool to test or simulate multiple metrics, like wait time for passengers, train congestion, average total trip time, average station congestion and average wait time for passengers at a station, for any given metro line network. A user can use it to construct their desirable metro network, and then use the model for simulating its performance. \
Given real world data, we observe that there is a big increase in wait time and congestion, both inside the train and at the station at rush hours. Statistically determined rush hours and the one we determine from our model is the same, and we also observe that there is a significant increase in traffic at junction stations as compared to other stations, irrespective of time. Given similar data, these patterns are reproducible. Given correct data, they are similar to real world patterns



## 2. Entities, state variables, and scales

The main entities that this model looks at are:



* **Metros**: The metro agents represent metros in the real world. They are able to move only on their designated metro line and path between predetermined stationary entities called stations. Along with the path, the spawn and death conditions are also determined by user input.
    * State:  They maintain the following variables in their state:-
        * Source  and Destination: start and end points of the metros.
        * Total and current capacity: How many people the train can transport and how many it is currently carrying.
        * Speed: the current speed to travel at (can change speed between stations). This is determined by the line it is on.
        * Path: The path that the train travels on.
    * Scale:  
        * Spatial: Depends on the provided map and positional data.
        * Temporal: moves &lt;speed> units every tick. Determined by the user.
* **Stations**: These are static agents. They represent the metro stations in the real world. These agents are usually set up in the beginning of the simulation. Stations are spawned at the beginning of the simulation. These agents do not die during the simulation.
    * State: They maintain the following variables in their state:-
        * Distance from other stations: list to keep track of distances to other stations from the current station.
        * Path: shortest path to travel from current station to another station.
    * Scale:
        * Spatial: Not Applicable
        * Temporal: Not applicable
* **Passengers**: These are agents that represent the metro riders or passengers in real life. They move with the metro when a metro is available and has sufficient capacity. Spawn time determined by the user and the agent dies on reaching the destination.
    * State: They maintain the following variables in their state:-
        * Path:  This is the shortest path computed by the passenger.
        * Source and Dest: source and destination station.
        * Spawn tick: Timestamp tick to spawn at.
        * inMetro: if the passenger is in a metro or not.
        * whoMetro: stores the ‘who’ of the metro the passenger was on.
        * totalWait: keeps track of its total wait time.
        * tTime: keeps track of its trip time.
    * Scale:
        * Spatial: appears much bigger but can be thought of as representing many people depending on the user.
        * Temporal: Same scale as metro
* **Metro Lines**: These are just links between the metro stations that together form the metro line network. 
    * States: They maintain the following variables in their state:-
        * Duration: time taken for a train to move between stations.
        * Speed: speed of a train on this link.
        * Colour: Line colour.
    * Scale:
        * Spatial: Depends on the provided map and positional data.
        * Temporal: Not Applicable
* **Environment**: The user can add a map image for the background for spatial reference when testing out metro networks on a real/fictional location.



![Variables](images/Variables.png)




## 3. Process overview and scheduling

The tool models event-driven entities. While the simulation is continuous, the processes are of scheduled nature. The patches display the city map and the stations are spawned. Along with this, their metro lines of appropriate colors are connected as links when the model is initialized. Once the simulation is started, based on the ticks and data inputs, the metros are spawned and moved to their respective stations. The metro repeats the same given route till it reaches the provided end time. The time here refers to the ticks that are converted to match the real time for ease of use in the data. The metro speed, capacity and wait time at the station is to be defined by the user. These parameters define the metro timetable, thus affecting passenger wait times and trip times. The metro schedule has been provided in the following figure.
![Figure1](images/Figure1.jpeg)

The passengers are spawned based on the given boarding time, start station and destination. Passengers are spawned every two hours and then they compute the shortest path based on these inputs. Each passenger also has an associated day with the boarding time. As the simulation runs for a week, when the day and the time(ticks) match the inputs, spawned passengers are moved to their start station. A passenger then waits for the arrival of a metro with the right destination and sufficient capacity. A passenger may need to switch lines depending on the shortest path, thus, when they check if the current station is the final destination. If it is, the passenger is ‘killed’ or else the passenger waits for the next metro. The passenger activities have been provided in the following figure.
![Figure2](images/Figure2.jpeg)

Time is modeled in discrete steps, and events occur at each unit of time(called tick, which is equivalent to 1 minute for the model). 



## 4. Design concepts

_<span style="text-decoration:underline;">Basic Principles</span>_

As the human population is increasing drastically in some major cities, there is a need to regulate traffic at different locations such as metros and metro-stations. How passengers travel within the city through metros, how the stations are properly connected with systematically aligned routes, time wasted at different stations waiting for the metros, how congestion problems can be solved at both metros and metro-stations is shown in the model. As passengers increase, congestion increases, and hence wait time increases as well. This issue is further compounded by similar time intervals of travel for different people. The working people have to travel to and from work, at specific times in the day. This creates an imbalance. To counter this issue, more and more metros need to be deployed for service. This model expands on this changing environment with respect to time on a weekly basis. For this model, decisions like boarding a metro for a passenger, or which path to take to reach a destination are taken on the level of a submodel. The model uses basic passenger logic for decisions, all other decisions are done based on rules set before the model is run.

_<span style="text-decoration:underline;">Emergence.</span>_

The final waiting time and congestion emerge from the passenger count and the number of metro trains. Changing the metro frequency or the number of passengers on a given day collectively impacts the observations. The stop time at each station for a metro is also a factor in this. For example, on increasing metro frequency during peak hours of traffic, congestion as well as wait time is reduced. This is supported by real world facts, and rules in the model.

_<span style="text-decoration:underline;">Adaptation. </span>_

A data driven and tool based approach minimizes the need for adaptation. The only adaptive property is that passengers find the fastest path to their destination for the input metro lines and distances. This decision is done using predefined rules only, considering time as the only metric for this decision.

_<span style="text-decoration:underline;">Objectives. </span>_

This is a vital aspect of the model for both the passengers and metros. The metro traverses its line with a given frequency and aims to complete its route. The passengers aim to reach their destination at the earliest given a start time, source and destination.

_<span style="text-decoration:underline;">Learning. </span>_

Metros and passengers do not change their actions based on past experiences in this model. They only follow a set of rules, and make decisions. 

_<span style="text-decoration:underline;">Prediction</span>_

Metros and passengers do not attempt to predict future outcomes and state of the model, they only make decisions based on current conditions and rules.

_<span style="text-decoration:underline;">Sensing</span>_

Metros keep track of their capacity and number of passengers currently in them. This variable ensures passengers can not board the metro when it is at max-capacity. The metros also use station links to traverse in their designated metro line colour. Metros determine when they have reached a station, and then determine which station they will go next. There is no effect of metros on each other, there may be multiple metros at the same location at any given time. Passengers keep track of which train they are alighting, and at which station they need to get out of the train. These variables are known by each agent themselves.

_<span style="text-decoration:underline;">Interaction. </span>_

The passenger and metro interaction is the most commonly occurring part of the model. They directly interact to check capacity, upcoming station, final stop etc. Passengers also directly interact with the stations for spawning at their start point and ensuring it’s on the right path to its destination. Similarly, the metro interacts with stations to wait for passengers boarding and alighting the metro. These interactions are modelled at each tick(minute) such that a passenger waits and boards when the right metro stops at their station.

<span style="text-decoration:underline;">Stochasticity</span>

The minimal stochasticity is a result of the data-based model implementation. Randomness is involved in choosing which passengers board the metro and in what order. This is especially important when the metro is at full-capacity midway through the boarding. Other aspects of the tool are based on the given user input data.

<span style="text-decoration:underline;">Collectives</span>

Passengers, metros and stations do not form any collective. Each of them have their own individual properties defining their behaviour but they have common interactions and responses.

<span style="text-decoration:underline;">Observation</span>

The observations from the model are Metro Congestion, Station Congestion, Passenger Wait Time and Passenger Total Travel time. The metro congestion is a result of the number of passengers in a metro at each tick. The station congestion is a result of the number of passengers at a station at each tick. The travelling passenger count depends on the data and the occurrence of the metros and the passengers, but it is capped at the given input metro capacity. The passenger wait time metric is for all passengers at any station. It shows the number of ticks a passenger is not in a metro and is waiting for one. The passenger total travel time metric is for all passengers at any time.  It shows the number of ticks a passenger has been travelling for without reaching their destination.



## 5. Initialization

The initialization state relies entirely on the user inputs and data files. At the first step, i.e. when t=0, the model has the stations and the colored links representing metro lines. All the metros are spawned but they are hidden as they will be moved to their starting stations at their corresponding start time. Similarly the passengers with spawn times under 2 hours are spawned but hidden. All state variables except the paths are set to 0 on initialization. This step is consistent, which is to say that for a given set of input data files, the model will have the same values and states every time. All the properties for metros and passengers were taken directly from the user, via csv files.


## 6. Input data

A tool based approach results in data playing an important role in the model. It initialises and directs major aspects of the simulation. We designed the model to take data in “csv” format as input. The setup requires the user to provide the position i.e. coordinates of the stations and their names. The links between stations, the colour of the line is also to be provided. Another file must have the data for metros with the start time, capacity and path defined for each metro train. The final data file is for spawning passengers at a given day of the week, time of the day, start station and destination station. The user can create their own metro network consisting of stations, the links between stations and the metros for the network. This configuration set by the user can also be saved to csv files for later use as direct input.


## 7. Submodels

For movement of a metro, it must keep in mind the duration it must reach the next station in and must move along a given path on a definite speed to do so. The train starts at the source, and moves along each station in some fixed time at a fixed speed. After reaching the destination station at the end of its path, the train reverses and goes back to the source, once again following the same speed and time pattern. This goes on until it is time for the train to end because of its end time from user input. The train finishes its journey, drops all the passengers to their expected stations, and dies. Similarly, for movement of a passenger, it is necessary to match the speed and direction (heading) of the metro the passenger is sitting in. It should stop at the stations the metro stops, and wait for the metro to start again after completing its station stop time. No passenger should alight from the train at that time. All the metrics, like speed and duration, were provided by the user.

# Execution Instructions
To run the model, one image and four "csv" files must be provided, namely "metros.csv", "stations.csv", "passengers.csv" and "links.csv". Then click "setup" followed by "load-config" to load the given input data. The "display-setup" is to toggle the simulation view. This is so that heavy or large data can be modelled in headless mode. The required format for each csv file is given below:

* metros.csv: colour, start time, end time, no. of stations in path, [path (one station in each column)]
* stations.csv: id, x-coordinate, y-coordinate, station name
* passengers.csv: source station, destination station, start time, day no. (1-7 for Monday to Sunday)
* links.csv: station A, station B, time between A and B, colour

For manually initializing stations, lines and metros:
To create stations, the user should select the add-station button and click at all positions they want on the screen to set the station at. Before clicking, the station name should be set for that particular station. By default, the color of stations is black. 
To create links between stations, the user should select the add-link button, select the link color they want, set the duration-of-travel property of the link using the slider, and then click on the two stations they want to connect. 
To create create metros, the user should select the spawn-metro button, then set the capacity property of the metro using the metro-capacity slider, and then start clicking on the stations they want the metro to travel on. The order of clicking should be in the order the user wants to set the path in. Beginning from the source station, they should click on all stations in the path, and to end to path, the user should click on the destination station again. The metro is not visible just after this process, it will be visible after model starts execution. Other properties of metro like start time and end time should be set directly in the csv file to be created next.
After creating this network, the user should save their work using the save-config button. This button saves all the metros, stations, and station-links data into csv files for later use. The user can later access the network using the load-config button to display it on the environment after setup. 
Please note that the model can only run after load-config button. Direct network setup and model start cannot be done.
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

house ranch
false
0
Rectangle -7500403 true true 270 120 285 255
Rectangle -7500403 true true 15 180 270 255
Polygon -7500403 true true 0 180 300 180 240 135 60 135 0 180
Rectangle -16777216 true false 120 195 180 255
Line -7500403 true 150 195 150 255
Rectangle -16777216 true false 45 195 105 240
Rectangle -16777216 true false 195 195 255 240
Line -7500403 true 75 195 75 240
Line -7500403 true 225 195 225 240
Line -16777216 false 270 180 270 255
Line -16777216 false 0 180 300 180

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
NetLogo 6.1.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup
load-config</setup>
    <go>go-metro</go>
    <metric>psg-wait-time</metric>
    <enumeratedValueSet variable="line-color">
      <value value="&quot;orange&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="image-file-name">
      <value value="&quot;dcmap.jpg&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metro-color">
      <value value="&quot;red&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="metro-capacity">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="duration-of-travel">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="station-name">
      <value value="&quot;AT&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="station-stop-time" first="1" step="1" last="5"/>
    <enumeratedValueSet variable="station-metrics">
      <value value="8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="display-setup">
      <value value="false"/>
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
