; to handle jokes/mistakes, take the num of recs for that passion in that dst over
; total num of recs for that dst. if below a threshold, consider it a joke

; hidden gems

; group passions together, meaning that if someone searches for one passion in
; that group, they may be interested in the others too
; eg: musuems, monuments, architecture, and sightseeing might be a group,
; culture, old town, history, art is another group; walking, city walks

; alternatively, keep track of passions that are searched together (either in the
; same query, or by the same user in sequential queries)

; alternatively again, calculate distance between cities (not physical, but how
; similar they are by the number of passions they share, and how endorsed those
; passions are for each city), call it the passion_distance
; although this will result in (n-1) * (n-2) * ... * 2 * 1 = (n-1)! passion_distances

; cluster cities. if one city is good for a passion, recommend its neighbors as
; well

; avg endorsments/city = num cities endorsed for that passion / num endorsments for that passion
; if a city's endorsment for that passion >= avg, it ranks high for that passion

; recommend based on number of endoresments for that passion in that place / total num
; endorsements for that place
