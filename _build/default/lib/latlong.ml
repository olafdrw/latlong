(* Preamble -- some math, and an "angle" type which might be part of a common library. *)
let pi = 4. *. atan 1.
let radians_of_degrees = ( *. ) (pi /. 180.)
let haversin theta = 0.5 *. (1. -. cos theta)
 
(* The angle type can track radians or degrees, which I'll use for automatic conversion. *)
type angle = Deg of float | Rad of float
let as_radians = function
  | Deg d -> radians_of_degrees d
  | Rad r -> r
 
(* Demonstrating use of a module, and record type. *)
module LatLong = struct
  type t = { lat: float; lng: float }
  let of_angles lat lng = { lat = as_radians lat; lng = as_radians lng }
  let sub a b = { lat = a.lat-.b.lat; lng = a.lng-.b.lng }
 
  let dist radius a b =
    let d = sub b a in
    let h = haversin d.lat +. haversin d.lng *. cos a.lat *. cos b.lat in
    2. *. radius *. asin (sqrt h)
end