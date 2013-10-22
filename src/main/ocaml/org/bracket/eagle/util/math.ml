module Logic = struct
    let int_of_bool a = if a then 1 else 0
    let bool_of_int a = a = 1
    let iob = int_of_bool
    let boi = bool_of_int
    let (&) a b = boi (iob a land iob b)
    let (|+) a b = boi (iob a lor iob b) (* sigh... why did | had to have a
    special meaning... oh well.*)
    let (&=) a b = a := !a & b
    let (|=) a b = a := !a |+ b
end

module Universe = struct
    let ( +=  ) a b = a := !a + b; !a
    let ( +.= ) a b = a := !a +. b; !a
    let ( -=  ) a b = a := !a - b; !a
    let ( -.= ) a b = a := !a -. b; !a
    let ( *=  ) a b = a := !a * b; !a
    let ( *.= ) a b = a := !a *. b; !a
    let ( /=  ) a b = a := !a / b; !a
    let ( /.= ) a b = a := !a /. b; !a
    let ( ~++ ) a = a += 1
    let (  ++ ) = (~++)
    let ( ~-- ) a = a -= 1
    let (  -- ) = (~--)
end

module Geometry = struct
    let pi = 4.0 *. atan 1.0
    let half_pi = 2.0 *. atan 1.0
    let quarter_pi = 2.0 *. atan 1.0
end
