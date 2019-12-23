open Syntax
let opt_rl1 =
let rec opt rl c =
  match rl with
  | [] -> (-1,-1)
  | x::xs ->
     begin
       match x with
       | RBlk(Label(n),f,s,j) ->
          begin
            let a = try int_of_string n with
                    | _-> -1
            in
            if (c = a) || (a = -1) then opt xs (c+1)
            else (int_of_string n,c)
            end
     end
in
function
| RLBLK(rl) -> opt rl 1
             
let opt3 rl n =
let rec opt2 rl (c1,c2) =
  let opt_l l (c1,c2) =
    match l with
    | Label(n)->
       let a = try int_of_string n with
               | _-> -1
       in
       if a = c1 then Label("l"^(string_of_int c2)) else Label(n)
  in
  let opt_f f (c1,c2) =
    match f with
    | RFrom(l) -> RFrom(opt_l l (c1,c2))
    | RFi(e,l1,l2) -> RFi(e, opt_l l1 (c1,c2), opt_l l2 (c1,c2))
    | REntry -> REntry
  in
  let opt_j j (c1,c2) =
    match j with
    | RGoto(l) -> RGoto(opt_l l (c1,c2))
    | RIf(e,l1,l2) -> RIf(e, opt_l l1 (c1,c2), opt_l l2 (c1,c2))
    | RExit -> RExit
  in
  match rl with
  | [] -> []
  | x::xs ->
     begin
       match x with
       | RBlk(l,f,s,j) ->
          [RBlk(opt_l l (c1,c2), opt_f f (c1,c2), s, opt_j j (c1,c2))]
          @ (opt2 xs (c1,c2))
     end
in
match rl with
| RLBLK(x) -> RLBLK(opt2 x n)
            
let rec opt_rl rl =
  let x = opt_rl1 rl
  in
  if x = (-1,-1) then rl
  else opt_rl(opt3 rl x)

              (*ラベルにlをつける関数*)
let label_rl =
  let rec label_rl2 =
    let label_l = function
      | Label(n) ->
         begin
           let a = try int_of_string n with
                   | _-> -1
           in
           if a = -1 then Label(n) else Label("l"^n)
         end
    in
    let label_f = function
      | RFrom(l) -> RFrom(label_l l)
      | RFi(e,l1,l2) -> RFi(e, label_l l1, label_l l2)
      | REntry -> REntry
    in
    let label_j = function
    | RGoto(l) -> RGoto(label_l l)
    | RIf(e,l1,l2) -> RIf(e, label_l l1, label_l l2)
    | RExit -> RExit
    in
    function
    | [] -> []
    | x::xs ->
       begin
         match x with
         | RBlk(l,f,s,j) ->
            [RBlk(label_l l, label_f f, s, label_j j)]
            @ (label_rl2 xs)
       end
  in
  function
  | RLBLK(x) -> RLBLK(label_rl2 x)
