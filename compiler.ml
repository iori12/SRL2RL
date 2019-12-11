open Syntax
(*let rec srlc b =
  let rec srlcc c (la,lb,lc,ld) =
    match c with
    | SCon(b1,b2) -> (srlcc b1 (la,lb,la^"0",la^"1"))
                     @(srlcc b2 (la^"0",la^"1",lc,ld))
                   
    | SStep(a) -> [RBlk2(Label lb,RFrom(Label la),a,RGoto(Label lc))]
                  @[RBlk1(Label lc,RFrom(Label lb),RGoto(Label ld))]
                
    | SIf(e1,b1,b2,e2) ->
       [RBlk1(Label lb,RFrom(Label la),RIf(e1,Label (la^"2"),Label (la^"4")))]
       @(srlcc b1 (lb,la^"2",la^"3",lc))
       @(srlcc b2 (lb,la^"4",la^"5",lc))
       @[RBlk1(Label lc,RFi(e2,Label (la^"3"),Label (la^"5")),RGoto(Label ld))]
      
    | SFrom(e1,b1,b2,e2) ->
       [RBlk1(Label lb,RFi(e1,Label la,Label (la^"9")),RGoto(Label (la^"6")))]
       @(srlcc b1 (lb,la^"6",la^"7",lc))
       @(srlcc b2 (lc,la^"8",la^"9",lb))
       @[RBlk1(Label lc,RFrom(Label (la^"7")),RIf(e2,Label ld,Label (la^"8")))]
  in
  RLBLK([RBlk1(Label "0",REntry,RGoto(Label "1"))]
      
        @(srlcc b ("0","1","2","3"))
        @[RBlk1(Label "3",RFrom(Label "2"),RExit)])
 *)

let rec srlc b =
  let max2 a b = if (int_of_string a)>(int_of_string b) then a else b
  in
  let max4 a b c d = if (max2 a b) > (max2 c d) then (max2 a b) else (max2 c d)
  in
  let pl n x = string_of_int((int_of_string x) + (int_of_string n))
  in
  let rec srlcc c (la,lb,lc,ld) =
    let x = max4 la lb lc ld
    in
    match c with
    | SCon(b1,b2) -> (srlcc b1 (la,lb,pl x la,pl x lb))
                     @(srlcc b2 (pl x la,pl x lb,lc,ld))
                   
    | SStep(a) -> [RBlk2(Label lb,RFrom(Label la),a,RGoto(Label lc))]
                  @[RBlk1(Label lc,RFrom(Label lb),RGoto(Label ld))]
                
    | SIf(e1,b1,b2,e2) ->
       [RBlk1(Label lb,RFrom(Label la),RIf(e1,Label (pl x la),Label (pl x lc)))]
       @(srlcc b1 (lb,pl x la,pl x lb,lc))
       @(srlcc b2 (lb,pl x lc,pl x ld,lc))
       @[RBlk1(Label lc,RFi(e2,Label (pl x lb),Label (pl x ld)),RGoto(Label ld))]
      
    | SFrom(e1,b1,b2,e2) ->
       [RBlk1(Label lb,RFi(e1,Label la,Label (pl x ld)),RGoto(Label (pl x la)))]
       @(srlcc b1 (lb,pl x la,pl x lb,lc))
       @(srlcc b2 (lc,pl x lc,pl x ld,lb))
       @[RBlk1(Label lc,RFrom(Label (pl x lb)),RIf(e2,Label ld,Label (pl x lc)))]
  in
  RLBLK([RBlk1(Label "1",REntry,RGoto(Label "2"))]
        @(srlcc b ("1","2","3","4"))
        @[RBlk1(Label "4",RFrom(Label "3"),RExit)])
 
   
