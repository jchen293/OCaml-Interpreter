type const = I of int | B of string | E of string | S of string | N of string | P of string 
type dataType = Pushs of const| Pushi of const| Pushb of const| Pushn of const | Push of const |Add| Sub | Mul |
                Div | Rem | Neg | Swap | Pop  | Cat| And| Or| Not| Equal| LessThan| Bind| If| Let| End |Quit;;

let interpreter ( (input : string), (output : string )) : unit =

  let invalidList = ['~';'!';'@';'#';'$';'%';'^';'&';'*';'(';')';'-';'+';'=';'\\';'|';'}';']';'{';'[';':';';';'\"';'<';'>';'/';'?';'\ ';',';'.'] in
  let ic = open_in input in

  let rec loop_read acc =
    try 

      let l = input_line ic in 
      loop_read (l::acc)
    with

    | End_of_file -> List.rev acc in
  (* let oc = open_out output in 
  let file_write string_val = Printf.fprintf oc "%s\n" string_val in *)

  let ls_str = loop_read [] in 

  let rec convertFunction p: string list list  = 

    match p with 
      [] -> [[]]
    | hd::tl ->  (String.split_on_char ' ' hd) :: convertFunction(tl)
  in

  let listOfList = convertFunction ls_str in 

  let strip_both_chars str =
    match String.length str with
    | 0 | 1 | 2 -> ""
    | len -> String.sub str 1 (len - 2)
  in
  let strip_last_char str =
    if str = "" then "" else
      String.sub str 0 ((String.length str) - 1)
  in

  let rec listToString2 (p:string list) (y:string list): string list =

    match p with
      [] -> y
    |hd::tl -> (match hd with
          "pushs" -> listToString2 tl y
        | "pushn" -> listToString2 tl y
        |_-> listToString2 tl (hd::y)
      )

  in

  let rec listToString3 (p:string list) (y:string) : string =

    match p with
      []->y
    |hd::tl -> listToString3 tl (hd^" "^y)
  in

  let substringCheck s1 s2 =
    try
      let len = String.length s2 in
      for i = 0 to String.length s1 - len do
        if String.sub s1 i len = s2 then raise Exit
      done;
      false
    with Exit -> true
  in

  let rec validCheck (x:string) (y:int): bool=

  if y = String.length(x) then true
  else if String.get x y = '_' then
  validCheck x (y+1)
  else if String.get x y = 'a' || String.get x y = 'A' || 
  String.get x y = 'b' || String.get x y = 'B'
  || String.get x y = 'c'|| String.get x y = 'C'
  || String.get x y = 'd'|| String.get x y = 'D'
  || String.get x y = 'e'|| String.get x y = 'E'
  || String.get x y = 'f'|| String.get x y = 'F'
  || String.get x y = 'g'|| String.get x y = 'G'
  || String.get x y = 'h'|| String.get x y = 'H'
  || String.get x y = 'i'|| String.get x y = 'I'
  || String.get x y = 'j'|| String.get x y = 'J'
  || String.get x y = 'k'|| String.get x y = 'K'
  || String.get x y = 'l'|| String.get x y = 'L'
  || String.get x y = 'm'|| String.get x y = 'M'
  || String.get x y = 'n'|| String.get x y = 'N'
  || String.get x y = 'o'|| String.get x y = 'O'
  || String.get x y = 'p'|| String.get x y = 'P'
  || String.get x y = 'q'|| String.get x y = 'Q'
  || String.get x y = 'r'|| String.get x y = 'R'
  || String.get x y = 's'|| String.get x y = 'S'
  || String.get x y = 't'|| String.get x y = 'T'
  || String.get x y = 'u'|| String.get x y = 'U'
  || String.get x y = 'v'|| String.get x y = 'V'
  || String.get x y = 'w'|| String.get x y = 'W'
  || String.get x y = 'x'|| String.get x y = 'X'
  || String.get x y = 'y'|| String.get x y = 'Y'
  || String.get x y = 'z'|| String.get x y = 'Z'
   then true
   else false
  in

 let rec invalidCharCheck (x:string): bool list=
     List.map (fun ch -> String.contains x ch) invalidList

     in
 let rec invalidCharCheck2 (p:bool list): bool =

 match p with
 []-> false
 |hd::tl -> (match hd with
  true->true
  |false->invalidCharCheck2 tl
 )
 in

  let rec convertType (p: string list list): dataType list =
    match p with 
      [[]] -> []
    | hd::tl -> (match hd with 

        | hd2::tl2::tl3 -> (match hd2 with
              "pushi" -> (try 
                            (Pushi (I (int_of_string tl2))::convertType tl) 
                          with Failure _ -> Pushi (E (":error:"))::convertType tl)
            |"pushn" ->  
               (if not (validCheck(strip_last_char(listToString3(listToString2 hd [])"")) 0) then Pushn (E (":error:"))::convertType tl 
               else if invalidCharCheck2(invalidCharCheck(strip_last_char(listToString3(listToString2 hd [])""))) then Pushn (E (":error:"))::convertType tl
               else Pushn (N (tl2))::convertType tl)

            |"pushs" -> 
            let lengthOfString = String.length((listToString3(listToString2 hd [])""))-2 in
            (if not ((String.get((strip_last_char(listToString3(listToString2 hd [])"")))0) ='\"') then Pushs (E (":error:"))::convertType tl 
            else if not ((String.get((strip_last_char(listToString3(listToString2 hd [])"")))lengthOfString) ='\"') then Pushs (E (":error:"))::convertType tl 
            else Pushs (S (strip_last_char(strip_both_chars(listToString3(listToString2 hd [])""))))::convertType tl)
            |"pushb" -> (if (String.get tl2 0 =':' && String.get tl2 ((String.length tl2)-1) =':' 
                             && (String.get tl2 1 ='t' || String.get tl2 1 ='f') && 
                             (String.get tl2 ((String.length tl2)-2) ='e' )&&
                             (substringCheck tl2 "true" = true || substringCheck tl2 "false" = true)) then Pushb (B (tl2))::convertType tl
                         else Pushb (E (":error:"))::convertType tl )

            |"push"  -> (if (String.get tl2 0 =':' && String.get tl2 ((String.length tl2)-1) =':' 
                             && (String.get tl2 1 ='u') && 
                             (String.get tl2 ((String.length tl2)-2) ='t')&&
                             (substringCheck tl2 "unit" = true)) then Pushb (P (tl2))::convertType tl
                         else Pushb (E (":error:"))::convertType tl )

            |_-> []
          )
        |hd3::tl3 -> (match hd3 with
              "pop" -> Pop ::convertType tl
            | "add" -> Add ::convertType tl
            | "swap"-> Swap::convertType tl
            | "mul" -> Mul::convertType tl
            | "div" -> Div::convertType tl
            | "rem" -> Rem::convertType tl
            | "neg" -> Neg::convertType tl
            | "sub" -> Sub::convertType tl
            | "quit" -> Quit::convertType tl
            | "cat" -> Cat::convertType tl
            | "and" -> And::convertType tl
            | "or" -> Or::convertType tl
            | "not" -> Not::convertType tl
            | "equal" -> Equal::convertType tl
            | "lessThan" ->LessThan::convertType tl
            | "bind" -> Bind::convertType tl
            | "if" -> If::convertType tl
            | "let" -> Let::convertType tl
            | "end" -> End::convertType tl
            |_->[]
          )
        |_->[]
      )
    |_-> []

in

  let inputList = convertType listOfList in


  let rec bindFunction (a:(string * const) list) (b:string) (c:string) (d:'a list): 'a list =

   match a with
   hd::tl -> (match hd with
   (ht,tl2)-> 
   if ht = b || ht =c then  bindFunction tl b c (tl2::d) 
   else bindFunction tl b c d   
   )
  |[]-> d

  in

  let rec bindFunction2 (a:(string * const) list) (b:string): const =

  match a with
  hd::tl -> (match hd with
  (ht,tl2)-> if ht = b then tl2 else bindFunction2 tl b )
  |[]-> E(":error:")

  in

  let rec boolFunction (a:(string*const) list) (b:string) : string =

   match a with 
   hd::tl -> (match hd with
   (ht,tl2)-> (match ht,tl2 with

   ht, B a -> if ht = b && (a =":true:" || a =":false:") then a 
   else boolFunction tl b
   |_-> boolFunction tl b
   ))
   |[]->""

  in

  let rec remFunction (a:(string*const) list) (b:string) : int =

  match a with 
  hd::tl -> (match hd with
  (ht,tl2) -> (match ht,tl2 with
  ht,I a-> if ht = b then a else remFunction tl b
  |_-> remFunction tl b
  ))
  |[]-> 0
  in

  let rec checkDup (a:string) (b:(string * const) list): bool =

  match b with
  hd::tl -> (match hd with
  (ht,tl2)-> if ht = a then true else checkDup a tl
  )
  |[]-> false
  in

  let rec reboundFunction (a:string) (b:(string * const) list) : string = 

  match b with
  hd::tl -> (match hd with
  (ht, tl2) -> (match ht, tl2 with
  aa, S b -> if a = aa then b else reboundFunction a tl
  |aa, E b -> if a = aa then b else reboundFunction a tl
  |aa, P b -> if a = aa then b else reboundFunction a tl
  |aa, I b -> if a = aa then string_of_int b else reboundFunction a tl
  |aa, B b -> if a = aa then b else reboundFunction a tl
  |_-> reboundFunction a tl
  ))
  |[] -> ""
  in

  let rec overwriteFunction (a:string) (x:(string * const) list) (b:(string * const) list) (c:const): (string * const) list =

  match b with
  hd::tl -> (match hd with
  (ht,tl2)-> if ht = a then  overwriteFunction a ((ht,c)::x) tl c else overwriteFunction a ((ht,tl2)::x) tl c )
  |[]-> x
  in

  let rec overwriteFunction2 (a:string) (b:(string * const) list) (c:string): (string * const) list =

  match b with

  hd::tl -> (match hd with
  (ht,tl2)-> if ht = a then ((c,tl2)::b) else overwriteFunction2 a tl c
  )
  |[] -> b
  in

   let rec overwriteFunction3 (a:string) (x:string) (y:(string * const) list) (b:(string * const) list) (c:const): (string * const) list =

  match b with
  hd::tl -> (match hd with
  (ht,tl2)->  if ht = a then overwriteFunction3 a x ((a,c)::y) tl c else overwriteFunction3 a x ((ht,tl2)::y) tl c 
  )
  |[]-> y
  in

  let rec combineValue (a:string) (b:(string * const) list) (c:int): int =

   match b with
   hd::tl -> (match hd with
   (ht,tl2)-> (match (ht,tl2) with
   (aa, I b) -> if aa = a then b+c else combineValue a tl c
   |_->combineValue a tl c
   ))
   |[]-> c
  in

  let rec minusValue (a:string) (b:(string * const) list): int =

  match b with
  hd::tl->(match hd with
  (ht,tl2) -> (match (ht,tl2)with
  (aa, I b) -> if aa = a then b else minusValue a tl
  |_-> minusValue a tl
  ))
  |[]-> 0
  in



  let rec printTuple (p:(string * const)list): string = 

  match p with
  hd::tl -> (match hd with
  (h,t) -> (match h,t with
   h, I a -> print_endline ("Type I: "^h^" value: "^string_of_int a); printTuple tl
  |h, S a -> print_endline ("Type S: "^h^" value: "^ a); printTuple tl
  |h, N a -> print_endline ("Type N: "^h^" value: "^ a); printTuple tl
  |h, P a -> print_endline ("Type P: "^h^" value: "^ a); printTuple tl
  |h, B a -> print_endline ("Type B: "^h^" value: "^ a); printTuple tl
  |_-> printTuple tl 
  ))
  |[]-> " "

  in


  let rec mainFunction2 (x:dataType list) (y:const list) (z:(string * const) list) : (const option)*(dataType list)=
    match x with
      [] ->  (None,x)
| hd::tl -> (match hd with 
          Pushi i-> mainFunction2 tl (i::y) z
        | Pushb b-> mainFunction2 tl (b::y) z
        | Pushn n -> mainFunction2 tl (n::y) z
        | Pushs s-> mainFunction2 tl (s::y) z
        | Push  p-> (match p with 
          P a -> mainFunction2 tl (P(a)::y) z
          |_-> mainFunction2 tl (E(":error:")::y) z
        )    
        | Pop -> (match y with 
        | [] -> mainFunction2 tl (E(":error:")::y) z
        | hd2::tl222 ->  mainFunction2 tl tl222 z
          )
        | Add ->  
           (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              (N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd44::hd55::tl55 -> (match(hd44,hd55) with
              (I a, I b) -> mainFunction2 tl ((I (a+b))::tl4) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |_-> mainFunction2 tl (E(":error:")::y) z
              ) else if List.length(temp) = 1 &&x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              I a -> mainFunction2 tl (I(a + a)::tl4) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              else (match (hd3,tl3) with
               (I a, I b) ->mainFunction2 tl ((I (a+b))::tl4) z
              | _->  mainFunction2 tl ((E(":error:"))::y) z
                  ) 
              | I a, N b -> if checkDup b z && minusValue b z !=0 then (mainFunction2 tl (I(combineValue b z a)::tl4)z)
                            else mainFunction2 tl ((E(":error:"))::y) z
              | N a, I b -> if checkDup a z && minusValue a z !=0 then (mainFunction2 tl (I(combineValue a z b)::tl4)z)
                            else mainFunction2 tl ((E(":error:")::y))z
              | I a, I b ->mainFunction2 tl ((I (a+b))::tl4) z
              | _->  mainFunction2 tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction2 tl (E(":error:")::y) z
            |_-> mainFunction2 tl (E(":error:")::y) z
          )
        | Sub -> 
              (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              (N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match (minusValue x z), (minusValue y2 z) with
              a, b -> if a != 0 && b!=0 then mainFunction2 tl(I(b-a)::tl4) z else mainFunction2 tl (E(":error:")::y) z
              ) else if List.length(temp) = 1 &&x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              I a -> mainFunction2 tl (I(a - a)::tl4) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              else (match (hd3,tl3) with
               (I a, I b) ->mainFunction2 tl ((I (b-a))::tl4) z
              | _->  mainFunction2 tl ((E(":error:"))::y) z) 
              | I a, N b -> if checkDup b z && minusValue b z != 0 then (mainFunction2 tl (I((minusValue b z)-a)::tl4)z)
                            else mainFunction2 tl ((E(":error:"))::y) z
              | N a, I b -> if checkDup a z && minusValue a z != 0 then (mainFunction2 tl (I(b-(minusValue a z))::tl4)z)
                            else mainFunction2 tl ((E(":error:"))::y) z
              | I a, I b ->mainFunction2 tl ((I (b-a))::tl4) z
              | _->  mainFunction2 tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction2 tl (E(":error:")::y) z
            |_-> mainFunction2 tl (E(":error:")::y) z
          )
        | Mul -> (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              (N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd44::hd55::tl55 -> (match(hd44,hd55) with
              (I a, I b) -> mainFunction2 tl ((I (a*b))::tl4) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |_-> mainFunction2 tl (E(":error:")::y) z
              ) else if List.length(temp) = 1 && x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              I a -> mainFunction2 tl (I(a * a)::tl4) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              else (match (hd3,tl3) with
               (I a, I b) ->mainFunction2 tl ((I (a*b))::tl4) z
              | _->  mainFunction2 tl ((E(":error:"))::y) z
                  ) 
              | I a, N b -> if checkDup b z && minusValue b z !=0 then (mainFunction2 tl (I((minusValue b z)*a)::tl4)z)
                            else mainFunction2 tl ((E(":error:"))::y) z
              | N a, I b -> if checkDup a z && minusValue a z !=0 then (mainFunction2 tl (I((minusValue a z)*b)::tl4)z)
                            else mainFunction2 tl ((E(":error:"))::y) z
              | I a, I b ->mainFunction2 tl ((I (a*b))::tl4) z
              | _->  mainFunction2 tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction2 tl (E(":error:")::y) z
            |_-> mainFunction2 tl (E(":error:")::y) z
          )
        | Div -> 
        (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              (N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match minusValue x z, minusValue y2 z with
              a, b ->( try mainFunction2 tl ((I (b/a))::tl4) z with Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)
              ) else if List.length(temp) = 1 && x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              I a -> mainFunction2 tl (I(a / a)::tl4) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |_-> mainFunction2 tl (E(":error:")::y) z
              ) else (match (hd3,tl3) with
               (I a, I b) ->( try mainFunction2 tl ((I (b/a))::tl4) z with Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)
              | _->  mainFunction2 tl ((E(":error:"))::y) z) 
              | I a, N b -> if checkDup b z && minusValue b z !=0 then (try (mainFunction2 tl (I((minusValue b z)/a)::tl4)z) with Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)
                            else mainFunction2 tl ((E(":error:"))::y) z
              | N a, I b -> if checkDup a z && minusValue a z !=0 then (try (mainFunction2 tl (I(b/(minusValue a z))::tl4)z) with Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)
                            else mainFunction2 tl ((E(":error:"))::y) z
              | I a, I b ->( try mainFunction2 tl ((I (b/a))::tl4) z with Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)
              | _->  mainFunction2 tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction2 tl (E(":error:")::y) z
            |_-> mainFunction2 tl (E(":error:")::y) z
          )
        | Rem -> 
        (match y with 
              hd31::tl31::tl41-> (match hd31,tl31 with
                  I a, I b -> (try mainFunction2 tl ((I (b mod a))::tl41) z with 
                      Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)
                | I a, N b -> if checkDup b z && minusValue b z !=0 then (try mainFunction2 tl ((I ((remFunction z b) mod a))::tl41) z with 
                      Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)
                      else mainFunction2 tl ((E(":error:")::y)) z
                | N a, I b -> if checkDup a z && minusValue a z !=0 then (try mainFunction2 tl ((I (b mod (remFunction z a)))::tl41) z with 
                      Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)
                      else mainFunction2 tl ((E(":error:")::y)) z
                | N a, N b -> if List.length(bindFunction z a b []) = 2 then (match minusValue a z, minusValue b z with
                  a, b -> (try mainFunction2 tl ((I (b mod a))::tl41) z with 
                      Division_by_zero -> mainFunction2 tl ((E(":error:"))::y) z)) 
                      else if List.length(bindFunction z a b []) = 1 && a = b then 
                      mainFunction2 tl(I(0)::tl41) z
                      else mainFunction2 tl ((E(":error:"))::y) z
                | _-> mainFunction2 tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction2 tl (E(":error:")::y) z
            |_->mainFunction2 tl (E(":error:")::y) z
          )
        | Neg -> (match y with
              hd34::tl34 -> (match hd34 with
                  N a -> if (checkDup a z) then let temp = (bindFunction2 z a) in 
                  (match temp with
                  I hd -> mainFunction2 tl (I(hd * -1)::tl34) z
                  |_-> mainFunction2 tl ((E(":error:"))::y) z)
                  else mainFunction2 tl ((E(":error:"))::y) z 
                |I a -> mainFunction2 tl (I(a * -1)::tl34) z
                | _-> mainFunction2 tl ((E(":error:"))::y) z
              )
            |_->mainFunction2 tl (E(":error:")::y) z
          )
        | Swap -> (match y with
              h::h2::t-> mainFunction2 tl (h2::h::t) z
            |h3::t2-> mainFunction2 tl (E(":error:")::y) z
            |_-> mainFunction2 tl (E(":error:")::y) z
          )

        | Cat -> 
              (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              N x, N y2 -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd::hd2::tl5 -> (match hd, hd2 with
               S a, S b -> mainFunction2 tl ((S (a^"" ^b))::tl4) z
                 |_->mainFunction2 tl ((E(":error:"))::y) z
              )
              |_->mainFunction2 tl ((E(":error:"))::y) z
              ) else (match (hd3,tl3) with
               S a, S b ->mainFunction2 tl ((S (a^"" ^b))::tl4) z
              | _->  mainFunction2 tl ((E(":error:"))::y) z
                  ) 
              | S a, S b -> mainFunction2 tl (S (b^""^a)::tl4) z
              | N a, S b -> (match bindFunction2 z a, b with
               S a, b -> mainFunction2 tl (S (b^""^a)::tl4) z
               |_->mainFunction2 tl ((E(":error:"))::y) z
              )
              | S a, N b -> (match a, bindFunction2 z b with
               a, S b -> mainFunction2 tl (S(b^""^a)::tl4) z
               |_-> mainFunction2 tl ((E(":error:"))::y) z
              )
              | _->  mainFunction2 tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction2 tl (E(":error:")::y) z
            |_-> mainFunction2 tl (E(":error:")::y) z
          )
        | And -> 
         (match y with 
              hd3::tl3::t-> (match (hd3,tl3) with
              N a, B b -> let temp2 = (bindFunction2 z a) in
              (match temp2 with
              B a -> if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |B a, N b -> let temp2 = (bindFunction2 z b) in
              (match temp2 with
              B b -> if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |B a, B b -> if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z

              |(N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd44::hd55::tl55 -> (match(hd44,hd55) with
              (B a, B b) -> if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |(S a, B b) ->if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |(B a, S b) ->if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |_-> mainFunction2 tl y z) 
              else if  List.length(temp) = 1 &&x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              B a ->  mainFunction2 tl (B(":true:")::t) z 
              |_-> mainFunction2 tl (E(":error:")::t) z
              )
              |_-> mainFunction2 tl (E(":error:")::t) z
              ) 
               else (match y with
              h::h2::tt -> (match h,h2 with 
              B a, B b -> if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |_-> mainFunction2 tl (E(":error:")::y) z
             )
             |h3::t2-> mainFunction2 tl (E(":error:")::y) z
            |_-> mainFunction2 tl (E(":error:")::y) z
        ) 
        |_-> mainFunction2 tl (E(":error:")::y) z
          )
          |_-> mainFunction2 tl (E(":error:")::y) z)

        | Or -> 
        (match y with 
              hd3::tl3::t-> (match (hd3,tl3) with

             B a, B b -> if a = ":true:" && b =":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":false:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z


            |N a, B b -> let temp2 = (bindFunction2 z a) in
              (match temp2 with
              B a -> if a = ":true:" && b =":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":false:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )

            |B a, N b -> 
            let temp2 = (bindFunction2 z b) in
              (match temp2 with
              B b -> if a = ":true:" && b =":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":false:" then mainFunction2 tl (B(":false:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":false:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |N x, N y2 -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd44::hd55::tl55 -> (match(hd44,hd55) with
              B a, B b -> if a = ":true:" && b =":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |(S a, B b) -> if a = ":true:" && b =":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |(B a, S b) -> if a = ":true:" && b =":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |_-> mainFunction2 tl (E(":error:")::y) z
              )
              |_-> mainFunction2 tl y z) else if List.length(temp) = 1 &&x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              B a -> if a = ":false:" then mainFunction2 tl (B(":false:")::t) z else mainFunction2 tl (B(":true:")::t) z
              |_-> mainFunction2 tl (E(":error:")::t) z
              )
              |_-> mainFunction2 tl (E(":error:")::t) z
              ) else (match y with
             h::h2::tt -> (match h,h2 with 
             B a, B b -> if a = ":true:" && b =":false:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction2 tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction2 tl (B(":false:")::t)  z
              else mainFunction2 tl (E(":error:")::y) z
              |_-> mainFunction2 tl (E(":error:")::y) z
             )
             |h3::t2-> mainFunction2 tl (E(":error:")::y) z
            |_-> mainFunction2 tl (E(":error:")::y) z
        ) 
        |_-> mainFunction2 tl (E(":error:")::y) z
          )
          |_-> mainFunction2 tl (E(":error:")::y) z)

        | Not -> (match y with
        h::t -> (match h with
        B a -> if a =":true:" then mainFunction2 tl (B(":false:")::t) z
        else if a =":false:" then mainFunction2 tl (B (":true:")::t)  z
        else mainFunction2 tl (E (":error:")::y)  z
        | N a -> if boolFunction z a = ":true:" then mainFunction2 tl (B(":false:")::t) z
        else if boolFunction z a =":false:" then mainFunction2 tl (B (":true:")::t)  z
        else mainFunction2 tl (E (":error:")::y)  z
        |_-> mainFunction2 tl (E(":error:")::y)  z
        )
        |_-> mainFunction2 tl (E(":error:")::y)  z
        )
        | Equal -> (match y with
        hd::hd2::t -> (match hd, hd2 with
        I a, I b -> if a = b then mainFunction2 tl (B (":true:")::t) z else mainFunction2 tl (B(":false:")::t) z
        |N a, N b -> if List.length(bindFunction z a b []) = 2 then (match bindFunction z a b [] with
        hd::hd2::tt -> (match hd, hd2 with
        I a, I b -> if a = b then mainFunction2 tl (B (":true:")::t) z else mainFunction2 tl (B(":false:")::t) z
        |_->mainFunction2 tl (E(":error:")::y) z
        )
        |_->mainFunction2 tl (E(":error:")::y) z
        )else mainFunction2 tl (B(":false:")::t) z
        |N a, I b -> (match (bindFunction2 z a), b with
        I a, b -> if a = b then mainFunction2 tl (B (":true:")::t) z  
        else  mainFunction2 tl (B (":false:")::t) z
        |_->mainFunction2 tl (E(":error:")::y) z
        )
        |I a, N b -> (match (bindFunction2 z b), a with
        I a, b -> if a = b then mainFunction2 tl (B (":true:")::t) z  
        else  mainFunction2 tl (B (":false:")::t) z
        |_->mainFunction2 tl (E(":error:")::y) z
        )
        |_->mainFunction2 tl (E(":error:")::y) z

        )
        |_->mainFunction2 tl (E(":error:")::y) z
        )
        | LessThan -> (match y with
        hd::hd2::t -> (match hd,hd2 with
        I a, I b -> if a > b then mainFunction2 tl (B (":true:")::t) z else mainFunction2 tl (B (":false:")::t) z
        |N aa, N bb -> if List.length(bindFunction z aa bb []) = 2 then (match (bindFunction2 z aa),(bindFunction2 z bb) with
        I a, I b -> if a > b then mainFunction2 tl (B (":true:")::t) z  
        else  mainFunction2 tl (B (":false:")::t) z
        |_-> mainFunction2 tl (E(":error:")::y) z
        ) else if List.length(bindFunction z aa bb []) = 1 && aa=bb then mainFunction2 tl (B (":false:")::t) z
         else mainFunction2 tl (E(":error:")::y) z
        |N a, I b -> (match (bindFunction2 z a), b with
        I a, b -> if a > b then mainFunction2 tl (B (":true:")::t) z  
        else  mainFunction2 tl (B (":false:")::t) z
        |_->mainFunction2 tl (E(":error:")::y) z
        )
        |I a, N b -> (match (bindFunction2 z b), a with
        I a, b -> if a < b then mainFunction2 tl (B (":true:")::t) z  
        else  mainFunction2 tl (B (":false:")::t) z
        |_->mainFunction2 tl (E(":error:")::y) z
        )
        |_->mainFunction2 tl (E(":error:")::y) z
        )
        |_->mainFunction2 tl (E(":error:")::y) z
        )
        | Bind -> (match y with
        hd::hd2::t -> (match hd, hd2 with

        I a, N b ->   if checkDup b z then mainFunction2 tl (S(":unit:")::t)(overwriteFunction b [] z hd)
                      else mainFunction2 tl (S (":unit:")::t) ((b,I(a))::z)
        | S a, N b -> if checkDup b z then mainFunction2 tl (S(":unit:")::t)(overwriteFunction b [] z hd) 
                      else mainFunction2 tl (S (":unit:")::t) ((b,S(a))::z)
        | N a, N b -> if checkDup b z && checkDup a z then mainFunction2 tl (S(":unit:")::t) (overwriteFunction3 b a [] z (S(reboundFunction a z)))
                      else if checkDup b z && checkDup a z = false then mainFunction2 tl (E(":error:")::y) z  
                      else if checkDup b z then mainFunction2 tl (S(":unit:")::t)(overwriteFunction2 b z a) 
                      else if bindFunction2 z a = E(":error:") then mainFunction2 tl (E(":error:")::y) z
                      else mainFunction2 tl (S (":unit:")::t) ((b,(bindFunction2 z a))::z) 
        | P a, N b -> if a=":error:" then mainFunction2 tl (E(":error:")::y) z  
                      else if checkDup b z then mainFunction2 tl (E(":unit:")::t)(overwriteFunction b [] z hd)
                      else mainFunction2 tl (S(":unit:")::t) ((b,P(a))::z)
        | B a, N b->  if checkDup b z then mainFunction2 tl (S(":unit:")::t)(overwriteFunction b [] z hd)
                      else mainFunction2 tl (S (":unit:")::t) ((b,B(a))::z)

        |_->mainFunction2 tl (E(":error:")::y) z
        )
        |_->mainFunction2 tl (E(":error:")::y) z
        )

        | If -> (match y with

        hd::hd2::hd3::t -> (match hd,hd2,hd3 with
        hd, hd2, B a -> (match hd, hd2 with

        N aa, N bb ->  if a = ":true:" then mainFunction2 tl (hd::t) z 

                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |I aa, N bb -> if a = ":true:" then mainFunction2 tl (hd::t) z 

                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |N aa, I bb ->  if a = ":true:" then mainFunction2 tl (hd::t) z 
                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |S aa, N bb -> if a = ":true:" then mainFunction2 tl (hd::t) z 

                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |N aa, S bb -> if a = ":true:" then mainFunction2 tl (hd::t) z 
                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |E aa, S bb -> if a = ":true:" then mainFunction2 tl (hd::t) z
                       else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |S aa, E bb -> if a =":true:" then mainFunction2 tl (hd::t) z
                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z 
        |E aa, N bb -> if a = ":true:" then mainFunction2 tl (hd::t) z 

                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |N aa, E bb ->  if a = ":true:" then mainFunction2 tl (hd::t) z 
                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |P aa, N bb -> if a = ":true:" then mainFunction2 tl (hd::t) z 

                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |N aa, P bb ->  if a = ":true:" then mainFunction2 tl (hd::t) z 
                      else if a = ":false:" then mainFunction2 tl (hd2::t) z
                      else mainFunction2 tl (E(":error:")::y) z
        |_-> if a = ":true:" then mainFunction2 tl (hd::t) z   
             else if a = ":false:" then mainFunction2 tl (hd2::t) z 
             else mainFunction2 tl (E(":error:")::y) z
        )

        | N a, N b, N c -> if boolFunction z c = ":true:" then mainFunction2 tl (N(a)::t) z
        else if (boolFunction z c) = ":false:" then mainFunction2 tl (N(b)::t) z
        else mainFunction2 tl (E(":error:")::y) z

        | N a, hd2, N c -> if boolFunction z c = ":true:" then mainFunction2 tl (N(a)::t) z 
                           else mainFunction2 tl (hd2::t) z 
        | hd, N a, N c -> if boolFunction z c = ":true:" then mainFunction2 tl ((hd::t)) z 
                           else mainFunction2 tl ((bindFunction2 z a)::t) z
        | hd, hd2, N a ->  if boolFunction z a = ":true:" then mainFunction2 tl (hd::t) z 
        else if (boolFunction z a) = ":false:" then mainFunction2 tl (hd2::t) z 
        else mainFunction2 tl (E(":error:")::y) z


        |_-> mainFunction2 tl (E(":error:")::y) z
        )
        |_-> mainFunction2 tl (E(":error:")::y) z
        )
        | Let -> (match mainFunction2 tl y z with
          (Some const,x1)-> mainFunction2 x1 (const::y) z
          | (None, x1)-> mainFunction2 x1 y z
        )

        | End -> (
          match y with
          []-> (None,tl)
          | hd1::tl1-> (Some hd1,tl)
        )

        |_-> mainFunction2 tl y z
      )

  in


   (* -------------------------------------above is the mainFunction2 ---------------------------------------------*)
   (* -------------------------------------above is the mainFunction2 ---------------------------------------------*)
   (* -------------------------------------above is the mainFunction2 ---------------------------------------------*)
   (* -------------------------------------above is the mainFunction2 ---------------------------------------------*)
   (* -------------------------------------above is the mainFunction2 ---------------------------------------------*)
   (* -------------------------------------above is the mainFunction2 ---------------------------------------------*)
   (* -------------------------------------above is the mainFunction2 ---------------------------------------------*)

  let rec mainFunction (x:dataType list) (y:const list) (z:(string * const) list) : const list =
    match x with
      [] ->  y
| hd::tl -> (match hd with 
          Pushi i-> mainFunction tl (i::y) z
        | Pushb b-> mainFunction tl (b::y) z
        | Pushn n -> mainFunction tl (n::y) z
        | Pushs s-> mainFunction tl (s::y) z
        | Push  p-> (match p with 
          P a -> mainFunction tl (P(a)::y) z
          |_-> mainFunction tl (E(":error:")::y) z
        )    
        | Pop -> (match y with 
        | [] -> mainFunction tl (E(":error:")::y) z
        | hd2::tl222 ->  mainFunction tl tl222 z
          )
        | Add ->  
           (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              (N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd44::hd55::tl55 -> (match(hd44,hd55) with
              (I a, I b) -> mainFunction tl ((I (a+b))::tl4) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |_-> mainFunction tl (E(":error:")::y) z
              ) else if List.length(temp) = 1 &&x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              I a -> mainFunction tl (I(a + a)::tl4) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |_-> mainFunction tl (E(":error:")::y) z
              )
              else (match (hd3,tl3) with
               (I a, I b) ->mainFunction tl ((I (a+b))::tl4) z
              | _->  mainFunction tl ((E(":error:"))::y) z
                  ) 
              | I a, N b -> if checkDup b z && minusValue b z !=0 then (mainFunction tl (I(combineValue b z a)::tl4)z)
                            else mainFunction tl ((E(":error:"))::y) z
              | N a, I b -> if checkDup a z && minusValue a z !=0 then (mainFunction tl (I(combineValue a z b)::tl4)z)
                            else mainFunction tl ((E(":error:")::y))z
              | I a, I b ->mainFunction tl ((I (a+b))::tl4) z
              | _->  mainFunction tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction tl (E(":error:")::y) z
            |_-> mainFunction tl (E(":error:")::y) z
          )
        | Sub -> 
              (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              (N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match (minusValue x z), (minusValue y2 z) with
              a, b -> if a != 0 && b!=0 then mainFunction tl(I(b-a)::tl4) z else mainFunction tl (E(":error:")::y) z
              ) else if List.length(temp) = 1 &&x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              I a -> mainFunction tl (I(a - a)::tl4) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |_-> mainFunction tl (E(":error:")::y) z
              )
              else (match (hd3,tl3) with
               (I a, I b) ->mainFunction tl ((I (b-a))::tl4) z
              | _->  mainFunction tl ((E(":error:"))::y) z) 
              | I a, N b -> if checkDup b z && minusValue b z != 0 then (mainFunction tl (I((minusValue b z)-a)::tl4)z)
                            else mainFunction tl ((E(":error:"))::y) z
              | N a, I b -> if checkDup a z && minusValue a z != 0 then (mainFunction tl (I(b-(minusValue a z))::tl4)z)
                            else mainFunction tl ((E(":error:"))::y) z
              | I a, I b ->mainFunction tl ((I (b-a))::tl4) z
              | _->  mainFunction tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction tl (E(":error:")::y) z
            |_-> mainFunction tl (E(":error:")::y) z
          )
        | Mul -> (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              (N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd44::hd55::tl55 -> (match(hd44,hd55) with
              (I a, I b) -> mainFunction tl ((I (a*b))::tl4) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |_-> mainFunction tl (E(":error:")::y) z
              ) else if List.length(temp) = 1 && x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              I a -> mainFunction tl (I(a * a)::tl4) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |_-> mainFunction tl (E(":error:")::y) z
              )
              else (match (hd3,tl3) with
               (I a, I b) ->mainFunction tl ((I (a*b))::tl4) z
              | _->  mainFunction tl ((E(":error:"))::y) z
                  ) 
              | I a, N b -> if checkDup b z && minusValue b z !=0 then (mainFunction tl (I((minusValue b z)*a)::tl4)z)
                            else mainFunction tl ((E(":error:"))::y) z
              | N a, I b -> if checkDup a z && minusValue a z !=0 then (mainFunction tl (I((minusValue a z)*b)::tl4)z)
                            else mainFunction tl ((E(":error:"))::y) z
              | I a, I b ->mainFunction tl ((I (a*b))::tl4) z
              | _->  mainFunction tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction tl (E(":error:")::y) z
            |_-> mainFunction tl (E(":error:")::y) z
          )
        | Div -> 
        (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              (N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match minusValue x z, minusValue y2 z with
              a, b ->( try mainFunction tl ((I (b/a))::tl4) z with Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)
              ) else if List.length(temp) = 1 && x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              I a -> mainFunction tl (I(a / a)::tl4) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |_-> mainFunction tl (E(":error:")::y) z
              ) else (match (hd3,tl3) with
               (I a, I b) ->( try mainFunction tl ((I (b/a))::tl4) z with Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)
              | _->  mainFunction tl ((E(":error:"))::y) z) 
              | I a, N b -> if checkDup b z && minusValue b z !=0 then (try (mainFunction tl (I((minusValue b z)/a)::tl4)z) with Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)
                            else mainFunction tl ((E(":error:"))::y) z
              | N a, I b -> if checkDup a z && minusValue a z !=0 then (try (mainFunction tl (I(b/(minusValue a z))::tl4)z) with Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)
                            else mainFunction tl ((E(":error:"))::y) z
              | I a, I b ->( try mainFunction tl ((I (b/a))::tl4) z with Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)
              | _->  mainFunction tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction tl (E(":error:")::y) z
            |_-> mainFunction tl (E(":error:")::y) z
          )
        | Rem -> 
        (match y with 
              hd31::tl31::tl41-> (match hd31,tl31 with
                  I a, I b -> (try mainFunction tl ((I (b mod a))::tl41) z with 
                      Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)
                | I a, N b -> if checkDup b z && minusValue b z !=0 then (try mainFunction tl ((I ((remFunction z b) mod a))::tl41) z with 
                      Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)
                      else mainFunction tl ((E(":error:")::y)) z
                | N a, I b -> if checkDup a z && minusValue a z !=0 then (try mainFunction tl ((I (b mod (remFunction z a)))::tl41) z with 
                      Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)
                      else mainFunction tl ((E(":error:")::y)) z
                | N a, N b -> if List.length(bindFunction z a b []) = 2 then (match minusValue a z, minusValue b z with
                  a, b -> (try mainFunction tl ((I (b mod a))::tl41) z with 
                      Division_by_zero -> mainFunction tl ((E(":error:"))::y) z)) 
                      else if List.length(bindFunction z a b []) = 1 && a = b then 
                      mainFunction tl(I(0)::tl41) z
                      else mainFunction tl ((E(":error:"))::y) z
                | _-> mainFunction tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction tl (E(":error:")::y) z
            |_->mainFunction tl (E(":error:")::y) z
          )
        | Neg -> (match y with
              hd34::tl34 -> (match hd34 with
                  N a -> if (checkDup a z) then let temp = (bindFunction2 z a) in 
                  (match temp with
                  I hd -> mainFunction tl (I(hd * -1)::tl34) z
                  |_-> mainFunction tl ((E(":error:"))::y) z)
                  else mainFunction tl ((E(":error:"))::y) z 
                |I a -> mainFunction tl (I(a * -1)::tl34) z
                | _-> mainFunction tl ((E(":error:"))::y) z
              )
            |_->mainFunction tl (E(":error:")::y) z
          )
        | Swap -> (match y with
              h::h2::t-> mainFunction tl (h2::h::t) z
            |h3::t2-> mainFunction tl (E(":error:")::y) z
            |_-> mainFunction tl (E(":error:")::y) z
          )

        | Cat -> 
              (match y with 
              hd3::tl3::tl4-> (match (hd3,tl3) with
              N x, N y2 -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd::hd2::tl5 -> (match hd, hd2 with
               S a, S b -> mainFunction tl ((S (a^"" ^b))::tl4) z
                 |_->mainFunction tl ((E(":error:"))::y) z
              )
              |_->mainFunction tl ((E(":error:"))::y) z
              ) else (match (hd3,tl3) with
               S a, S b ->mainFunction tl ((S (a^"" ^b))::tl4) z
              | _->  mainFunction tl ((E(":error:"))::y) z
                  ) 
              | S a, S b -> mainFunction tl (S (b^""^a)::tl4) z
              | N a, S b -> (match bindFunction2 z a, b with
               S a, b -> mainFunction tl (S (b^""^a)::tl4) z
               |_->mainFunction tl ((E(":error:"))::y) z
              )
              | S a, N b -> (match a, bindFunction2 z b with
               a, S b -> mainFunction tl (S(b^""^a)::tl4) z
               |_-> mainFunction tl ((E(":error:"))::y) z
              )
              | _->  mainFunction tl ((E(":error:"))::y) z
              )
            | hd4::tl5 -> mainFunction tl (E(":error:")::y) z
            |_-> mainFunction tl (E(":error:")::y) z
          )
        | And -> 
         (match y with 
              hd3::tl3::t-> (match (hd3,tl3) with
              N a, B b -> let temp2 = (bindFunction2 z a) in
              (match temp2 with
              B a -> if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |B a, N b -> let temp2 = (bindFunction2 z b) in
              (match temp2 with
              B b -> if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |B a, B b -> if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z

              |(N x, N y2) -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd44::hd55::tl55 -> (match(hd44,hd55) with
              (B a, B b) -> if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |(S a, B b) ->if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |(B a, S b) ->if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |_-> mainFunction tl (E(":error:")::y) z
              )

              |_-> mainFunction tl y z) 
              else if  List.length(temp) = 1 &&x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              B a ->  mainFunction tl (B(":true:")::t) z 
              |_-> mainFunction tl (E(":error:")::t) z
              )
              |_-> mainFunction tl (E(":error:")::t) z
              ) 
               else (match y with
              h::h2::tt -> (match h,h2 with 
              B a, B b -> if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |_-> mainFunction tl (E(":error:")::y) z
             )
             |h3::t2-> mainFunction tl (E(":error:")::y) z
            |_-> mainFunction tl (E(":error:")::y) z
        ) 
        |_-> mainFunction tl (E(":error:")::y) z
          )
          |_-> mainFunction tl (E(":error:")::y) z)

        | Or -> 
        (match y with 
              hd3::tl3::t-> (match (hd3,tl3) with

             B a, B b -> if a = ":true:" && b =":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":false:" then mainFunction tl (B(":false:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":false:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":false:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z


            |N a, B b -> let temp2 = (bindFunction2 z a) in
              (match temp2 with
              B a -> if a = ":true:" && b =":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":false:" then mainFunction tl (B(":false:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":false:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":false:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |_-> mainFunction tl (E(":error:")::y) z
              )

            |B a, N b -> 
            let temp2 = (bindFunction2 z b) in
              (match temp2 with
              B b -> if a = ":true:" && b =":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && (boolFunction z b) = ":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && (boolFunction z b) = ":false:" then mainFunction tl (B(":false:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":true:" && (boolFunction z a) = ":false:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":true:" then mainFunction tl (B(":true:")::t)  z
              else if b = ":false:" && (boolFunction z a) = ":false:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |N x, N y2 -> let temp = (bindFunction z x y2 []) in

              if List.length(temp) = 2 then (match temp with
              hd44::hd55::tl55 -> (match(hd44,hd55) with
              B a, B b -> if a = ":true:" && b =":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |(S a, B b) -> if a = ":true:" && b =":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |(B a, S b) -> if a = ":true:" && b =":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |_-> mainFunction tl (E(":error:")::y) z
              )
              |_-> mainFunction tl y z) else if List.length(temp) = 1 &&x = y2 then (match temp with
              hd44::tl55 -> (match hd44 with
              B a -> if a = ":false:" then mainFunction tl (B(":false:")::t) z else mainFunction tl (B(":true:")::t) z
              |_-> mainFunction tl (E(":error:")::t) z
              )
              |_-> mainFunction tl (E(":error:")::t) z
              ) else (match y with
             h::h2::tt -> (match h,h2 with 
             B a, B b -> if a = ":true:" && b =":false:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":true:" && b =":true:" then mainFunction tl (B(":true:")::t)  z
              else if a = ":false:" && b =":false:" then mainFunction tl (B(":false:")::t)  z
              else mainFunction tl (E(":error:")::y) z
              |_-> mainFunction tl (E(":error:")::y) z
             )
             |h3::t2-> mainFunction tl (E(":error:")::y) z
            |_-> mainFunction tl (E(":error:")::y) z
        ) 
        |_-> mainFunction tl (E(":error:")::y) z
          )
          |_-> mainFunction tl (E(":error:")::y) z)

        | Not -> (match y with
        h::t -> (match h with
        B a -> if a =":true:" then mainFunction tl (B(":false:")::t) z
        else if a =":false:" then mainFunction tl (B (":true:")::t)  z
        else mainFunction tl (E (":error:")::y)  z
        | N a -> if boolFunction z a = ":true:" then mainFunction tl (B(":false:")::t) z
        else if boolFunction z a =":false:" then mainFunction tl (B (":true:")::t)  z
        else mainFunction tl (E (":error:")::y)  z
        |_-> mainFunction tl (E(":error:")::y)  z
        )
        |_-> mainFunction tl (E(":error:")::y)  z
        )
        | Equal -> (match y with
        hd::hd2::t -> (match hd, hd2 with
        I a, I b -> if a = b then mainFunction tl (B (":true:")::t) z else mainFunction tl (B(":false:")::t) z
        |N a, N b -> if List.length(bindFunction z a b []) = 2 then (match bindFunction z a b [] with
        hd::hd2::tt -> (match hd, hd2 with
        I a, I b -> if a = b then mainFunction tl (B (":true:")::t) z else mainFunction tl (B(":false:")::t) z
        |_->mainFunction tl (E(":error:")::y) z
        )
        |_->mainFunction tl (E(":error:")::y) z
        )else mainFunction tl (B(":false:")::t) z
        |N a, I b -> (match (bindFunction2 z a), b with
        I a, b -> if a = b then mainFunction tl (B (":true:")::t) z  
        else  mainFunction tl (B (":false:")::t) z
        |_->mainFunction tl (E(":error:")::y) z
        )
        |I a, N b -> (match (bindFunction2 z b), a with
        I a, b -> if a = b then mainFunction tl (B (":true:")::t) z  
        else  mainFunction tl (B (":false:")::t) z
        |_->mainFunction tl (E(":error:")::y) z
        )
        |_->mainFunction tl (E(":error:")::y) z

        )
        |_->mainFunction tl (E(":error:")::y) z
        )
        | LessThan -> (match y with
        hd::hd2::t -> (match hd,hd2 with
        I a, I b -> if a > b then mainFunction tl (B (":true:")::t) z else mainFunction tl (B (":false:")::t) z
        |N aa, N bb -> if List.length(bindFunction z aa bb []) = 2 then (match (bindFunction2 z aa),(bindFunction2 z bb) with
        I a, I b -> if a > b then mainFunction tl (B (":true:")::t) z  
        else  mainFunction tl (B (":false:")::t) z
        |_-> mainFunction tl (E(":error:")::y) z
        ) else if List.length(bindFunction z aa bb []) = 1 && aa=bb then mainFunction tl (B (":false:")::t) z
         else mainFunction tl (E(":error:")::y) z
        |N a, I b -> (match (bindFunction2 z a), b with
        I a, b -> if a > b then mainFunction tl (B (":true:")::t) z  
        else  mainFunction tl (B (":false:")::t) z
        |_->mainFunction tl (E(":error:")::y) z
        )
        |I a, N b -> (match (bindFunction2 z b), a with
        I a, b -> if a < b then mainFunction tl (B (":true:")::t) z  
        else  mainFunction tl (B (":false:")::t) z
        |_->mainFunction tl (E(":error:")::y) z
        )
        |_->mainFunction tl (E(":error:")::y) z
        )
        |_->mainFunction tl (E(":error:")::y) z
        )
        | Bind -> (match y with
        hd::hd2::t -> (match hd, hd2 with

        I a, N b ->   if checkDup b z then mainFunction tl (S(":unit:")::t)(overwriteFunction b [] z hd)
                      else mainFunction tl (S (":unit:")::t) ((b,I(a))::z)
        | S a, N b -> if checkDup b z then mainFunction tl (S(":unit:")::t)(overwriteFunction b [] z hd) 
                      else mainFunction tl (S (":unit:")::t) ((b,S(a))::z)
        | N a, N b -> if checkDup b z && checkDup a z then mainFunction tl (S(":unit:")::t) (overwriteFunction3 b a [] z (S(reboundFunction a z)))
                      else if checkDup b z && checkDup a z = false then mainFunction tl (E(":error:")::y) z  
                      else if checkDup b z then mainFunction tl (S(":unit:")::t)(overwriteFunction2 b z a) 
                      else if bindFunction2 z a = E(":error:") then mainFunction tl (E(":error:")::y) z
                      else mainFunction tl (S (":unit:")::t) ((b,(bindFunction2 z a))::z) 
        | P a, N b -> if a=":error:" then mainFunction tl (E(":error:")::y) z  
                      else if checkDup b z then mainFunction tl (E(":unit:")::t)(overwriteFunction b [] z hd)
                      else mainFunction tl (S(":unit:")::t) ((b,P(a))::z)
        | B a, N b->  if checkDup b z then mainFunction tl (S(":unit:")::t)(overwriteFunction b [] z hd)
                      else mainFunction tl (S (":unit:")::t) ((b,B(a))::z)

        |_->mainFunction tl (E(":error:")::y) z
        )
        |_->mainFunction tl (E(":error:")::y) z
        )

        | If -> (match y with

        hd::hd2::hd3::t -> (match hd,hd2,hd3 with
        hd, hd2, B a -> (match hd, hd2 with

        N aa, N bb ->  if a = ":true:" then mainFunction tl (hd::t) z 

                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |I aa, N bb -> if a = ":true:" then mainFunction tl (hd::t) z 

                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |N aa, I bb ->  if a = ":true:" then mainFunction tl (hd::t) z 
                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |S aa, N bb -> if a = ":true:" then mainFunction tl (hd::t) z 

                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |N aa, S bb -> if a = ":true:" then mainFunction tl (hd::t) z 
                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |E aa, S bb -> if a = ":true:" then mainFunction tl (hd::t) z
                       else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |S aa, E bb -> if a =":true:" then mainFunction tl (hd::t) z
                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z 
        |E aa, N bb -> if a = ":true:" then mainFunction tl (hd::t) z 

                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |N aa, E bb ->  if a = ":true:" then mainFunction tl (hd::t) z 
                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |P aa, N bb -> if a = ":true:" then mainFunction tl (hd::t) z 

                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |N aa, P bb ->  if a = ":true:" then mainFunction tl (hd::t) z 
                      else if a = ":false:" then mainFunction tl (hd2::t) z
                      else mainFunction tl (E(":error:")::y) z
        |_-> if a = ":true:" then mainFunction tl (hd::t) z   
             else if a = ":false:" then mainFunction tl (hd2::t) z 
             else mainFunction tl (E(":error:")::y) z
        )

        | N a, N b, N c -> if boolFunction z c = ":true:" then mainFunction tl (N(a)::t) z
        else if (boolFunction z c) = ":false:" then mainFunction tl (N(b)::t) z
        else mainFunction tl (E(":error:")::y) z

        | N a, hd2, N c -> if boolFunction z c = ":true:" then mainFunction tl (N(a)::t) z 
                           else mainFunction tl (hd2::t) z 
        | hd, N a, N c -> if boolFunction z c = ":true:" then mainFunction tl ((hd::t)) z 
                           else mainFunction tl ((bindFunction2 z a)::t) z
        | hd, hd2, N a ->  if boolFunction z a = ":true:" then mainFunction tl (hd::t) z 
        else if (boolFunction z a) = ":false:" then mainFunction tl (hd2::t) z 
        else mainFunction tl (E(":error:")::y) z


        |_-> mainFunction tl (E(":error:")::y) z
        )
        |_-> mainFunction tl (E(":error:")::y) z
        )

        | Let -> ( match mainFunction2 tl y z with
          (Some const,x1)-> mainFunction x1 (const::y) z
          | (None, x1)-> mainFunction x1 y z
        )
        | Quit -> printTuple z;  y

        |_-> mainFunction tl y z
      )

  in

  let stack = mainFunction inputList [] []in

  let rec printStack (p: const list): string list =
    match p with
      [] -> []
    |hd::tl -> (match hd with
          I i -> (string_of_int i) :: printStack tl
        | B b ->  b :: printStack tl
        | S s -> s::printStack tl
        | N n -> n::printStack tl
        | P p -> p::printStack tl
        | E e -> e::printStack tl
      )

  in

  let stringStack = printStack stack in


  List.iter (fun s -> print_endline s) (stringStack)
(* List.iter (fun s -> file_write s) (stringStack) *)

;;

interpreter ("input/input1.txt" ,"output.txt" );;