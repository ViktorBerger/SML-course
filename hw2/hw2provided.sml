(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(one:string, all: string list) = 
   case all of
       [] => NONE
     | c::cs  => if same_string(c,one)
		then SOME cs
		else case all_except_option(one,cs) of
			 NONE => NONE
		       | SOME lst => SOME (c::lst) 


(* string list list * string -> string list *)
fun get_substitutions1(lists: string list list, s: string) = 
    case lists of
	[] => []
      | l::ls => case all_except_option(s,l) of
		    NONE => get_substitutions1(ls,s)
		  | SOME lst =>  lst @ get_substitutions1(ls,s)

fun get_substitutions2(lists: string list list, s: string) = 
    let fun help(lst: string list list, acc: string list) = 
	    case lst of
		[] => acc
	      | l::ls => case all_except_option(s,l) of
			     NONE => help(ls,acc)
			   | SOME lst =>  help(ls, acc @ lst)
    in 
	help(lists,[])
    end

fun similar_names(lst: string list list, name:{first:string, middle:string, last:string}) = 
    let fun help(firsts,middle,last,acc) = 
	    case firsts of
		[] => acc
	      | n::ns => help(ns,middle,last,{first=n,middle=middle, last=last}::acc)
    in
	case name of
	    {first,middle,last} => help(rev(get_substitutions1(lst,first))@[first],middle,last,[])
    end		

 
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(s,r) = 
    case s of
    Spades => Black 
     | Clubs => Black
     | Diamonds => Red
     | Hearts => Red 

fun card_value(s,r) = 
    case r of
	Num i => i
     | Ace => 11
     | _ => 10 

fun remove_card(cs: card list, ca:card, e:exn) = 
    case cs of
	[] => raise e
     | c::cs => if c = ca then cs else c::remove_card(cs,ca,e)

fun all_same_color(cs:card list) = 
    case cs of 
	[] => true 
     | (s,r)::[] => true
     | (s,r)::((s1,r1)::cards) => card_color(s,r) = card_color(s1,r1) 
				  andalso all_same_color(((s1,r1)::cards))	

fun sum_cards (cards: card list) = 
    let fun aux(deck,acc) =
	  case deck of
	      [] => acc
	   | c::cs => aux(cs,acc + card_value(c))
    in
	aux(cards,0)
    end

fun score(cards: card list, goal: int) = 
    let val sum = sum_cards(cards)
    in
	let val  preliminary = 
		 if sum > goal
		 then 3 * (sum - goal)
		 else (goal - sum)
	in 
	    if all_same_color(cards)
	    then preliminary div 2
	    else preliminary
	end
    end

fun officiate(cards: card list, moves: move list, goal:int) = 
    let fun state(hand: card list,deck: card list, mv: move list) =
	    case mv of
		[] => hand
	      | (Discard c)::mv' => state( remove_card(hand,c,IllegalMove), deck, mv')
	      | Draw::mv' => case deck of 
				 [] => hand
			       | c::cs => if sum_cards(c::hand) > goal
					  then c::hand
					  else state(c::hand,cs,mv')
    in
	score( state([],cards,moves), goal)
    end


