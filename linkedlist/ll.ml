type node = { mutable data: int;
			  mutable next: node option}

type llist = { mutable head: node option;
			   mutable size: int}

let new_list () =
	{ head = None; size = 0}

let add_first i ll =
	ll.head <- Some {data = i; next = ll.head};
	ll.size <- ll.size+1

let add_last i ll =
	let rec add_last_h i n = 
		match n.next with
		| None -> n.next <- Some {data=i; next = None}
		| Some nn -> add_last_h i nn
	in
	(match ll.head with
	| None -> ll.head <- Some  {data=i; next=None}
	| Some n -> add_last_h i n);
	ll.size <- ll.size+1

let string_of_llist ll =
	let rec helper (n:node option) : string =
		match n with
		| None -> ""
		| Some nn -> string_of_int nn.data ^ ", " ^ helper nn.next
	in
	helper ll.head

let mapl f l =
	let rec helper no =
		match no with
		| None -> ()
		| Some n -> n.data <- f n.data;
						helper n.next
	in
	helper l.head