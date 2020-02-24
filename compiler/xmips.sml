structure xMips =
struct
structure S = Symbol
structure E = Emulator
structure M = Mips
structure C = Compile
(*
   compile filename
   run
   break/b label
   breaki/bi address
   unbreak/ub label/all
   unbreak/ubi address
   step/s
   continue/c
   exit/x
   display/d register [register ...]
   display/d address [count]
   list/l [[label] [count]]
   listi/li [[address] [count]]
*)
		  
exception ExitRepl
val prompt = "xMips> "
exception Continue

fun get_list_args args (def_start, def_count)=
  (if (length args) = 0
  then (def_start, def_count)
  else
      let
	  val flabel = List.nth(args,0)
	  val faddr = (E.get_addr (S.symbol flabel)
		       handle _ =>
			      (print ("Bad label: " ^ flabel ^ "\n");
			       raise Continue
		      ))
      in
	  if (length args) = 1 then
	      (faddr, def_count)
	  else if (length args) = 2 then
	      let
		  val e = List.nth(args,1)
		  val ecount = valOf (Int.fromString e)
			       handle _ =>
				      (print ("Bad count value: " ^ e ^ "\n");
				       raise Continue)
	      in
		  (faddr, ecount)
	      end
	  else
	      (print "Too many args\n";
	       	       raise Continue)
      end
	  )

fun run () =
  let
      val continue = ref(true)
      (* info to track for list command. Address and count. *)
      val list_address = ref(0)
      val list_count = ref(10)
  in
 
      (while !continue do
	     ( (* while body *)
	       let
		   val cmd_line = Cmd.get_command prompt
	       in
		   (case cmd_line of
			[] => ()
		      | [("compile"|"c"), filename] =>
			(print("compiling " ^ filename ^"\n");
			 let val p = (C.compile filename)
			 in
			     E.load_program p
			 end
			 handle _ => print "Compile failed\n")
			    
		      | [("run"|"r")] =>
			(print "Running program\n";
			 E.run()
			)
		      | [("trace"|"t"), "on"] =>
			(print "Turn trace on\n";
			 E.set_trace_on())
		      | [("trace"|"t"), "off"] =>
			(print "Turn trace off\n";
			 E.set_trace_off())
		      | [("break"|"b")] =>
			(print ("Break points: "); app (fn x=> print((Int.toString x) ^",")) (!E.break_points); print("\n"))
			    
		      | [("break"|"b"), label] =>
			(print("set break point at " ^ label ^ "\n");
			 E.set_break_at_label label
			)
		      | [("breaki"|"bi"), address] =>
			(print("set break point at " ^ address ^ "\n");
			 E.set_break_at_addr (valOf(Int.fromString address))
			)
		      |  [("clear")] => (print "Removing all breakpoints\n"; E.clear_break_points ())

		      | [("step"|"s")] =>
			E.exec_one(true)
				  
		      | [("continue"|"c")] =>
			E.exec_pgm()

		      | ("display"|"d")::args =>
			let
			    fun print_reg (regname:string) =
			      (case E.mips_reg regname of
				   NONE =>   print (regname ^ ": Invalid of not in scope\n")
				 | SOME reg => 
				   if E.is_active_reg reg then
				       print (regname ^ ":  " ^ Int.toString(E.get_reg reg) ^ "\n")
				   else
				       ())
			in
			    app print_reg args
			end

		      | ("displayi"|"di")::addresses =>
			(
			  app E.print_mem_entry (map (valOf o Int.fromString) addresses)
			)

		      | ("list"|"l")::args =>
			let
			    val (start,count) = get_list_args args ((!list_address), (!list_count))
			in
			    E.print_program (start, start+E.mul32 count);
			    list_address := start + E.mul32 count ;
			    list_count := count
			end

		      | [("exit"|"x")] =>  raise ExitRepl
		      | ("help"|"h")::_ => print "Help\n"	   
		      | _ => print "Unrecognized command; check if an argument is missing\n"
				)
	       end
	       handle Continue => ()
		    | ExitRepl => continue := false
		    | _ => print "Exception occured, ignoring it\n" 
	     )
      )
  end  
    
end
