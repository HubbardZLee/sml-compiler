signature CMD = sig
    val get_command: string -> string list
end
		    
structure Cmd :> CMD =
struct 
  structure SS = Substring;
  structure C = Char;
  structure TIO = TextIO;

  fun get_command (prompt) =
      let
	  val _ = print prompt;
	  val line = TIO.inputLine TIO.stdIn
	  fun trimWs ss = (SS.dropl (C.isSpace) (SS.dropr (C.isSpace) ss))
	  fun split_substr (substr) =
	    let val (l, r) = SS.splitl (not o C.isSpace) substr
		val (l, r) = (trimWs l, trimWs r)
	     in if SS.size l + SS.size r = 0
		   then []
		   else (SS.string l)::split_substr r
	    end
      in
	  case line of
	      SOME str => split_substr(SS.full str)
	   | NONE => []
      end
			 
end
    
