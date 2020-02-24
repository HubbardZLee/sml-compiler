
structure Emulator =

struct

structure M = Mips
structure S = Symbol
structure H = HashTable
type reg_value = int

(*  State: registers and memory *)
(* --------- registers -------------- *)

(* Model registers: 
   * real_registers: values of real MIPS registers
   * stack_frame: stack of values of virtual registers

   * Virtual registers are in a stack because virtual registers are expected to be 
   * local to a procedure

   *)

val real_registers: (int S.table) ref = ref (S.empty)
val stack_frame: ((int S.table) list) ref = ref ([S.empty])
val virtual_regnames: (M.reg S.table) ref = ref(S.empty)

(* make our own table of MIPS registers, so we don't need to change mips.sig/.sml code *)
val real_regnames =
    (foldl (fn(reg,acc) =>
	     let val regname = (S.symbol o M.reg2name) reg
	     in S.enter(acc, regname, reg)
	     end)
	  S.empty  M.registers)
			
fun reg2sym reg = S.symbol (M.reg2name reg)
exception VirtualRegisterNotInitialized
fun mips_reg (regname:string) =
  let
      val regsym = (S.symbol regname)
  in
      case S.look((! virtual_regnames), regsym) of
	  SOME x => SOME x
	| NONE => S.look (real_regnames, regsym)
  end
		       

fun is_active_reg (reg) =
  if not (M.isvirtual reg) then
      (* real registers are always active *)
      true
  else
      (* virtual registers are active only when in top scope *)
      let
	  val top_frame::rest = !stack_frame
      in
	  (case S.look(top_frame, reg2sym reg) of
	       NONE => false
	     | SOME x => true )
      end
	  
fun get_reg reg =
  let
      val reg_ = reg2sym reg
  in
      if M.isvirtual reg then
	  let
	      val top_frame::rest = !stack_frame
	  in
	      (case S.look(top_frame, reg_) of
		  NONE => (* virtual register should be used without value in it *)
	          raise VirtualRegisterNotInitialized
		| SOME x => x )
	  end
      else case S.look(!real_registers, reg_) of
	       (* initial value of real registers is zero *)
	       NONE => 0
	     | SOME x => x
	  
  end
fun set_reg (reg, value) =
  let
      val regsym = reg2sym reg
  in
      if M.isvirtual reg then
	  let val top_frame::stack = !stack_frame
	  in
	      stack_frame := (S.enter(top_frame, regsym, value))::stack;
	      (* Keep symbol to Mips reg mapping; can't create it otherwise *)
	      virtual_regnames := S.enter(!virtual_regnames, regsym, reg)
	  end
      else
	  real_registers := S.enter(!real_registers, regsym, value)
  end
fun push_frame () =
  stack_frame := (S.empty)::(!stack_frame)
      
fun pop_frame () =
  let
      val _::stack = !stack_frame
  in
      stack_frame := stack
  end 

(* ------------ memory -------------- *)
val MEMSIZE = 4*10240

datatype mem_value = IntValue of reg_value | StrValue of string | InstrValue of M.instruction
type labeled_mem = {value:mem_value, labels: M.lab list}
exception MemoryError
val memory: labeled_mem Array.array = Array.array(MEMSIZE,{value=IntValue(0), labels=[]})
(* Assume 32 bit architecture; permit addressing addresses that are multiple of 4 *)
val off32 = 4
fun mul32 x = x * off32
fun div32 x = Int.div(x,off32)

fun get_mem_entry addr = Array.sub(memory, div32 addr)
fun set_mem_entry (addr,entry) = Array.update(memory, div32 addr, entry)
											
fun get_value addr = let val {value=v, labels= l} = get_mem_entry addr in v end
fun set_value (addr,v) = let val {value=_, labels= l} = get_mem_entry addr in set_mem_entry(addr, {value=v, labels=l}) end

fun get_labels addr = let val {value=v, labels= l} = get_mem_entry addr in l end
fun add_label (addr, l) =
  let val {value=v, labels=labels} = get_mem_entry addr
  in
      set_mem_entry(addr, {value=v, labels=l::labels})
  end
fun clear_labels addr = let val {value=v, labels=_} = get_mem_entry addr in set_mem_entry(addr, {value=v, labels=[]}) end

fun get_mem_int addr =  let val IntValue(v) = get_value addr in v end
fun set_mem_int (addr, v) = set_value(addr, IntValue(v))

fun get_mem_str addr = let val StrValue(v) = get_value addr in if v="\\n" then "\n" else v end
fun set_mem_str (addr,v) =  set_value(addr, StrValue(v))
fun get_mem_instr addr = let val InstrValue(v) = get_value addr in v end
fun set_mem_instr (addr,v) = set_value(addr, InstrValue(v))
					 
fun reset_mem () = Array.appi (fn (i,_)=> (set_mem_int(i, 0); (clear_labels i))) memory
(* --------- initialize state ---------- *)

fun reset_regs () =
 (     (* empty registers and stack frame *)
   real_registers := S.empty;
    stack_frame := [S.empty];
    (* initialize $gp, $fp, $sp, and $ra *)
    (* $gp contains address of location that points
       to the bottom of the heap  *)
    (let
	val heap_top = MEMSIZE - 2*10240
	val heap_ptr = heap_top - 4
    in
	set_mem_int (heap_top, heap_ptr);
	set_reg ((M.reg "$gp"), heap_top)
    end
    );
    set_reg ((M.reg "$sp"), MEMSIZE-1);
    set_reg ((M.reg "$fp"), MEMSIZE-1);
    (* $ra = -1 means terminate *)
    set_reg ((M.reg "$ra"), ~1)
 )
     

exception ExitProgram
exception SyscallNotImplemented
exception InstructionNotImplemented
(* Visit for details of calling convention for syscall
 *   http://students.cs.tamu.edu/tanzir/csce350/reference/syscalls.html
 *)
fun exec_syscall () =
  let
      val call_code = get_reg (M.reg "$v0")
      val a0 = get_reg (M.reg "$a0")
  in
      case call_code of
	  1 => (*print int *)
	  print(Int.toString(a0))
	| 9 => (* allocate memory *)
	  (* $a0 = number of bytes of storage desired	$v0 = address of block *)
	  let
	      (* FIXIT -- initialize gp correctly *)
	      val gp = get_reg(M.reg "$gp")
	  in
	      set_reg(M.reg "$gp", gp+a0);
	      set_reg((M.reg "$v0"), gp)
	  end
	| 4 => (* print string *)
	  print(get_mem_str(a0))
	| 10 => (* exit *)
	  raise ExitProgram
	| _ =>  (* not implemented *)
	  raise SyscallNotImplemented
  end


(* ---------- code ----------------- *)
val load_address = ref(0)
val label_table: (int S.table) ref = ref(S.empty)
		      
fun reset_pgm () = (load_address := 0; label_table := S.empty)
fun bump_load () = load_address := !load_address + off32
fun next_load_address () = !load_address

fun load_label lab =
  (* insert label at the next available address
     don't increment the address, though.
     the label will get attached to the next 
     instruction/data inserted *)
  let
      val addr = next_load_address()
  in
      add_label (addr, lab);
      label_table := S.enter(!label_table, lab, addr)
  end

exception MissingLabel
fun get_addr lab =
  (case S.look(!label_table, lab) of
       NONE => raise MissingLabel
     | SOME x => x)

fun load_string str =
  (set_mem_str (next_load_address(), str);
   bump_load()  )
       
fun load_instr instr =
    (set_mem_instr (next_load_address(), instr); bump_load())

(* if anything is inserted, assume program is loaded *)
fun exists_pgm () = next_load_address() <> 0

fun get_instr pc = get_mem_instr pc

(* ------ debugging support ---------- *)

fun print_mem_entry addr =
  let
      val {value = value, labels = labels} = get_mem_entry addr
      val hxaddr = StringCvt.padLeft #"0" 4 (Int.toString addr)
  in
      app (fn x => print (M.lab2string(x)^":\n")) labels;
      (case value of
	   IntValue n => print (hxaddr ^"\t.intz\t" ^ (Int.toString n)^"\n")
	 | StrValue s => print (hxaddr^ "\t.asciiz\t\"" ^ s ^"\"\n")
	 | InstrValue i => print(hxaddr ^ (M.instr2string i)) 
      )
  end
      
fun print_program (start_addr, end_addr) =
  let val addr = ref(start_addr)
  in
      while (!addr < end_addr) do
	    (
	      print_mem_entry (!addr);
	      addr := !addr + off32
	    )
	    
  end


(*
* exec_instr: pc -> pc
*  executes a single mips instruction at location
*  returns the address of the next instruction to be executed
*  Instruction addresses are in the code area, and not the memory. 
*)
fun exec_instr (pc:int): int =
  let
      val instr = get_instr(pc)
      val get_imm = M.immed2int
	
      val npc = pc+off32
  in
      case instr of
	 M.Arith2(op2, rd, rs) => 
	 let
	     val vrs = get_reg rs
	     val result =
	       case op2 of M.Abs => abs vrs
			 | M.Neg => ~vrs
			 | M.Not => raise InstructionNotImplemented
	 in
	     set_reg (rd, result); npc
	 end
       | M.Arith3 (op3, rd, rs, rt) => 
	 let
	     val vrs = get_reg rs
	     val vrt = get_reg rt
	     val result =
		 case op3 of M.Add => vrs+vrt
			   | M.And => raise InstructionNotImplemented
			   | M.Mulo =>vrs+vrt
			   | M.Div => Int.div(vrs,vrt)
			   | M.Or =>  raise InstructionNotImplemented
			   | M.Rem => Int.rem(vrs,vrt)
			   | M.Sub => vrs-vrt
			   | M.Xor => raise InstructionNotImplemented
			   | M.Seq => if vrs = vrt then 1 else 0
			   | M.Slt => if vrs < vrt then 1 else 0
	 in
	     set_reg (rd, result); npc
	  end
       | M.Arithi (opi, rt, rs, immed) => 
	 let
	     val vrs = get_reg rs
	     val vim = get_imm immed
	     val result = 
		 case opi of M.Addi => vrs+vim
			   | M.Andi => raise InstructionNotImplemented
			   | M.Ori  => raise InstructionNotImplemented
			   | M.Xori => raise InstructionNotImplemented
	 in
	     set_reg (rt, result); npc
	 end	               

       | M.Li (reg, im)  => (set_reg (reg, get_imm im); npc)
       | M.La (reg, lab) => (set_reg (reg, get_addr lab); npc)
       | M.Lw (reg, (im, r)) =>
	 let
	     val addr = (get_imm im)+(get_reg r)
	     val value = get_mem_int addr
	 in
	     set_reg (reg, value); npc
	 end
       | M.Sw (reg, (im, r)) =>
	 let
	     val addr = (get_imm im)+(get_reg r)
	     val value = get_reg reg
	 in
	     set_mem_int (addr, value); npc
	 end
       | M.Move (reg1, reg2) => (set_reg (reg1, get_reg reg2); npc)
       | M.Branchz (comparecode, reg, lab) => 
	 let
	     val vreg = get_reg reg
	     val bpc = get_addr lab
	     val result =
		 case comparecode 
		  of M.Lt => vreg < 0
		   | M.Eq => vreg = 0
		   | M.Ne => vreg <> 0
		   | M.Ge => vreg >= 0
		   | M.Gt => vreg > 0
		   | M.Le => vreg <= 0
	 in
	     if result then bpc else npc
	 end

       | M.Branchu (comparecode, reg1, reg2, lab) => 
	 let
	     val vreg1 = get_reg reg1
	     val vreg2 = get_reg reg2
	     val bpc = get_addr lab
	     val result = 
		 case comparecode 
		  of M.Ltu => raise InstructionNotImplemented
		   | M.Geu => raise InstructionNotImplemented
		   | M.Gtu => raise InstructionNotImplemented
		   | M.Leu => raise InstructionNotImplemented
	 in
	     if result then bpc else npc
	 end

       | M.Branch (comparecode, reg1, reg2, lab) => 
	 let
	     val vreg1 = get_reg reg1
	     val vreg2 = get_reg reg2
	     val bpc = get_addr lab
	     val result =
		 case comparecode 
		  of M.Lt => vreg1 < vreg2
		   | M.Eq => vreg1 = vreg2 
		   | M.Ne => vreg1 <> vreg2
		   | M.Ge => vreg1 >= vreg2
		   | M.Gt => vreg1 > vreg2
		   | M.Le => vreg1 <= vreg2
	 in
	     if result then bpc else npc
	 end

       | M.J (lab) => get_addr lab
       | M.Jal (lab) =>
	 (* making a procedure call, push a frame*)
	 (set_reg ((M.reg "$ra"), npc);
	  push_frame();
	 get_addr lab)
       | M.Jalr (reg1, reg2, use, def)  =>
	 (* making a procedure call, push a frame*)
	 (set_reg (reg1, npc);
	  push_frame();
	 get_reg reg2)
       | M.Jr (reg,also) =>
	 (* returning from procedure, remove a frame *)
	 ((if (M.reg2name reg) = "$ra" then
	      pop_frame()
	  else ());
	 (get_reg reg))
       | M.Syscall =>
	 (exec_syscall();
	  npc)
       | M.Nop => npc
  end

val trace_on = ref(false)
fun set_trace_off () = trace_on := false
fun set_trace_on () = trace_on := true
fun is_trace_on () = !trace_on
			 
fun trace_instr (pc): unit =
  let
     val instr = get_instr(pc)
  in
     print (">" ^ Int.toString(pc) ^ "  " ^ M.instr2string(instr))
  end

val break_points: (int list) ref = ref([])

fun is_break addr =
  List.exists (fn x=> addr = x) (!break_points)

fun clear_break_points () = break_points := []
						
fun set_break_at_label labname =
  (let
      val addr = get_addr (S.symbol labname)
  in
      if not (is_break addr) then
	  (break_points := addr::(!break_points);
	   print ("break point at: " ^ (Int.toString addr) ^"\n"))
      else
	  print "Already a break point\n"
  end
   handle _ => print ("Label "^labname^" not found\n")
  )

fun set_break_at_addr addr =
  break_points := addr::(!break_points)
	      
exception InfiniteLoop
val pc = ref(0)
fun get_pc () = !pc
fun set_pc npc = pc := npc

fun exec_one (print_trace): unit =
  (pc := exec_instr(!pc);
   if print_trace then trace_instr (!pc) else ()
  )

fun exec_pgm (): unit =
  let
      val done = ref(false) (* termination flag *)
  in
      while not (!done) do
	    let
		val tpc = !pc
	    in
		exec_one(is_trace_on());
		(if tpc = (!pc) then raise InfiniteLoop else ());
		(* check for breakpoint *)
		(if is_break (!pc)
		 then
		     (print ("break at: ");
		     print_mem_entry (!pc);
		     done := true)
		 else
		     done := (!pc) < 0)
	    end
  end

(* ---- load strings and code in program memory -- *)

fun load_string_block (s: M.stringblock list): unit =
  (* s: is a list of string blocks
     each block is a pair of label and string
     insert it in the memory
   *)
   let
       val update = fn (lab,str) => (load_label lab; load_string str)
   in
       app update s
   end

fun load_funcode (f: M.codeblock list): unit =
  (* f: is a list of code blocks for one function
     each code block is a pair of label and a sequence of instructions
     load the label and instructions in memory
   *)
   let
       val update = fn (lab,instrs)=> (load_label lab; app load_instr instrs)
   in
       app update f	   
   end


 (* ------ load MIPS program in memory and execute it --- *)

 fun load_program (p:M.program) =
   let
       val (slist,flist) = p
   in
       (* reset memory and registers *)
       reset_mem ();
       reset_pgm ();
       (* load program in pgm memory *)
       load_string_block slist;
       app load_funcode flist;
       print "Loaded program in memory\n"
       (*print_program (0, next_load_address())*)
   end
       
 fun run ()  =
   let
       val mainpc = get_addr (S.symbol "main")
   in
       (* execute the program *)
       reset_regs ();
       set_pc mainpc;
       exec_pgm ()
   end
end
    
