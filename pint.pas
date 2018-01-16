(*
Copyright 1990 Alan Burns and Geoff Davies

This file is part of Pascal-FC.

Pascal-FC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Pascal-FC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Pascal-FC; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)



(* @(#)pint.p	5.3 12/3/92 *)


//program pint(objfile,pmdfile,input,output);
program pint(input,output);



(* Pascal-FC interpreter *)


const


(* implementation-independent constants *)
	(* @(#)globcons.i	4.1 10/24/89 *)

   alng=10; 		(* length of identifiers *)
   xmax = maxint;
   omax=200;            (* largest op-code for p-machine *)
   funmax = omax;	(* highest function number *)


(* implementation-dependent constants *)
(* impcons.i *)
(* BM 1 version *)


	target = 'IBM PC compatibles';

   maxmons=10;          (* maximum monitor in a program *)
   maxcapsprocs=10;     (* maximum exported procedures from a monitor *)
   intermax=10;         (* max no. of mapped ipc primitives *)
   tmax=150;            (* max size of symbol table *)
   bmax=50;             (* max size of block table *)
   amax=20;             (* max size of array table *)
   casemax=20;          (* max number of case labels or selects *)
   chanmax=20;          (* maximum size of channel table - gld *)
   cmax=2000;           (* max size of p-code array *)
   lmax=7;              (* max depth of block nesting *)
   smax=1500;           (* max size of string table *)
   rmax = 50;		(* real constant table limit *)
   etmax = 20;		(* enumeration type upper bounds table *)

   llng=121;		(* max source input line length *)
   tabstop=3;           (* for 1 implementation - gld *)
   tabchar = 9;

   fals = 0;
   tru = 1;
   charl=0;      	(* first legal ascii character *)
   charh=127;   	(* last legal ascii character *)

   intmax = 32767;	(* maximum integer on target *)
   intmsb = 16;		(* most sig. bit in target integer *)

   realmax = 1e38;	(* maximum real number on target 
										   or host, whichever is smaller *)
   minreal = 1e-37;	(* smallest real (for division) *)
   emax = 38;		(* maximum real exponent on target *)
   emin = -emax;

   bsmsb = 7;		(* most sig. bit in target bitset *)

   impfiles = false;
   impmapping = false;
   imptiming = false;
   impreals = true;

   monvarsize = 2;
   protvarsize = 3;
   chansize=3;
   entrysize = 3;       (* space for a process entry point *)
   sfsize=6;            (* size of "frame" in a select statement *)

   bitsetsize = 1;
   intsize = 1;
   boolsize = 1;
   charsize = 1;
   semasize = 1;
   condvarsize = 1;
   synchrosize = 0;
   procsize = 1;
   enumsize = 1;
   realsize = 1;

   objalign = 1;
   pushdown = false;

(* interpreter-specific constants *)

   stepmax=8;
   statmax=200000;      (* maximum statements before "livelock *)

(* NOTE - make (stmax - (stkincr * pmax)) >= stkincr *)

   stmax=5000;
   stkincr=200; 
   pmax=20;
   msb = 7;


   actrecsize = 5;	(* size of subprogram "housekeeping" block *)


type 

	(* @(#)globtypes.i	4.7 11/8/91 *)

   opcode=(ldadr,ldval,ldind,updis,cobeg,coend,wait,signal,stfun,ixrec,
      jmp,jmpiz,for1up,for2up,mrkstk,callsub,ixary,ldblk,cpblk,
      ldcon,ifloat,readip,wrstr,wrval,stop,retproc,retfun,repadr,notop,
      negate,store,relequ,relneq,rellt,relle,relgt,relge,orop,
      add,sub,andop,mul,divop,modop,rdlin,wrlin,selec0,chanwr,
      chanrd,delay,resum,enmon,exmon,mexec,mretn,
      lobnd,hibnd,pref,sleap,
      procv,ecall,acpt1,acpt2,rep1c,rep2c,btest,enmap,wrfrm,w2frm,
      wrsfm,wrbas,power2,slabl,blokk,param,case1,case2,selec1,
      sinit,prtex,prtjmp,prtsel,prtslp,prtcnd);

   index = -xmax .. xmax; 
   alfa = packed array[1..alng] of char;
   myobject = (konstant,variable,type1,prozedure,funktion,monproc,address,
             grdproc,xgrdproc); 

   types = (notyp,ints,reals,bools,chars,arrays,records,
      semafors,channels,monvars,condvars,synchros,adrs,
      procs,entrys,enums,bitsets,
      protvars,protq);

   typset = set of types;

   fnametype = packed array[1..30] of char;

   order =
   packed record 
      f: opcode;
      x: -lmax..+lmax; 
      y: integer;
		instyp: types;
		line: integer
   end; 
   orderarray = array[0..cmax] of order;

   objorder =
	packed record
		f: 0..omax;
		x: -lmax..lmax;
		y: integer;
		l: integer
	end;
   objorderarray = array[0..cmax] of objorder;

   tabrec =
	packed record
   		name: alfa;
   		link: index;
   		obj: myobject;
   		typ: types;
   		ref: index;
   		normal: boolean;
   		lev: 0..lmax;
   		taddr: integer;
			auxref: index
   	end;
	tabarray = array[0..tmax] of tabrec;

	atabrec =
  	packed record 
      	inxtyp,eltyp:types; 
      	inxref,elref,low,high,elsize,size:index; 
   	end; 
	atabarray = array[1..amax] of atabrec;

	btabrec =
  	packed record 
      	last,lastpar,psize,vsize:index;
      	tabptr: 0..tmax
   end; 
	btabarray = array[1..bmax] of btabrec;

	stabarray = packed array[0..smax] of char;
	realarray = array[1..rmax] of real;

   intabrec =
   packed record
      tp: types;
      lv:  0..lmax;
      rf: integer;
      vector: integer;
      off:  integer;
		tabref: integer
   end;
	intabarray = array[1..intermax] of intabrec;


(* unixtypes.i *)

(* Pascal-FC "universal" compiler system *)
(* implementation-dependent type declaration for Unix *)


   objcoderec =
	packed record
		fname:		fnametype;
		prgname:    alfa;
		gencode:    objorderarray;
		ngencode:   0..cmax;

		gentab:  tabarray;
		ngentab: 0..tmax;

		genatab:    atabarray;
		ngenatab: 0..amax;

		genbtab: btabarray;
		ngenbtab: 0..bmax;

		genstab: stabarray;
		ngenstab: 0..smax;
		genrconst: realarray;

      useridstart: 0..tmax;


	end;


   ptype=0..pmax;
	powerset = set of 0..bsmsb;

	qpointer = ^qnode;
	qnode =
	record
		proc: ptype;
		next: qpointer
	end;

	stackrec =
			record
				case tp: types of
					ints:		(i: integer);
					bitsets:	(bs: powerset);
					reals:		(r: real)
				end;


(* This type is declared within the GCP Run Time System *)
    UnixTimeType = LongInt;








var
	objfile: file of objcoderec;

	pmdfile: text;
	stantyps: typset;
	ch: char;


      ir: objorder; 
      ps:
			(run,fin,divchk,inxchk,charchk,stkchk,
   		redchk,deadlock,channerror,guardchk,queuechk, 
			procnchk,statchk,nexistchk, namechk,casechk,
			bndchk,instchk,inpchk,setchk,ovchk,seminitchk);

      lncnt,chrcnt:integer; 
      h1,h2,h3,h4:integer;
      h1r:real;
		foundcall: boolean;		(* used in select (code 64) *)

      s: array[1..stmax] of stackrec;
      ptab:array[ptype] of
      record
         t,b,pc,stackbase,stacksize:integer;
         display:array[1..lmax] of integer; 
         suspend:integer; 
         chans: integer;
			repindex: integer;
			onselect: boolean;
         active, termstate:boolean;
         curmon: integer;
         wakeup, wakestart: integer;
			clearresource:  boolean;

			varptr: 0..tmax

      end;
      npr, procmax, curpr:ptype;
      stepcount:integer;
      concflag: boolean;
      statcounter: 0..maxint;
		sysclock: 0..maxint;

        
        (* I declare them to be UnixTimeType (lognints) *)        
		now, last: UnixTimeType;

      procqueue: record
                  proclist: array [1..pmax] of
                      record
                        proc: ptype;
                        link: ptype
                      end;
                  free: 0..pmax
                end;

      eventqueue: record
                     first: qpointer;
                     time: integer
                  end;






   function itob(i:integer):boolean;
      begin 
         if i=tru 
         then itob:=true
         else itob:=false 
      end;


   function btoi(b:boolean):integer;
      begin 
         if b 
         then btoi:=tru 
         else btoi:=fals
      end;


		procedure printname(name: alfa; var tofile: text);

		var
			index:  integer;
			endfound: boolean;

		begin
			index := 1;
			endfound := name[index] = ' ';
			while not endfound do
				begin
				write(tofile,name[index]);
				index := index + 1;
				if index <= alng then
					endfound := (name[index] = ' ')
				else
					endfound := true
				end
		end;  (* printname *)


		procedure nameobj(target: integer;
								 var tp: types; var tofile: text);

		var
			tptr, procptr, offset, prtarget: integer;
			rf: index;




			procedure unselector(var rf: index; var tp: types);

			(* output array subscripts or record fields *)


				procedure arraysub(var rf: index; var tp: types);

				var
					sub: integer;

				begin
					write(tofile,'[');
					with  objfile^.genatab[rf] do
						begin
						sub := (offset div elsize) + low;
						offset := offset mod elsize;
						case inxtyp of
							ints,
							enums:		write(tofile,sub:1);
							chars:		write(tofile,'''',chr(sub),'''');
							bools:		write(tofile,itob(sub))
						end;
						write(tofile,']');
						tp := eltyp;
						rf := elref
						end
				end;  (* arraysub *)


				procedure recfield(var rf: index; var tp: types);

				var
					tptr: integer;


				begin
					write(tofile,'.');
					with objfile^ do
						begin
						tptr := genbtab[rf].last;
						while gentab[tptr].taddr > offset do
							tptr := gentab[tptr].link;
						printname(gentab[tptr].name,tofile);
						rf := gentab[tptr].ref;
						tp := gentab[tptr].typ;
						offset := offset - gentab[tptr].taddr
						end  (* with *)
				end;  (* recfield *)




			begin
				repeat
					if tp = arrays then
						arraysub(rf,tp)
					else
						recfield(rf,tp)
				until not (tp in [arrays,records])
			end;  (* unselector *)


			procedure followlinks(target: integer; var tptr, offset: integer);

			var
				dx: integer;

			begin
				with objfile^ do
					begin
					while gentab[tptr].obj <> variable do
						tptr := gentab[tptr].link;
					dx := gentab[tptr].taddr;
					while dx > target do
						begin
						tptr := gentab[tptr].link;
						if gentab[tptr].obj = variable then
							dx := gentab[tptr].taddr
						end  (* while *)
					end;  (* with *)
				offset := target - dx
			end;  (* followlkins *)

			procedure monitortyp(target: integer; var tptr, offset: integer);

			(* name monitor boundary queue, h-p queue
            or any variable declared in a monitor *)

			begin
				with objfile^ do
					begin
					printname(gentab[tptr].name,tofile);
					if offset = 0 then
						if gentab[tptr].typ = monvars then
							write(tofile,' (monitor boundary queue)')
						else
							write(tofile,' (resource boundary queue)')
					else
						if offset = 1 then
							begin  (* h-p queue *)
							printname(gentab[tptr].name,tofile);
							write(tofile,' (monitor high-priority queue)')
							end
						else
							begin  (* declared variable *)
							write(tofile,'.');
							tptr := genbtab[gentab[tptr].ref].last;
							followlinks(target,tptr,offset);
							if gentab[tptr].typ = protq then
								printname(gentab[tptr-1].name,tofile)
							else
								printname(gentab[tptr].name,tofile);
							rf := gentab[tptr].ref;
							tp := gentab[tptr].typ
							end
					end  (* with *)
			end;  (* monitortyp *)

			procedure entryname(bref: integer);


			var
				tptr: integer;


			begin
				write(tofile,'.');
				target := ((target - ptab[1].stackbase) mod stkincr);
				with objfile^ do
					begin
					tptr := genbtab[bref].last;
					followlinks(target,tptr,offset);
					printname(gentab[tptr].name,tofile);
					end  (* with *)
			end;  (* entryname *)



		begin  (* Nameobj *)
			if target > ptab[0].stacksize then
				begin
				procptr := ((target - ptab[1].b) div stkincr) + 1;
				prtarget := ptab[procptr].varptr
				end
			else
				prtarget := target;
			with objfile^ do
				begin
				tptr := genbtab[2].last;
				followlinks(prtarget,tptr,offset);
				rf := gentab[tptr].ref;
				tp := gentab[tptr].typ;
				if tp in [monvars,protvars] then
					monitortyp(target,tptr,offset)
				else
					printname(gentab[tptr].name,tofile)
				end;  (* with *)
			if tp in [arrays,records] then
				unselector(rf,tp);
			if target > ptab[0].stacksize then
				begin
				entryname(rf);
				tp := entrys
				end
		end;  (* nameobJ *)

        
	procedure getcode;

	(* get code from objfile *)

	begin

		reset(objfile)

	end;  (* getcode *)


	procedure putversion(var tofile: text);

	begin
		write(tofile,'- Interpreter Version P5.3');


		write(tofile,' - ')

	end;  (* putversion *)









		procedure headermsg(tp: types; var tofile: text);


		begin
      	with ptab[curpr] do 
				begin
         	write(tofile, 'Abnormal halt ');
				if active then
					begin
					if curpr = 0 then
						write(tofile,'in main program ')
					else
						begin
						write(tofile,'in process ');
						nameobj(varptr,tp,tofile)
						end;
					writeln(tofile,' with pc = ', pc:1)
					end
				else
					begin
					write(tofile,'on termination of process ');
					nameobj(varptr,tp,tofile);
					writeln(tofile)
					end
				end;
      	write(tofile,'Reason:   ');

      	case ps of
         	deadlock:  
            	writeln(tofile,'deadlock');
         	divchk:  
            	writeln(tofile,'division by 0'); 
         	inxchk:  
            	writeln(tofile,'invalid index ');
				charchk:
					writeln(tofile,'illegal or uninitialised character');
         	stkchk:  
            	writeln(tofile,'storage overflow');
         	redchk:  
            	writeln(tofile,'reading past end of file');
				channerror:
					writeln(tofile,'channel error');
				guardchk:
					writeln(tofile,'closed guards');
         	procnchk:
					writeln(tofile,'more than ',pmax:1,' processes');
         	statchk:
					writeln
						(tofile,'statement limit of ',statmax:1,
						' reached (possible livelock)');
				nexistchk:
					writeln(tofile,
					'attempt to call entry of non-existent/terminated process');
				namechk:
         		writeln(tofile,
					'attempt to make entry on process without unique name');
				casechk:
					writeln(tofile,
						'label of ',s[ptab[curpr].t].i:1,' not found in case');
				bndchk:
					writeln(tofile,'ordinal value out of range');
				instchk:
					writeln(tofile,'multiple activation of a process');
				inpchk:
					writeln(tofile,'error in numeric input');
				setchk:
					writeln(tofile,'bitset value out of bounds');
				ovchk:
					writeln(tofile,'arithmetic overflow');
				seminitchk:
					writeln(tofile,'attempt to initialise semaphore from process')
				end;  (* case *)

         	writeln(tofile); writeln(tofile)
		end;  (* headermsg *)


		procedure printyp(tp: types; var tofile: text);

		begin
			case tp of
				semafors:	writeln(tofile,' (semaphore)');
				condvars:	writeln(tofile,' (condition)');
				monvars,
				protvars:		writeln(tofile);
				channels:	writeln(tofile,' (channel)');
				entrys:		writeln(tofile,' (entry)');
				procs:		;
				protq:		writeln(tofile,' (procedure guard)')
			end
		end;  (* printyp *)



		procedure oneproc(nproc: integer);


		(* give pmd report on one process *)

		var
			tp: types;
			loop, frameptr, chanptr: integer;

		begin

			writeln(pmdfile,'----------');

			with ptab[nproc] do
				begin
				if nproc = 0 then

					writeln(pmdfile,'Main program')

				else
					begin

					write(pmdfile,'Process ');
					nameobj(varptr,tp,pmdfile);
					writeln(pmdfile)

					end;

				writeln(pmdfile);
				write(pmdfile,'Status:  ');

				if active then
					begin

					writeln(pmdfile,'active');
					writeln(pmdfile,'pc = ',pc:1)

					end
				else
					if nproc = 0 then

						writeln(pmdfile,'awaiting process termination')

					else

						writeln(pmdfile,'terminated');

				if termstate or (suspend <> 0) then
					begin

					writeln(pmdfile);
					writeln(pmdfile,'Process suspended on:');
					writeln(pmdfile);

            	if suspend > 0 then
               	begin

						nameobj(suspend,tp,pmdfile);
						printyp(tp,pmdfile)

						end
					else
						begin
						frameptr := chans;
						for loop := 1 to abs(suspend) do
							begin
							chanptr := s[frameptr].i;
							if chanptr <> 0 then	(* 0 means timeout *)
								begin

								nameobj(chanptr,tp,pmdfile);
								printyp(tp,pmdfile)

								end
							else

								writeln(pmdfile,'timeout alternative');

							frameptr := frameptr + sfsize
							end
						end;
					if termstate then

						writeln(pmdfile,'terminate alternative')

					end
				end;  (* with *)

			writeln(pmdfile); writeln(pmdfile)

		end;  (* oneproc *)



		procedure globals;


		(* print global variables *)

		var
			h1: integer;
			noglobals: boolean;

		begin
			noglobals := true;

			writeln(pmdfile);
			writeln(pmdfile,'==========');
			writeln(pmdfile,'Global variables');
			writeln(pmdfile);

			with objfile^ do
				begin
         	h1 := genbtab[2].last;
         	while gentab[h1].link <> 0 do
            	with gentab[h1] do  
               	begin
               	if obj=variable   then
                  	if typ in (stantyps + [semafors,enums]) then
								begin
								noglobals := false;
                     	case typ of
                        	ints, semafors, enums: 

                           	writeln(pmdfile,name,' = ',s[taddr].i);

									reals:

										writeln(pmdfile,name,' = ',s[taddr].r);

                        	bools:  

                           	writeln(pmdfile,name,' = ',itob(s[taddr].i));

                        	chars:  

                              writeln(pmdfile,name,' = ',chr(s[taddr].i mod 64));

                     	end   (* case *)
							end;  (* if *)
               		h1 := link
               	end  (* with gentab *)
					end;  (* with objfile^ *)
			if noglobals then

				writeln(pmdfile,'(None)')

		end;  (* globals *)





   procedure expmd;

   (* print post-mortem dump on execution-time error *)

	var
		h1: integer;
		tp: types;






   begin  (* Expmd *)
		rewrite(pmdfile);
		write(pmdfile,'Pascal-FC post-mortem report on ');
		printname(objfile^.prgname,pmdfile);
		writeln(pmdfile);
		putversion(pmdfile);
		writeln(pmdfile);
		headermsg(tp,pmdfile);
		headermsg(tp,output);
		writeln;
		writeln('See pmdfile for post-mortem report');
      for h1 := 0 to procmax do

			oneproc(h1);
		if (curpr <> 0) or (ps <> stkchk) then
			globals

   end ; (* expmd *)



(* real-time clock management module *)


{ Get the real time. MicroSecond can be Null and is ignored then. }
//function  GetUnixTime (var MicroSecond: Integer): UnixTimeType;
//  asmname '_p_GetUnixTime';

function GetUnixTime (var MicroSecond: Integer): UnixTimeType;
begin
    MicroSecond := 1;
end;


	procedure initclock;

    var
        microsecs : integer;
    
	begin
        microsecs := 0;
		sysclock := 0;
		last := GetUnixTime(microsecs)
	end;  (* initclock *)


	procedure checkclock;

    var
        microsecs : integer;
    
	begin
		now := GetUnixTime(microsecs);
		if now <> last then
			begin
			last := now;
			sysclock := sysclock + 1
			end
	end;  (* checkclock *)

	procedure doze(n: integer);

	begin
		while eventqueue.time > sysclock do
			checkclock
	end;  (* doze *)



	procedure runprog;

	(* execute program once *)

	label
      97,98;


	procedure getqueuenode(pnum: ptype; var ptr: qpointer);

	(* place pnum in a dynamic queue node *)

	begin
		new(ptr);
		with ptr^ do
			begin
			proc := pnum;
			next := nil
			end
	end;  (* getqueuenode *)



   procedure joineventq(waketime: integer);

   (* join queue of processes which have executed a "sleep" *)


   var
      thisnode, frontpointer, backpointer: qpointer;
      foundplace: boolean;

   begin
		with ptab[curpr] do
			begin
			wakeup := waketime;
			if wakestart = 0 then
				wakestart := pc
			end;
      stepcount := 0;
      getqueuenode(curpr,thisnode);
		with eventqueue do
			begin
			frontpointer := first;
			if frontpointer <> nil then
				begin
				backpointer := nil;
				foundplace := false;
				while not foundplace and (frontpointer <> nil) do
					if ptab[frontpointer^.proc].wakeup > waketime then
						foundplace := true
					else
						begin
						backpointer := frontpointer;
						frontpointer := backpointer^.next
						end;
					thisnode^.next := frontpointer;
					if backpointer <> nil then
						backpointer^.next := thisnode
				end;  (* if first <> nil *)
			if frontpointer = first then
					begin
					first := thisnode;
					time := waketime
					end
			end  (* with eventqueue *)
   end;  (* joineventq *)


	procedure leventqueue(pnum: ptype);

	(* process pnum is taken from event queue *)
	(* (a rendezvous has occurred before a timeout alternative expires) *)

	var
		frontpointer, backpointer: qpointer;
		found: boolean;

	begin
		with eventqueue do
			begin
			frontpointer := first;
			backpointer := nil;
			found := false;
			while not found and (frontpointer <> nil) do
				if frontpointer^.proc = pnum then
					found := true
				else
					begin
					backpointer := frontpointer;
					frontpointer := frontpointer^.next
					end;
			if found then
				begin
				if backpointer = nil then
					begin
					first := frontpointer^.next;
					if first <> nil then
						time := ptab[first^.proc].wakeup
					else
						time := 0
					end
				else
					backpointer^.next := frontpointer^.next;
				dispose(frontpointer)
				end  (* if found *)
			end  (* with eventqueue *)
		end;  (* leventqueue *)


      procedure alarmclock; forward;

      procedure chooseproc;

      (* modified to permit a terminate option on select - gld *)

      var d: integer;
          procindex: integer;
          foundproc, procwaiting: boolean;

      begin 
         foundproc:=false;
         repeat
            procwaiting:=false;
            d:=procmax+1; 

            procindex:=(curpr+trunc(random*procmax))mod(procmax+1); 

            while not foundproc and (d>=0) do
               with ptab[procindex] do
                    begin
                  foundproc := active and (suspend=0) and
                     (wakeup=0) and not termstate;
                  if not foundproc then
                     begin
                     if active and not termstate then
                        procwaiting := true;
                     d:=d-1;procindex:=(procindex+1) mod (procmax+1) 
                     end  (* if not foundproc *)
                  end;  (* with *)
            if not foundproc then
               if procwaiting then
                  if eventqueue.first <> nil then
							begin
							doze(eventqueue.time - sysclock);
                     alarmclock
							end
                  else
                     begin 
                     ps:=deadlock;goto 98 
                     end 
               else
                  ptab[0].active := true
            else
               begin
               curpr := procindex;
               stepcount:=trunc(random*stepmax) 
               end
         until foundproc or (ps <> run)
      end;  (* chooseproc *)



		procedure clearchans(pnum, h: integer);

		(* clear all channels on which the process sleeps *)

		var
			loop, nchans, frameptr, chanptr: integer;

		begin
			with ptab[pnum] do
				begin
         	nchans := abs(suspend);
				frameptr := chans;
         	for loop := 1 to nchans do
            	begin
					chanptr := s[frameptr].i;
					if chanptr <> 0 then  (* timeout if 0 *)
						begin
            		s[chanptr].i := 0;
            		if chanptr = h then
							if onselect then
								begin
               			repindex := s[frameptr+5].i;
								onselect := false
								end
						end;
					frameptr := frameptr + sfsize
            	end;
				chans := 0;
         	suspend:=0;
         	termstate:=false;
				end  (* with *)
		end;  (* clearchans *)



   procedure wakenon(h: integer);

   (* awakens the process asleep on this channel *)
   (* also used to wake a process asleep on several entries
      in a select statement, where it cannot be in a queue *)

   var procn: integer;

   begin
      procn := s[h+2].i;
      with ptab[procn] do
         begin
			clearchans(procn,h);
			leventqueue(procn);
			wakeup := 0;
         pc:=s[h+1].i

         end  (* with ptab[procn] *)

   end;  (* wakenon *)


   procedure initqueue;

   (* initialise process queue *)

   var index: 1..pmax;

   begin  (* initqueue *)
      with procqueue do
         begin
         free:=1;
         for index :=1 to pmax-1 do
            proclist[index].link:=index+1;
         proclist[pmax].link:=0
         end  (* with *)
   end;  (* initqueue *)


   procedure getnode(var node: ptype);

   (* get a node from the free list for process queues *)
   (* the link is set to zero *)

   begin  (* getnode *)
      with procqueue do
         if free=0 then
            ps := queuechk
         else
            begin
            node := free;
            free := proclist[node].link;
            proclist[node].link := 0
            end
   end;  (* getnode *)


   procedure disposenode(node: ptype);

   (* return monitor queue node to free list *)

   begin  (* disposenode *)
      with procqueue do
         begin
         proclist[node].link := free;
         free := node
         end
   end;  (* disposenode *)


   procedure joinqueue(add: integer);

   (* join a process queue *)
   (* add is the stack address of the condvar or monvar *)

   var newnode, temp: ptype;

   begin  (* joinqueue *)
      ptab[curpr].suspend := add;
      stepcount := 0;
      getnode(newnode);
      procqueue.proclist[newnode].proc := curpr;
      if s[add].i < 1 then
         s[add].i := newnode
      else
         begin
         temp:= s[add].i;
         with procqueue do
            begin
            while proclist[temp].link <> 0 do
               temp := proclist[temp].link;
            proclist[temp].link := newnode
            end
         end
   end;  (* joinqueue *)
         





      procedure alarmclock;

      (* wake processes on event queue *)

      var
         now: integer;
         frontpointer, backpointer: qpointer;
         finished: boolean;

      begin
         now := eventqueue.time;
         finished := false;
         with eventqueue do
				begin
				frontpointer := first;
            while (frontpointer <> nil) and not finished do
               begin
					with ptab[frontpointer^.proc] do
						begin
						clearchans(frontpointer^.proc,0);
						wakeup := 0;
						pc := wakestart;
						wakestart := 0
						end;
					backpointer := frontpointer;
					frontpointer := frontpointer^.next;
					dispose(backpointer);
					if frontpointer <> nil then
						finished := ptab[frontpointer^.proc].wakeup <> now
               end;  (* while *)
				first := frontpointer;
				if frontpointer = nil then
					time := 0
				else
					time := ptab[frontpointer^.proc].wakeup
				end (* with eventqueue *)
      end;  (* alarmclock *)



   procedure procwake(add: integer);

   (* wakes the first process in a monitor queue *)
   (* add is the stack address of the condvar or monvar *)

   var pr, node: ptype;

   begin  (* procwake *)
      if s[add].i>0 then
         begin
         node := s[add].i;
         pr := procqueue.proclist[node].proc;
         s[add].i := procqueue.proclist[node].link;
         disposenode(node);

         ptab[pr].suspend := 0
         end
   end;  (* procwake *)
      
   
   procedure releasemon(curmon: integer);

   (* release mutual exclusion on a monitor *)


   begin

      if s[curmon+1].i > 0 then
         procwake(curmon+1)
      else
         if s[curmon].i > 0 then
               begin
               procwake(curmon);
               if s[curmon].i = 0 then
                  s[curmon].i := -1
               end
         else
            s[curmon].i := 0
   end;  (* releasemon *)


	procedure skipblanks;

	begin
		while  not eof and (input^ = ' ') do
			get(input)
	end;  (* skipblanks *)



	procedure readunsignedint(var inum: integer;  var numerror: boolean);

	var
		digit: integer;

	begin  (* Readunsignedint *)
      inum := 0;
      numerror := false;
      repeat 
         if inum > (intmax div 10) then
            numerror := true
         else
            begin
            inum := inum*10;
            digit := ord(input^) - ord('0');
            if digit > (intmax - inum) then
               numerror := true
            else
               inum := inum + digit
            end;
         get(input)
      until not (input^ in['0'..'9']); 
      if numerror then
      	inum := 0
	end;  (* readunsignedint *)



	procedure readbasedint(var inum: integer;  var numerror: boolean);

	(* on entry inum has been set by unsignedint *)

	var
		digit, base: integer;
		negative: boolean;

	begin
         get(input);
         if (inum in [2, 8, 16]) then
				base := inum
			else
				begin
				base := 16;
				numerror := true
				end;
			inum := 0; negative := false;
         repeat
            if negative then
               numerror := true
            else
               begin
               if inum > (intmax div base) then
                  begin
						if inum <= (intmax div (base div 2)) then
                  	negative := true
						else
							numerror := true;
                  inum := inum mod (intmax div base + 1)
                  end;
               inum := inum*base;
               if input^ in ['0'..'9'] then
                  digit := ord(input^) - ord('0')
               else
						if input^ in ['A'..'Z'] then
							digit := ord(input^) - ord('A') + 10
						else
							if input^ in ['a'..'z'] then
                        		digit := ord(input^) - ord('a') + 10
							else
								numerror := true;
               if digit >= base then
                  numerror := true
               else
                  inum := inum + digit
               end;
            get(input)
         until not (input^ in ['0'..'9','A'..'Z','a'..'z']);
         if negative then
				if inum = 0 then
					numerror := true
				else
					inum := (-maxint + inum) - 1;
         if numerror then
            inum := 0
	end;  (* readbasedint *)


	procedure findstart(var sign: integer);

	(* find start of integer or real *)

	begin
		skipblanks;
		if eof then
			ps := redchk
		else
			begin
			sign := 1;
			if input^ = '+' then
				get(input)
			else
				if input^ = '-' then
					begin
					get(input);
					sign := -1
					end
			end
	end;  (* findstart *)




	procedure readint(var inum: integer);

	var
		sign: integer;
		numerror: boolean;

	begin  (* Readint *)
		findstart(sign);
		if not eof then
			begin
			if input^ in ['0'..'9'] then
				begin
				readunsignedint(inum,numerror);
				inum := inum * sign;
				if input^ = '#' then
					readbasedint(inum,numerror)
				end
			else
				numerror := true;
			if numerror then
				ps := inpchk
			end
	end;  (* readint *)



	procedure readscale(var e: integer; var numerror: boolean);

	var
		s, sign, digit: integer;

	begin
		get(input);
		sign := 1; s := 0;
		if input^ = '+' then
			get(input)
		else
			if input^ = '-' then
				begin
				get(input); 
				sign := -1
				end;
		if not (input^ in ['0'..'9']) then
			numerror := true
		else
			repeat
				if s > (intmax div 10) then
					numerror := true
				else
					begin
					s := 10*s;
					digit :=  ord(input^) - ord('0');
					if digit > (intmax - s) then
						numerror := true
					else
						s := s + digit
					end;
				get(input)
			until not (input^ in ['0'..'9']);
		if numerror then
			e := 0
		else
			e := s*sign + e
	end;  (* readscale *)


	procedure adjustscale(var rnum: real; k,e: integer; var numerror: boolean);

	var
		s: integer;
		d, t: real;

	begin
	 	if (k + e) > emax then
			numerror := true
		else
				begin
				while e < emin do
					begin
					rnum := rnum/10.0;
					e := e + 1
					end;
				s := abs(e); t := 1.0; d := 10.0;
				repeat
					while not odd(s) do
						begin
						s := s div 2; d := sqr(d)
						end;
					s := s - 1; t := d*t
				until s = 0;
				if e >= 0 then
					if rnum > (realmax/t) then
						numerror := true
					else
						rnum := rnum *t
				else
					rnum := rnum/t
				end
	end;  (* adjustscale *)


	procedure readreal(var rnum: real);

	var
		k,e, sign, digit: integer;
		numerror: boolean;

	begin
		numerror := false;
		findstart(sign);
		if not eof then
			if input^ in ['0'..'9'] then
				begin
				while input^ = '0' do
					get(input);
				rnum := 0.0; k := 0; e := 0;
				while input^ in ['0'..'9'] do
					begin
					if rnum > (realmax/10.0) then
						e := e + 1
					else
						begin
						k := k + 1;
						rnum := rnum * 10.0;
						digit := ord(input^) - ord('0');
						if digit <= (realmax - rnum) then
							rnum := rnum + digit
						end;
					get(input)
					end;
				if input^ = '.' then
					begin  (* fractional part *)
					get(input);
					repeat
						if input^ in ['0'..'9'] then
							begin
							if rnum <= (realmax/10.0) then
								begin
								e := e - 1;
								rnum := 10.0*rnum;
								digit := ord(input^) - ord('0');
								if digit <= (realmax - rnum) then
									rnum := rnum + digit
								end;
							get(input)
							end
						else
							numerror := true
					until not (input^ in ['0'..'9']);
					if input^ in ['e','E'] then readscale(e,numerror);
					if e <> 0 then adjustscale(rnum,k,e,numerror)
					end  (* fractional part *)
				else
					if input^ in ['e','E'] then
						begin
						readscale(e,numerror);
						if e <> 0 then adjustscale(rnum,k,e,numerror)
						end
					else
						if e <> 0 then
							numerror := true;
				rnum := rnum * sign
				end
			else
				numerror := true;
		if numerror then
			ps := inpchk
	end;  (* readreal *)







begin (* Runprog *) 
	stantyps := [ints,reals,chars,bools];
      writeln;
      writeln('Program ',objfile^.prgname,'...  execution begins ...');
      writeln; writeln;
      initqueue;
      s[1].i:=0;s[2].i:=0;s[3].i:=-1;s[4].i:=objfile^.genbtab[1].last;
      with ptab[0] do 
      begin 
         stackbase := 0; b:=0;suspend:=0;display[1]:=0; 
			pc:=objfile^.gentab[s[4].i].taddr;
         active:=true; termstate:=false;stacksize:=stmax-pmax*stkincr;
         curmon := 0; wakeup := 0; wakestart := 0;
			onselect := false;
         t:=objfile^.genbtab[2].vsize-1;
			if t > stacksize then
				begin
				ps := stkchk;
				goto 98
				end;
         for h1:=5 to t do s[h1].i:=0
      end;
      for curpr := 1 to pmax do with ptab[curpr] do 
         begin
            active := false; termstate:=false;
            display[1] := 0; pc := 0; suspend := 0;  curmon := 0;
            wakeup := 0; wakestart := 0;
            stackbase := ptab[curpr-1]. stacksize+1; 
				b := stackbase;
				stacksize := stackbase + stkincr-1;
            t:= b-1 ;
				onselect := false;
				clearresource := true
         end; 
      npr :=0; procmax := 0; curpr:=0;
      stepcount:=0;
      ps := run; lncnt:=0;chrcnt:=0;
      concflag:=false;
      statcounter := 0;
		initclock;

      with eventqueue do
         begin
         first := nil;
         time := -1
         end;


      repeat
         if (ptab[0].active) and (ptab[0].suspend = 0) and (ptab[0].wakeup=0) then
            curpr := 0
         else 
            if stepcount = 0  then
               chooseproc
            else 
               stepcount := stepcount - 1; 
         with ptab[curpr] do
            begin

            ir := objfile^.gencode[pc];

            pc:=pc+1

            end; 
         if concflag then curpr := npr;
   
         with ptab[curpr] do
         case ir.f of 

            0:  
               begin
                  (*load address*) t:=t+1;
                  if t > stacksize
                  then ps := stkchk 
                  else s[t].i := display[ir.x]+ir.y 
               end; 

            1:  
               begin
                  (*load value*) t:= t +1;
                  if t > stacksize
                  then ps := stkchk 

                  else s[t] := s[display[ir.x]+ir.y]

               end; 

            2:  
               begin
                  (*load indirect*) t := t +1;
                  if t > stacksize
                  then ps := stkchk 
                  else s[t] := s[s[display[ir.x]+ir.y].i]
               end; 

            3:  
               begin
                  (*update display*)
                  h1 := ir.y;h2:=ir.x;h3:=b;
                  repeat
                     display[h1] := h3;h1:=h1-1;h3:=s[h3+2] .i
                  until h1 = h2 
               end; 

            4:  
               (*cobegin*)
					;

            5:  
               (*coend*)
               begin

                  procmax := npr;
                  ptab[0].active := false;
						stepcount := 0
               end; 

            6:  
               begin
                  (*wait*)
                  h1 := s[t].i;t:= t - 1; 

                  if s[h1].i>0
                  then s[h1].i := s[h1].i - 1 

                  else
                     begin
                        suspend:= h1; stepcount := 0
                     end
               end; 

            7:  
               begin
                  (*signal*)
                  h1 := s[t].i;t:=t-1;h2:=pmax+1;
						h3:=trunc(random*h2); 
                  while (h2>=0) and (ptab[h3].suspend<> h1) do
                     begin
                        h3:=(h3+1) mod (pmax+1); h2:= h2-1
                     end; 

                  if h2<0 
                  then s[h1].i := s[h1].i + 1 
                  else ptab[h3].suspend := 0

               end; 

            8:  
               case ir.y of 
						0:	
							s[t].i := abs(s[t].i);
						1:	
							s[t].r := abs(s[t].r);
						2:	  (* integer sqr *)
							if (intmax div abs(s[t].i)) < abs(s[t].i) then
								ps := ovchk
							else
								s[t].i := sqr(s[t].i);
						3:	  (* real sqr *)
							if (realmax/abs(s[t].r)) < abs(s[t].r) then
								ps := ovchk
							else
								s[t].r := sqr(s[t].r);
						4:	
							s[t].i := btoi(odd(s[t].i));
						5:	if not(s[t].i in [charl..charh]) then
								ps := charchk;
						6:	;
						7:	(* succ *)
							s[t].i := s[t].i + 1;
						8: (* pred *)
							s[t].i := s[t].i - 1;
						9:	  (* round *)
							if abs(s[t].r) >= (intmax + 0.5) then
								ps := ovchk
							else
								s[t].i := round(s[t].r);
						10:	(* trunc *)
							if abs(s[t].r) >= (intmax + 1.0) then
								ps := ovchk
							else
								s[t].i := trunc(s[t].r);
						11:
							s[t].r := sin(s[t].r);
						12:
							s[t].r := cos(s[t].r);
						13:
							s[t].r := exp(s[t].r);
						14:  (* ln *)
							if s[t].r <= 0.0 then
								ps := ovchk
							else
								s[t].r := ln(s[t].r);
						15:  (* sqrt *)
							if s[t].r < 0.0 then
								ps := ovchk
							else
								s[t].r := sqrt(s[t].r);
						16:
							s[t].r := arctan(s[t].r);
						
                  17: 
                     begin
                        t:= t +1; 
                        if t > stacksize  
                        then ps := stkchk 
                        else s[t].i := btoi(eof(input)) 
                     end; 

                  18: 
                     begin
                        t := t + 1; 
                        if t > stacksize  
                        then ps := stkchk 
                        else s[t].i := btoi(eoln(input))
                     end; 
                  19:
                     begin
                     h1 := abs(s[t].i) + 1;
                     s[t].i := trunc(random*h1)
                     end;
                  20:	(* empty *)
                     begin
                     h1 := s[t].i;
                     if s[h1].i=0 then
                        s[t].i := 1
                     else
                        s[t].i := 0
                     end;  (* f21 *)
						21:	(* bits *)
							begin
							h1 := s[t].i;
							s[t].bs := [];
							h3 := 0;
							if h1 < 0 then
								if bsmsb < intmsb then
									begin
									ps := setchk;
									h1 := 0
									end
								else
									begin
									s[t].bs := [bsmsb];
									h1 := (h1 + 1) + maxint;
									h3 := 1
									end;
							for h2 := 0 to bsmsb-h3 do
								begin
								if (h1 mod 2) = 1 then
									s[t].bs := s[t].bs + [h2];
								h1 := h1 div 2
								end;
							if h1 <> 0 then
								ps := setchk
							end;  (* f21 *)

						24:	(* int - bitset to integer *)
								begin
								h1 := 0;
								if bsmsb = intmsb then
									if intmsb in s[t].bs then
										h1 := 1;
								h2 := 0;  (* running total *)
								h3 := 1;  (* place value *)
								for h4 := 0 to bsmsb-h1 do
									begin
									if h4 in s[t].bs then
										h2 := h2 + h3;
									h3 := h3*2
									end;
								if h1 <> 0 then
									s[t].i := (h2-maxint) - 1
								else
									s[t].i := h2
								end;

						25:  (* clock *)
							begin
							t := t + 1;
							if t > stacksize then
								ps := stkchk
							else
								s[t].i := sysclock
							end;  (* f25 *)

               end; 

       9: 
                s[t].i := s[t].i + ir.y;

            10: 
               pc := ir.y;

            (*jump*)
            11: 
               begin
                  (*conditional jump*)
                  if s[t].i = fals
                  then pc := ir.y; t:= t-1; 
               end; 

				12:	(* case1 *)
					if s[t].i = s[t-1].i then
						begin
						t := t - 2;
						pc := ir.y
						end
					else
						t := t - 1;

				13:	(* case 2 *)
					ps := casechk;

            14: 
               begin
                  (*for1up*) h1 := s[t-1].i;
                  if h1 <= s[t].i 
                  then s[s[t-2].i].i := h1
                  else
                     begin
                        t := t-3; pc := ir.y
                     end
               end; 

            15: 
               begin
                  (*for2up*) h2 := s[t-2].i; h1 := s[h2].i +1;
                  if h1 <= s[t].i
                  then
                     begin
                        s[h2].i := h1; pc := ir.y 
                     end
                  else t := t-3;
               end; 

            18: 
               begin
               if ir.x = 1 then
                  begin  (* process *)
                  if npr = pmax then 
                     begin
                     ps := procnchk;
                     goto 98
                     end 
                  else 
                     begin
                     npr := npr + 1; 
                     concflag:=true;
                     curpr := npr;
                     end
                  end;
                  h1 := objfile^.genbtab[objfile^.gentab[ir.y].ref].vsize;
                  with ptab[curpr] do
                     begin
                     if t+h1 > stacksize  then
                        ps := stkchk 
                     else
                        begin
                        t := t+5; s[t-1].i := h1-1; s[t].i := ir.y
                        end
                     end  (* with *)
               end; 

            19: 
               begin
                  h1 := t-ir.y; 
                  h2 := s[h1+4].i; (*h2 points to tab*) 
                  h3 := objfile^.gentab[h2].lev; display[h3+1] := h1; 
                  h4 := s[h1+3].i+h1; 
                  s[h1+1].i := pc; s[h1+2].i := display[h3];
                  if ir.x=1 then
                     begin  (* process *)
                     active := true;
                     s[h1+3].i := ptab[0].b ;
                     concflag := false
                     end
                  else
                     s[h1+3].i := b;
                  for h3 := t+1 to h4 do s[h3].i := 0;
                  b := h1; t := h4; pc := objfile^.gentab[h2].taddr 
               end; 

            21: 
					with objfile^ do
               	begin
                  (*index*) h1 := ir.y; (*h1 points to genatab*) 
                  h2 := genatab[h1].low; h3 := s[t].i; 
                  if h3 < h2
                  then ps := inxchk 
                  else
                  if h3 > genatab[h1].high 
                  then ps := inxchk 
                  else
                     begin
                        t := t-1; s[t].i := s[t].i + (h3-h2)*genatab[h1].elsize
                     end
               end; 

            22: 
               begin
                  (*load block*) h1 := s[t].i; t := t-1;
                  h2 := ir.y+t; 
                  if h2 > stacksize 
                  then ps := stkchk 
                  else
                     while t < h2 do
                        begin 
                           t := t+1; s[t] := s[h1]; h1 := h1+1
                        end 
               end; 

            23: 
               begin
                  (*copy block*) h1 := s[t-1].i;
                  h2 := s[t].i; h3 := h1+ir.y;
                  while h1 < h3 do 
                     begin
                        s[h1] := s[h2]; h1 := h1+1;h2 := h2+1 
                     end; 
                  t := t-2
               end; 

            24: 
               begin
                  (*literal*) t := t+1; 
                  if t > stacksize
                  then ps := stkchk 
                  else s[t].i := ir.y 
               end; 

				25:
					begin
					t := t + 1;
					if t > stacksize then
						ps := stkchk
					else
						s[t].r := objfile^.genrconst[ir.y]
					end;

				26:
					begin  (* float *)
					h1 := t - ir.y;
					s[h1].r := s[h1].i
					end;





            27: 
               begin
                  (*read*)
                  if eof(input) 
                  then ps := redchk 
                  else
                     case ir.y of 
                        1:    (* integer *)
                           readint(s[s[t].i].i); 

                        3:    (* char *)
									if eof then
										ps := redchk
									else
                           	begin
                              read(ch);s[s[t].i].i := ord(ch) 
                           	end; 
								4:  (* real *)
									readreal(s[s[t].i].r)
                     end; 
                  t := t-1
               end; 

            28: 
               begin
                  (*write string*)
						if ir.x = 1 then
							begin
							h3 := s[t].i;
							t := t - 1
							end
						else
							h3 := 0;
                  h1 := s[t].i; h2 := ir.y;t := t-1;
                  chrcnt := chrcnt+h1+h3;
						while h3 > h1 do
							begin
							write(' ');
							h3 := h3 - 1
							end;
                  repeat
                     write(objfile^.genstab[h2]);h1 := h1-1; h2 := h2+1 
                  until h1=0
               end; 

            29: 
               begin
               case ir.y of 
                  1:    (* ints *)
                     write(s[t].i); 
                  2:  (* bools *)
                     write(itob(s[t].i)); 
                  3:    (* chars *)
                     if (s[t].i < charl) or (s[t].i > charh) then
                        ps := charchk
                     else
								write(chr(s[t].i));
						4:  (* reals *)
							write(s[t].r);
						5:  (* bitsets *)
								for h1 := bsmsb downto 0 do
									if h1 in s[t].bs then
										write('1')
									else
										write('0')
               end;   (* case *)
               t := t-1
               end;   (* s9 *)

				30:
					begin  (* write formatted *)
					h3 := s[t].i;  (* field width *)
					t := t - 1;
						case ir.y of
							1:	
								write(s[t].i:h3);  (* ints *)
							2:
								write(itob(s[t].i):h3);  (* bools *)
							3:
								if (s[t].i < charl) or (s[t].i > charh) then
									ps := charchk
							else
								write(chr(s[t].i):h3);
							4:	write(s[t].r:h3);
							5:
								begin
								while h3 > (bsmsb + 1) do
									begin
									write(' ');
									h3 := h3 - 1
									end;
								for h1 := bsmsb downto 0 do
									if h1 in s[t].bs then
										write('1')
									else
										write('0')
								end
						end;  (* case *)
					t := t - 1
					end;  (* 30 *)

            31: 
               ps := fin; 

            32: 
               begin
                  t := b -1; pc:= s[b+1].i; 
                  if pc <> 0
                  then b := s[b+3].i
                  else
                     begin
                        npr := npr -1; active := false; 
                        stepcount := 0; ptab[0].active := (npr = 0);

                     end
               end; 

            33: 
               begin
                  (* exit function *) 
                  t:= b; pc := s[b+1].i; b:= s[b+3].i;
               end; 

            34: 

               s[t] := s[s[t].i]; 


            35: 
               s[t].i := btoi(not(itob(s[t].i))); 

            36: 
               s[t].i := -s[t].i; 

				37:
					begin		(* formatted reals output *)
					h3 := s[t-1].i;
					h4 := s[t].i;
					write(s[t-2].r:h3:h4);
					t := t - 3
					end;

            38: 

               begin
                  (*store*) s[s[t-1].i] := s[t]; t:= t-2; 

               end; 

				39:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r = s[t+1].r)
					end;

				40:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r <> s[t+1].r)
					end;

				41:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r < s[t+1].r)
					end;

				42:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r <= s[t+1].r)
					end;

				43:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r > s[t+1].r)
					end;

				44:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].r >= s[t+1].r)
					end;


            45: 
               begin
                  t:= t-1; s[t].i := btoi(s[t].i = s[t+1].i); 
               end; 

            46: 
               begin
                  t := t -1; s[t].i := btoi(s[t].i <> s[t+1].i);
               end; 

            47: 
               begin
                  t := t -1; s[t].i := btoi(s[t].i < s[t+1].i); 
               end; 

            48: 
               begin
                  t := t -1; s[t].i := btoi(s[t].i <= s[t+1].i);
               end; 

            49: 
               begin
                  t := t -1; s[t].i := btoi(s[t].i > s[t+1].i); 
               end; 

            50: 
               begin
                  t := t -1; s[t].i := btoi(s[t].i >= s[t+1].i);
               end; 

            51: 
               begin
                  t := t -1; s[t].i := btoi(itob(s[t].i) or itob(s[t+1].i));  
               end; 

            52: 
               begin
               t := t -1; 
					if ((s[t].i > 0) and (s[t+1].i > 0)) or
						((s[t].i < 0) and (s[t+1].i < 0)) then
						if (maxint - abs(s[t].i)) < abs(s[t+1].i) then
							ps := ovchk;
					if ps <> ovchk then
						s[t].i := s[t].i + s[t+1].i; 
               end; 

            53: 
               begin
               t := t -1; 
					if ((s[t].i < 0) and (s[t+1].i > 0)) or
						((s[t].i > 0) and (s[t+1].i < 0)) then
						if  (maxint - abs(s[t].i)) < abs(s[t+1].i) then
							ps := ovchk;
					if ps <> ovchk then
						s[t].i := s[t].i - s[t+1].i; 
               end; 

				54:
					begin
					t := t - 1;
					if ((s[t].r > 0.0) and (s[t+1].r > 0.0)) or
						((s[t].r < 0.0) and (s[t+1].r < 0.0)) then
							if (realmax - abs(s[t].r)) < abs(s[t+1].r) then
								ps := ovchk;
					if ps <>  ovchk then
						s[t].r := s[t].r + s[t+1].r
					end;

				55:
					begin
					t := t - 1;
					if ((s[t].r > 0.0) and (s[t+1].r < 0.0)) or
								  ((s[t].r < 0.0) and (s[t+1].r > 0.0)) then
									  if (realmax - abs(s[t].r)) < abs(s[t+1].r) then
								ps := ovchk;
					if ps <>  ovchk then
						s[t].r := s[t].r - s[t+1].r
					end;

            56: 
               begin
                  t := t -1; s[t].i := btoi(itob(s[t].i) and itob(s[t+1].i)); 
               end; 

            57: 
               begin
               t := t -1; 
					if s[t].i <> 0 then
						if (maxint div abs(s[t].i)) < abs(s[t+1].i) then
							ps := ovchk;
					if ps <> ovchk then
						s[t].i := s[t].i * s[t+1].i; 
               end; 

            58: 
               begin
                  t := t -1;
                  if s[t+1].i = 0 
                  then ps := divchk 
                  else
                     s[t].i := s[t].i div s[t+1].i; 
               end; 

            59: 
               begin
                  t := t -1;
                  if s[t+1].i = 0
                  then ps := divchk 
                  else
                     s[t].i := s[t].i mod s[t+1].i; 
               end; 

				60:
					begin
					t := t - 1;
					if (abs(s[t].r) > 1.0) and (abs(s[t+1].r) > 1.0) then
						if (realmax/abs(s[t].r)) < abs(s[t+1].r) then
							ps := ovchk;
					if ps <> ovchk then
						s[t].r := s[t].r * s[t+1].r
					end;

				61:
					begin
					t := t - 1;
					if s[t+1].r < minreal then
						ps := divchk
					else
						s[t].r := s[t].r / s[t+1].r
					end;


            62: 
               if eof(input)
               then ps := redchk
               else readln;

            63: 
               begin
                  writeln; chrcnt := 0 
               end; 

            64:
               begin
               h1 := t;
               h2 := 0;
               while s[h1].i <> -1 do
                  begin
                  h1 := h1 - sfsize;
                  h2 := h2 + 1
                  end;  (* h2 is now the number of open guards *)
               if h2=0 then
                  begin
                  if ir.y=0 then
                     ps:=guardchk  (* closed guards and no else/terminate *)
                  else
                     if ir.y=1 then
                        termstate:=true
                  end
               else
                  begin  (* channels/entries to check *)
                  if ir.x=0 then
                     h3 := trunc(random*h2)  (* arbitrary choice *)
                  else
                     h3:=h2-1;  (* priority select *)
                  h4 := t-(sfsize-1)-(h3*sfsize);  (* h4 points to bottom of "frame" *)
                  h1 := 1;
						foundcall := false;
                  while not foundcall and (h1 <=  h2) do
                     begin
							if s[h4].i = 0 then
								begin	(* timeout alternative *)
								if s[h4+3].i < 0 then
									s[h4+3].i := sysclock
								else
									s[h4+3].i := s[h4+3].i + sysclock;
								if (wakeup = 0) or (s[h4+3].i < wakeup) then
									begin
									wakeup := s[h4+3].i;
									wakestart := s[h4+4].i
									end;
                     	h3 := (h3+1) mod h2;
                     	h4 := t - (sfsize-1) - (h3*sfsize);
                     	h1 := h1 + 1
								end
							else
								if s[s[h4].i].i <> 0 then
									foundcall := true
								else
									begin
                     		h3 := (h3+1) mod h2;
                     		h4 := t - (sfsize-1) - (h3*sfsize);
                     		h1 := h1 + 1
									end
                     end;  (* while not foundcall ... *)
                  if not foundcall then  (* no channel/entry has a call *)
                     begin
                     if ir.y <> 2 then  (* ie, if no else part *)
                        begin  (* sleep on all channels *)
                        if ir.y=1 then termstate:=true;
                        h1 := t - (sfsize-1) - ((h2-1)*sfsize);
								chans := h1;
                        for h3 := 1 to h2 do
                           begin
                           h4 := s[h1].i;  (* h4 points to channel/entry *)
									if h4 <> 0 then	(* 0 means timeout *)
										begin
                           	if s[h1+2].i=2 then
                              	s[h4].i := -s[h1+1].i (* query sleep *)
                           	else
                              	if s[h1+2].i=0 then
                                 	s[h4].i := h1+1
                              	else
                                 	if s[h1+2].i = 1 then
                                    	s[h4] := s[h1+1]  (* shriek sleep *)
                                 	else
                                    	s[h4].i := -1;  (* entry sleep *)
                           	s[h4+1]:= s[h1+4];  (* wake address *)
                           	s[h4+2].i:=curpr
										end; (* if h4 <> 0 *)
                           h1:=h1+sfsize
                           end;  (* for loop *)
                        stepcount := 0;
                        suspend:=-h2;
								onselect := true;
								if wakeup <> 0 then
									joineventq(wakeup)
                        end (* sleep on open-guard channels/entries *)
                     end (* no call *)
                  else
                     begin  (* someone is waiting *)
							wakeup := 0;
							wakestart := 0;
                     h1 := s[h4].i;  (* h1 points to channel/entry *)
                     if s[h4+2].i in [0..2] then
                        begin  (* channel rendezvous *)
                        if ((s[h1].i<0) and (s[h4+2].i=2))
                         or ((s[h1].i>0) and (s[h4+2].i<2)) then
                           ps := channerror
                        else
                           begin  (* rendezvous *)
                           s[h1].i := abs(s[h1].i);
                           if s[h4+2].i=0 then
                            s[s[h1].i]:=s[h4+1]
                           else
                            begin  (* block copy *)
                            h3:=0;
                            while h3<s[h4+3].i do
                              begin
                              if s[h4+2].i=1 then
                                 s[s[h1].i+h3]:=s[s[h4+1].i+h3]
                              else
                                 s[s[h4+1].i+h3]:=s[s[h1].i+h3];
                              h3:=h3+1
                              end  (* while *)
                            end;  (* block copy *)
                           pc:=s[h4+4].i;
                           repindex  := s[h4+5].i;  (* recover repindex *)
                           wakenon(h1)  (* wake the other process *)
                           end  (* rendezvous *)
                        end  (* channel rendezvous *)
                     else
                        pc := s[h4+4].i  (* entry *)
                     end  (* someone was waiting *)
                  end;  (* calls to check *)
               t := t-1 - (h2*sfsize)
               end;  (* case 64 *)

            65:      (* channel write - gld *)
               begin
               h1 := s[t-1].i;   (* h1 now points to channel *)
               h2 := s[h1].i;   (* h2 now has value in channel[1] *)
               h3 := s[t].i;   (* base address of source (for ir.x=1) *)
               if h2>0 then
                  ps:=channerror  (* another writer on this channel *)
               else
                  if h2 = 0 then
                     begin  (* first *)
                     if ir.x=0 then s[h1].i:=t else s[h1].i := h3;
                     s[h1+1].i:=pc;
                     s[h1+2].i:=curpr;
                     chans := t-1;
                     suspend := -1;
                     stepcount := 0
                     end  (* first *)
                  else
                     begin  (* second *)
                     h2:=abs(h2);  (* readers leave negated address *)
                     if ir.x=0 then
                        s[h2]:=s[t]
                     else
                        begin
                        h4:=0;  (* loop control for block copy *)
                        while h4 < ir.y do
                           begin
                           s[h2 + h4] := s[h3 + h4];
                           h4 := h4 + 1
                           end  (* while *)
                        end;  (* ir.x was 1 *)
                     wakenon(h1)
                     end;  (* second *)
               t := t - 2
               end;  (* case 65 *)

            66:      (*  channel read - gld *)
               begin 
               h1 := s[t-1].i;
               h2 := s[h1].i;
               h3 := s[t].i;
               if h2<0 then
                  ps:=channerror
               else
                  if h2 = 0 then
                     begin  (* first *)
                     s[h1].i := -h3;
                     s[h1+1].i:=pc;
                     s[h1+2].i:=curpr;
                     chans := t-1;
                     suspend := -1;
                     stepcount := 0
                     end  (* first *)
                  else
                     begin  (* second *)
                     h2:=abs(h2);
                     h4 := 0;
                     while h4 < ir.y do
                        begin
                        s[h3 + h4] := s[h2 + h4];
                        h4 := h4 + 1
                        end;
                     wakenon(h1)
                     end;
               t := t - 2
               end;  (* case 66 *)
         67:
               begin (* delay *)
               h1 := s[t].i;
               t := t - 1;
               joinqueue(h1);
					if curmon <> 0 then
               	releasemon(curmon)
               end;  (* case 67 *)


         68:
               begin  (* resume *)
               h1 := s[t].i;
               t := t - 1;
               if s[h1].i > 0  then
                  begin
                  procwake(h1);
						if curmon <> 0 then
                  	joinqueue(curmon+1)
                  end
               end;  (* case 68 *)

         69:
               begin  (* enter monitor *)
               h1 := s[t].i;  (* address of new monitor variable *)
               s[t].i := curmon;  (* save old monitor variable *)
               curmon := h1;
               if s[curmon].i = 0 then

                  s[curmon].i := -1

               else
                  joinqueue(curmon)
               end;  (* case 69 *)
         70:
               begin  (* exit monitor *)
               releasemon(curmon);
               curmon := s[t].i;
               t := t - 1
               end;  (* case 70 *)
         71:
               begin  (* execute monitor body code *)
               t := t +1;
               s[t].i := pc;
               pc := ir.y
               end;  (* case 70 *)
         72:
               begin  (* return from monitor body code *)
               pc := s[t].i;
               t := t - 1
               end;  (* case 72 *)

			74:	(* check lower bound *)
					if s[t].i < ir.y then
						ps := bndchk;

			75:	(* check upper bound *)
					if s[t].i > ir.y then
						ps := bndchk;

			78:
					;  (* no operation *)

			96:  (* pref *)
				;
					
         97:
               begin  (* sleep *)
               h1 := s[t].i;
               t := t - 1;
               if h1 <= 0 then
                  stepcount := 0
               else
                  joineventq(h1 + sysclock)
               end;  (* case 97 *)

         98:
               begin  (* set process var on process start-up *)
					h1 := s[t].i;
					varptr := h1;
               if s[h1].i = 0 then
                  s[h1].i := curpr
               else
                  ps := instchk;
               t := t - 1
               end;

         99:
               begin  (* ecall *)
                  h1 := t-ir.y;
                  t := h1 - 2;
                  h2 := s[s[h1-1].i].i;  (* h2 has process number *)
                  if h2 > 0  then
							if not ptab[h2].active then
								ps := nexistchk
							else
                     	begin
                     	h3 := ptab[h2].stackbase+s[h1].i;  (* h3 points to entry *)
                     	if s[h3].i <= 0 then
                        	begin  (* empty queue on entry *)
                        	if s[h3].i < 0 then
										begin  (* other process has arrived *)
										for h4 := 1 to ir.y do
											s[h3+h4+(entrysize-1)] := s[h1+h4];
                           	wakenon(h3)
										end;
                        	s[h3+1].i := pc;
                        	s[h3+2].i := curpr
                        	end;
                     	joinqueue(h3);
								s[t+1].i := h3;
                     	chans := t+1;
                     	suspend := - 1
                     	end
                  else
                     if h2 = 0 then
                        ps := nexistchk
                     else
                        ps := namechk
               end;

         100:
               begin    (* acpt1 *)
                  h1 := s[t].i;    (* h1 points to entry *)
                  t := t - 1;
                  if s[h1].i = 0 then
                     begin  (* no calls - sleep *)
                     s[h1].i := - 1;
                     s[h1+1].i := pc;
                     s[h1+2].i := curpr;
                     suspend := - 1;
                     chans := t+1;
                     stepcount := 0
                     end
						else
							begin  (* another process has arrived *)
							h2 := s[h1+2].i;  (* hs has proc number *)
                     h3 := ptab[h2].t + 3;  (* h3 points to first parameter *)
                     for h4 := 0 to ir.y - 1 do

                        s[h1+h4 + entrysize] := s[h3+h4]

							end
               end;

         101:

               begin  (* acpt2 *)
                  h1 := s[t].i; (* h1 points to entry *)
                  t := t - 1;
                  procwake(h1);

                  if s[h1].i <> 0 then
                     begin  (* queue non-empty *)
                     h2 := procqueue.proclist[s[h1].i].proc;  (* h2 has proc id *)
                     s[h1+1].i := ptab[h2].pc;
                     s[h1+2].i := h2
                     end
               end;

         102:	(* rep1c *)
               s[display[ir.x]+ir.y].i := repindex;

			103:	(* rep2c *)
					begin  (* replicate tail code *)
					h1 := s[t].i;
					t := t - 1;
					s[h1].i := s[h1].i + 1;
					pc := ir.y
					end;

			104:  (* powr2 *)

					begin
					h1 := s[t].i;
					if not (h1 in [0..bsmsb]) then
						ps := setchk
					else
						s[t].bs := [h1]
					end;  (* 104 *)

			105:	(* btest *)
					begin
					t := t - 1;
					h1 := s[t].i;
					if not (h1 in [0..bsmsb]) then
						ps := setchk
					else
						s[t].i := btoi(h1 in s[t+1].bs)
					end;  (* 105 *)

			107:  (* write based *)
					begin
					h3 := s[t].i;
					h1 := s[t-1].i;
					h1r := s[t-1].r;
					t := t - 2;
					if h3 = 8 then


						write(h1r:11:8)


					else


						write(h1r:8:16)


					end;  (* 107 *)


			112:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].bs = s[t+1].bs)
					end;  (* 112 *)

			113:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].bs <> s[t+1].bs)
					end;  (* 113 *)

			114:
					begin
					t := t - 1;
					//s[t].i := btoi(s[t].bs < s[t+1].bs)
					s[t].i := btoi((s[t].bs <= s[t+1].bs) and (s[t].bs <> s[t+1].bs))
					end;  (* 114 *)

			115:

					begin
					t := t - 1;
					s[t].i := btoi(s[t].bs <= s[t+1].bs)
					end;  (* 115 *)


			116:
					begin
					t := t - 1;
					//s[t].i := btoi(s[t].bs > s[t+1].bs)
					s[t].i := btoi((s[t].bs >= s[t+1].bs) and (s[t].bs <> s[t+1].bs))
					end;  (* 116 *)

			117:
					begin
					t := t - 1;
					s[t].i := btoi(s[t].bs >= s[t+1].bs)
					end;  (* 117 *)

			118:
					begin
					t := t - 1;
					s[t].bs := s[t].bs + s[t+1].bs
					end;  (* 118 *)

			119:
					begin
					t := t - 1;
					s[t].bs := s[t].bs - s[t+1].bs
					end;  (* 119 *)

			120:
					begin
					t := t - 1;
					s[t].bs := s[t].bs * s[t+1].bs
					end;  (* 120 *)
			121:	(* sinit *)
					if curpr <> 0 then
						ps := seminitchk
					else
						begin
						s[s[t-1].i] := s[t];
						t := t - 2
						end;

			129:
					begin (* prtjmp *)
					if s[curmon+2].i = 0 then
						pc := ir.y
					end;
			130:
					begin (* prtsel *)
               h1 := t;
               h2 := 0;
					foundcall := false;
               while s[h1].i <> -1 do
                  begin
                  h1 := h1 - 1;
                  h2 := h2 + 1
                  end;  (* h2 is now the number of open guards *)
					if h2 <> 0 then
						begin  (* barriers to check *)
                  h3 := trunc(random*h2);  (* arbitrary choice *)
						h4 := 0;  (* count of barriers tested *)
                  while not foundcall and (h4 <  h2) do
                    	begin
							if s[s[h1 + h3 + 1].i].i <> 0 then
								foundcall := true
							else
								begin
                     	h3 := (h3+1) mod h2;
                     	h4 := h4 + 1
								end
							end;
						end;  (* barriers to check *)
					if not foundcall then
						releasemon(curmon)
					else
						begin
						h3 := s[h1 + h3 +1].i;
						procwake(h3)
						end;
					t := h1 - 1;
					s[curmon+2].i := 0;
					pc := s[t].i;
					t := t - 1
					end;
			131:
					begin (* prtslp *)
               h1 := s[t].i;
               t := t - 1;
               joinqueue(h1)
					end;
			132:
					begin (* prtex *)
					if ir.x = 0 then
						clearresource := true
					else
						clearresource := false;
					curmon := s[t].i;
					t := t - 1
					end;
			133:	(* prtcnd *)
					if clearresource then
						begin
						s[curmon+2].i := 1;
						t := t + 1;
						s[t].i := pc;
						t := t + 1;
						s[t].i := -1;
						pc := ir.y
						end

         end  (*case*);

			checkclock;

         if eventqueue.first <> nil then
            if eventqueue.time <= sysclock then
               alarmclock;
         statcounter := statcounter + 1;;
         if statcounter >= statmax then
            ps := statchk
      until ps <> run;

      98: writeln;
      if ps <> fin then

         expmd

      else
         begin
         writeln;
         writeln('Program terminated normally')
         end;
      97: writeln
	end;  (* runprog *)


begin  (* Main *)
    (* 
       Randomize is used to implement the
       native function: random, provided
       by GNU Pascal to implement randomness
     *)

    (* dgm *)
    if paramcount=2 then begin
                assign(objfile,paramstr(1));
                assign(pmdfile,paramstr(2))
                end;

    randomize;
	putversion(output);


	rewrite(pmdfile);

	getcode;

	repeat

		runprog;
		writeln;
		writeln('Type r and RETURN to rerun'); 
		if eof then
			begin
			ch := 'x';
			writeln('End of data file - program terminating');
			writeln
			end
		else
			begin

			if eoln then readln;
			readln(ch);
			writeln; 
			end

	until not (ch in ['r','R'])

end.



