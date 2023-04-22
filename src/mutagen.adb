with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings; use Ada.Strings;

package body Mutagen is

   package Fixed renames Ada.Strings.Fixed;

   function initialize (filename : String) return PuzzleState is

      package IO renames Ada.Text_IO;
      file : IO.File_Type;

      function getReagent (line : String) return Reagent is

         reag : Reagent;
         trimmed_line : constant String := Fixed.Trim (line, Both);
         name_idx : Positive := trimmed_line'First;
         base_beg_idx : Positive := 1;
         found_name : Boolean := False;

         function parseCompound (str : String) return Compound is
            comp   : Compound;
            is_neg : constant Boolean := str (str'First) = '-';
         begin
            if is_neg then
               comp.base := str (str'First + 1 .. str'First + 2);
            else
               comp.base := str (str'First .. str'First + 1);
            end if;

            comp.sign := not is_neg;
            return comp;
         end parseCompound;

      begin
         for i in trimmed_line'Range loop
            if not found_name then
               if trimmed_line (i) = ' ' then
                  found_name := True;
               else
                  name_idx := i;
               end if;
            elsif trimmed_line (i) /= ' ' and then base_beg_idx = 1 then
               base_beg_idx := i;
            elsif (trimmed_line (i) = ' ' or else trimmed_line'Last = i) and then
               base_beg_idx /= 1 then
               reag.length := reag.length + 1;
               reag.compounds  (reag.length) := parseCompound (trimmed_line (base_beg_idx .. i));
               base_beg_idx := 1;
            end if;
         end loop;

         reag.name := To_Unbounded_String
            (trimmed_line (trimmed_line'First .. name_idx));

         return reag;

      end getReagent;

      function buildPuzzleInfo (exitus_1 : out Reagent)
                                return ReagentList is

         reagents : ReagentList := ReagentVectorClass.Empty_Vector;

      begin

         IO.Open (file, IO.In_File, filename);
         while not IO.End_Of_File (File => file) loop
            declare
               line : constant String := IO.Get_Line (file);
            begin
               if Fixed.Index (line, "xitus-1") > 0 then
                  exitus_1 := getReagent (line);
               elsif line (line'First) /= '#' then
                  reagents.Append (getReagent (line));
               end if;
            end;
         end loop;
         IO.Close (file);

         return reagents;
      end buildPuzzleInfo;

      exitus_1 : Reagent;
      reagents : constant ReagentList := buildPuzzleInfo (exitus_1);

      puzzle_start : PuzzleState;

   begin
      puzzle_start.reagents := reagents;
      puzzle_start.exitus := exitus_1;

      return puzzle_start;
   end initialize;

   function act (state : PuzzleState; action : Positive)
                 return PuzzleState is

      MissingAction : exception;

      reagent_selected : constant Reagent := state.reagents (action);
      new_state : PuzzleState := state;

      procedure applyCompound (cur_sol : in out Sequence; comp : Compound) is
         use SequenceClass;
         index : constant Extended_Index := cur_sol.Find_Index (comp.base);
      begin

         if not comp.sign and then index /= No_Index then
            cur_sol.Delete (index);
         elsif index = No_Index and then comp.sign then
            cur_sol.Append (comp.base);
         end if;

      end applyCompound;

   begin
      if reagent_selected.length = 0 then
         raise MissingAction with "death";
      end if;

      Append (new_state.reagents_used, reagent_selected.name & ", ");
      for comp of reagent_selected.compounds (reagent_selected.compounds'First ..
                                                 reagent_selected.length) loop
         applyCompound (new_state.current_sequence, comp);

      end loop;
      new_state.previous_sequence := state.current_sequence;
      return new_state;
   end act;

   function getAdjacents (state : PuzzleState)
                    return PuzzleStates is

      states : PuzzleStates (Positive'First .. state.reagents.Last_Index);
   begin
      for i in states'Range loop
         states (i) := act(state, i);
      end loop;
      return states;
   end getAdjacents;

   function calcHeuristic (state : PuzzleState) return Heuristic is
      score        : Integer := 0;
      is_goal      : Boolean := True;
      index_c      : Natural := 0;
      use Mutagen.SequenceClass;
   begin
      for I in state.current_sequence.First_Index .. state.current_sequence.Last_Index loop
         if state.exitus.length >= I and then (state.exitus.compounds (I - index_c).base =
            state.current_sequence (I)) then
            score := score + 3;
         else
            score := score - 1;
            index_c := index_c + 1;
            is_goal := False;
         end if;
      end loop;

      if is_goal and then state.exitus.length = Natural(state.current_sequence.Length) then
         return GOAL;
      elsif state.current_sequence = state.previous_sequence then
         return NEVER;
      else
         return Heuristic (score);
      end if;
   end calcHeuristic;

   procedure DisplayReagent
      (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
       Value  : Reagent) is

      CR : constant Character := Character'Val (13);
      LF : constant Character := Character'Val (10);
      NL : constant String := [CR, LF];

      display : constant String := NL & "(" &
                  Value.compounds (Value.compounds'First .. Value.length)'Image &
               "," & NL &
                  Value.name'Image &
                  "," & NL &
                  Value.length'Image & ")";
   begin

      Output.Put (display);
   end DisplayReagent;

   procedure DisplayState
      (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
       Value  : PuzzleState) is
   begin
      Output.Put (Value.reagents_used'Image);
   end DisplayState;

end Mutagen;
