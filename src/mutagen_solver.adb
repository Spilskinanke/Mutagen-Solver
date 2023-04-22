with Mutagen;
with Base; use Base;
with Ada.Text_IO;
with Base.SolutionSearch;
with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line;

procedure Mutagen_Solver is

   function IsNumeric (Item : String) return Boolean is
      Dummy : Float;
   begin
      Dummy := Float'Value (Item);
      return True;
   exception
      when others =>
         return False;
   end IsNumeric;

   package IO renames Ada.Text_IO;
   package CLI renames Ada.Command_Line;
   package SS is new Base.SolutionSearch
      (State         => Mutagen.PuzzleState,
       ManyStates    => Mutagen.PuzzleStates,
       getAdjacents  => Mutagen.getAdjacents,
       calcHeuristic => Mutagen.calcHeuristic);

   use Mutagen;

   procedure solvePuzzle (input : String; threads : Positive) is

      initial_state : constant PuzzleState := initialize (input);
      sol           : PuzzleState;
      startT, endT  : Time;
      millis        : Duration;

   begin
      IO.Put_Line ("Starting puzzle " & input);
      startT := Clock;
      sol := SS.search (initial_state, threads);
      endT := Clock;
      millis := (endT - startT) * 1000;
      IO.Put_Line ("Time: " & millis'Image);
      IO.Put_Line ("Solution: " & sol.reagents_used'Image);
      IO.New_Line;
   end solvePuzzle;

   threads : Positive := 1;
   skip_first : Natural := 0;
begin
   if IsNumeric (CLI.Argument (1)) then
      threads := Integer'Value (CLI.Argument (1));
      skip_first := skip_first + 1;
   end if;

   for i in 1 + skip_first .. CLI.Argument_Count loop
      solvePuzzle (CLI.Argument (i), threads);
   end loop;

end Mutagen_Solver;
