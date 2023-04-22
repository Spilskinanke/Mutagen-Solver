package Base is

   subtype Heuristic is Integer;

   NEVER : constant Heuristic := Heuristic'First;
   GOAL  : constant Heuristic := Heuristic'Last;

   function heuristicPriority (L, R : Heuristic) return Boolean;

end Base;
