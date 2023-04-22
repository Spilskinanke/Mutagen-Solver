package body Base is

   function heuristicPriority (L, R : Heuristic) return Boolean is begin
      return L > R;
   end heuristicPriority;

end Base;
