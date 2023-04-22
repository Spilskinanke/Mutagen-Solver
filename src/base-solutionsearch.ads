with Ada.Containers.Unbounded_Priority_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;

generic
   type State is private;
   type ManyStates is array (Positive range <>) of State;
   with function getAdjacents (s : State) return ManyStates;
   with function calcHeuristic (s : State) return Heuristic;
package Base.SolutionSearch is
   
  

   package StateInterface is new Ada.Containers.Synchronized_Queue_Interfaces
      (Element_Type => State);

   package NodeHeapClass is new Ada.Containers.Unbounded_Priority_Queues 
      (Queue_Interfaces => StateInterface,
       Queue_Priority   => Heuristic,
       Get_Priority     => calcHeuristic,
       Before           => heuristicPriority);
   subtype NodeHeap is NodeHeapClass.Queue;

   function search (initial : State; threads : Positive := 1) return State;

end Base.SolutionSearch;
