with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Synchronous_Barriers;

package body Base.SolutionSearch is

   function search (initial : State; threads : Positive := 1) return State is

      package IO renames Ada.Text_IO;

      released : Boolean;

      state_space  : NodeHeap;
      barrier : Ada.Synchronous_Barriers.Synchronous_Barrier (threads + 1);

      protected type ThreadInfo is
         procedure goalFound (goal : State);
         function isGoalFound return Boolean;
         function getGoalstate return State;
         
      private
         is_goal_found : Boolean := False;
         goal_state    : State;
         queue_locked  : Boolean := False;
      end ThreadInfo;

      protected body ThreadInfo is

         procedure goalFound (goal : State)  is
         begin
            is_goal_found := True;
            goal_state := goal;
         end goalFound;

         function isGoalFound return Boolean is
         begin
            return is_goal_found;
         end isGoalFound;

         function getGoalstate return State is
         begin
            return goal_state;
         end getGoalstate;
         
      end ThreadInfo;

      thread_info : ThreadInfo;

      task type SearchThread(id : Natural);
      task body SearchThread is
         released : Boolean;
      begin
         --IO.Put_Line ("STARTING NEW THREAD " & id'Image);
         while not thread_info.isGoalFound loop

            declare
               s : State;
            begin
               --IO.Put_Line (id'Image & ": " & "GETTING NEW STATE");
               state_space.Dequeue (s);
               --IO.Put_Line (id'Image & ": " & "GOT STATE " & s'Image);
               for new_state of getAdjacents (s) loop
                  --IO.Put_Line (id'Image & ": " & "IN LOOP");
                  declare
                     val : constant Heuristic := calcHeuristic (new_state);
                  begin
                     --IO.Put_Line (id'Image & ": " & "GOT ADJACENT " & new_state'Image & " WITH " & val'Image);
                     if val = GOAL then
                        thread_info.goalFound (new_state);
                        exit;
                     elsif val /= NEVER then
                        state_space.Enqueue (new_state);
                     end if;
                     
                  end;
               end loop;
               --IO.Put_Line (id'Image & ": " & "OUT OF LOOP");
            end;
         end loop;
         --IO.Put_Line (id'Image & ": " & "FINISHED");
         Ada.Synchronous_Barriers.Wait_For_Release (barrier, released);
      end SearchThread;

   begin
      state_space.Enqueue (initial);

      declare
         search_threads : array (Positive range 1 .. threads) of SearchThread(1);
      begin
         Ada.Synchronous_Barriers.Wait_For_Release (barrier, released);
         return thread_info.getGoalstate;
      end;

   end search;

end Base.SolutionSearch;
