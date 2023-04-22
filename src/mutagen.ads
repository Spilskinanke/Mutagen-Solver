with Ada.Strings.Text_Buffers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Base; use Base;

package Mutagen is

   subtype CompoundBase is String (1 .. 2);

   type Compound is record
      base : CompoundBase;
      sign : Boolean;
   end record;

   type CompoundList is array (1 .. 20) of Compound;

   type Reagent is record
      compounds : CompoundList;
      name      : Unbounded_String;
      length    : Natural := 0; -- 0 indicates null reagent
   end record with Put_Image => DisplayReagent;
   procedure DisplayReagent
      (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
       Value  : Reagent);

   package ReagentVectorClass is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Reagent,
       "="          => "=");

   subtype ReagentList is ReagentVectorClass.Vector;

   package SequenceClass is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => CompoundBase,
       "="          => "=");
   subtype Sequence is SequenceClass.Vector;

   type PuzzleState is record

      reagents : ReagentList := ReagentVectorClass.Empty_Vector;
      reagents_used : Unbounded_String := To_Unbounded_String ("");
      previous_sequence : Sequence := SequenceClass.Empty_Vector;
      current_sequence : Sequence := SequenceClass.Empty_Vector;
      exitus : Reagent;
   end record with Put_Image => DisplayState;
   procedure DisplayState
      (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
       Value  : PuzzleState);


   type PuzzleStates is array (Positive range <>) of PuzzleState;

   function initialize (filename : String) return PuzzleState;
   function act (state : PuzzleState; action : Positive) return PuzzleState;
   function getAdjacents (state : PuzzleState) return PuzzleStates;
   function calcHeuristic (state : PuzzleState) return Heuristic;

end Mutagen;
