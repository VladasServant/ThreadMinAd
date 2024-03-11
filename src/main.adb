with Ada.Text_IO;
use Ada.Text_IO;

procedure Main is

   Dim : constant Integer := 50000;
   Thread_Num : constant Integer := 10;

   type Arr_Type is array (Positive range <>) of Integer;
   Arr : Arr_Type(1..Dim);

   procedure Init_Arr is
   begin
      for I in Arr'Range loop
         Arr(I) := I;
      end loop;
      Arr(6) := -27;
   end Init_Arr;

   function Part_Min(Start_Index, Finish_Index : in Integer) return Integer is
      Min_Element : Integer := Arr(Start_Index);
   begin
      for I in Start_Index + 1 .. Finish_Index loop
         if Arr(I) < Min_Element then
            Min_Element := Arr(I);
         end if;
      end loop;
      return Min_Element;
   end Part_Min;

   task type Starter_Thread is
      entry Start(Start_Index, Finish_Index : in Integer);
   end Starter_Thread;

   protected Part_Manager is
      procedure Set_Part_Min(Min_Element : in Integer);
      entry Get_Min(Min_Element : out Integer);
   private
      Tasks_Count : Integer := 0;
      Min1 : Integer := Integer'Last;
   end Part_Manager;

   protected body Part_Manager is
      procedure Set_Part_Min(Min_Element : in Integer) is
      begin
         Min1 := Integer'Min(Min1, Min_Element);
         Tasks_Count := Tasks_Count + 1;
      end Set_Part_Min;

      entry Get_Min(Min_Element : out Integer) when Tasks_Count = Thread_Num is
      begin
         Min_Element := Min1;
      end Get_Min;

   end Part_Manager;

   task body Starter_Thread is
      Min_Element : Integer := Integer'Last;
      Start_Index, Finish_Index : Integer;
   begin
      accept Start(Start_Index, Finish_Index : in Integer) do
         Starter_Thread.Start_Index := Start_Index;
         Starter_Thread.Finish_Index := Finish_Index;
      end Start;
      Min_Element := Part_Min(Start_Index  => Start_Index,
                              Finish_Index => Finish_Index);
      Part_Manager.Set_Part_Min(Min_Element);
   end Starter_Thread;

   function Parallel_Min return Integer is
      Min_Element : Integer := Integer'Last;
      Thread : array(1..Thread_Num) of Starter_Thread;
   begin
      for i in 1..Thread_Num loop
         Thread(i).Start((i - 1) * Dim / Thread_Num + 1, i * Dim / Thread_Num);
         end loop;
      Part_Manager.Get_Min(Min_Element);
      return Min_Element;
   end Parallel_Min;

   Min_Element : Integer;
   Min_Index : Integer;

   begin
      Init_Arr;
      Min_Element := Integer'Last;
      Min_Index := 0;

   for I in Arr'Range loop
      if Arr(I) < Min_Element then
         Min_Element := Arr(I);
         Min_Index := I;
      end if;
   end loop;

   Put_Line("Min element in the array: " & Min_Element'Img);
   Put_Line("Index of min element:" & Min_Index'Img);

end Main;
