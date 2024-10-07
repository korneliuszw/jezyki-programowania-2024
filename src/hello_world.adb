-- A skeleton of an ADA program for an assignment in programming languages

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Hello_World is

   ----GLOBAL VARIABLES---

   Number_Of_Producers  : constant Integer := 5;
   Number_Of_Assemblies : constant Integer := 3;
   Number_Of_Consumers  : constant Integer := 2;
   Delay_Factor: constant Duration := 0.2;

   subtype Producer_Type is Integer range 1 .. Number_Of_Producers;
   subtype Assembly_Type is Integer range 1 .. Number_Of_Assemblies;
   subtype Consumer_Type is Integer range 1 .. Number_Of_Consumers;

   --each Producer is assigned a Product that it produces
   Product_Name  : constant array (Producer_Type) of String (1 .. 8) :=
     ("Product1", "Product2", "Product3", "Product4", "Product5");
   --Assembly is a collection of products
   Assembly_Name : constant array (Assembly_Type) of String (1 .. 9) :=
     ("Big Mac  ", "Ze. Bambi", "McFlurry ");

   ----TASK DECLARATIONS----

   -- Producer produces determined product
   task type Producer is
      entry Start (Product : in Producer_Type; Production_Time : in Integer);
   end Producer;

   -- Consumer gets an arbitrary assembly of several products from the buffer
   -- but he/she orders it randomly
   task type Consumer is
      entry Start
        (Consumer_Number : in Consumer_Type; Consumption_Time : in Integer);
   end Consumer;

   task Cleaning is
        entry Start(Cleaning_Day_When: in Integer);
    end Cleaning;

   -- Buffer receives products from Producers and delivers Assemblies to Consumers
   task type Buffer is
      -- Accept a product to the storage (provided there is a room for it)
      entry Take (Product : in Producer_Type; Number : in Integer);
      -- Deliver an assembly (provided there are enough products for it)
      entry Deliver (Assembly : in Assembly_Type; Number : out Integer);
      entry Cleaning_Day;
      entry Print_Stats;
   end Buffer;

   P : array (1 .. Number_Of_Producers) of Producer;
   K : array (1 .. Number_Of_Consumers) of Consumer;
   B : Buffer;

   ----TASK DEFINITIONS----

   --Producer--

   task body Producer is
      subtype Production_Time_Range is Integer range 1 .. 3;
      package Random_Production is new Ada.Numerics.Discrete_Random
        (Production_Time_Range);
      --  random number generator
      G                    : Random_Production.Generator;
      Producer_Type_Number : Integer;
      Product_Number       : Integer;
      Production           : Integer;
      Random_Time          : Duration;
   begin
      accept Start (Product : in Producer_Type; Production_Time : in Integer)
      do
         --  start random number generator
         Random_Production.Reset (G);
         Product_Number       := 1;
         Producer_Type_Number := Product;
         Production           := Production_Time;
      end Start;
      Put_Line
        (ESC & "[93m" & "P: Started producer of " &
         Product_Name (Producer_Type_Number) & ESC & "[0m");
      loop
         Random_Time := Duration (Random_Production.Random (G) * Delay_Factor);
         delay Random_Time;
         Put_Line
           (ESC & "[93m" & "P: Produced product " &
            Product_Name (Producer_Type_Number) & " number " &
            Integer'Image (Product_Number) & ESC & "[0m");
         -- Accept for storage
            --     B.Take(Producer_Type_Number, Product_Number);
            --     Product_Number := Product_Number + 1;
        loop
            select
               B.Take(Producer_Type_Number, Product_Number);
               Product_Number := Product_Number + 1;
               exit;
            else
               Put_Line("Other product is being loaded to buffer. PLZ WAIT");
               delay 0.5 * Delay_Factor;
            end select;
         end loop;
      end loop;
   end Producer;

   --Consumer--

   task body Consumer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new Ada.Numerics.Discrete_Random
        (Consumption_Time_Range);

      --each Consumer takes any (random) Assembly from the Buffer
      package Random_Assembly is new Ada.Numerics.Discrete_Random
        (Assembly_Type);

      G               : Random_Consumption.Generator;
      GA              : Random_Assembly.Generator;
      Consumer_Nb     : Consumer_Type;
      Assembly_Number : Integer;
      Consumption     : Integer;
      Assembly_Type   : Integer;
      Consumer_Name   :
        constant array (1 .. Number_Of_Consumers) of String (1 .. 9) :=
        ("Consumer1", "Consumer2");
   begin
      accept Start
        (Consumer_Number : in Consumer_Type; Consumption_Time : in Integer)
      do
         Random_Consumption.Reset (G);
         Random_Assembly.Reset (GA);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line
        (ESC & "[96m" & "C: Started consumer " & Consumer_Name (Consumer_Nb) &
         ESC & "[0m");
      loop
         delay Duration
           (Random_Consumption.Random (G) * Delay_Factor); --  simulate consumption
         Assembly_Type := Random_Assembly.Random (GA);
         -- take an assembly for consumption
         B.Deliver (Assembly_Type, Assembly_Number);
         if Assembly_Number = 0 then
             Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) & " lacks assembly of type " & Assembly_Name(Assembly_Type) & ESC & "[0m");
         else
             Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) &
                      " takes assembly " & Assembly_Name(Assembly_Type) & " number " &
                      Integer'Image(Assembly_Number) & ESC & "[0m");
         end if;
      end loop;
   end Consumer;

   -- Cleaning
    task body Cleaning is
        Cleaning_Day: Integer;
        Day_Number: Integer := 1;
        Day_Duration: constant Duration := 2.0 * Delay_Factor;
    begin
        accept Start(Cleaning_Day_When: in Integer) do
            Cleaning_Day := Cleaning_Day_When;
        end Start;
        loop
            delay Day_Duration;
            Put_Line (ESC & "[92m" & "Day " & Integer'Image(Day_Number) & ESC & "[0m");
            if Day_Number = Cleaning_Day then
                Put_Line (ESC & "[92m" & "Cleaning day" & ESC & "[0m");
                Day_Number := 1;
                B.Cleaning_Day;
            else
                Day_Number := Day_Number + 1;
            end if;
        end loop;
    end Cleaning;


   --Buffer--

   task body Buffer is
      Storage_Capacity : constant Integer := 30;
      type Stat is (Hit, Miss);
      type Storage_type is array (Producer_Type) of Integer;
      Storage              : Storage_type := (0, 0, 0, 0, 0);
      Assembly_Content     : array (Assembly_Type, Producer_Type) of Integer :=
        --  ((2, 1, 2, 1, 2), (1, 2, 0, 1, 1), (0, 2, 2, 1, 1));
        ((2, 1, 2, 0, 2), (1, 2, 0, 1, 0), (3, 2, 2, 0, 1));
      Max_Assembly_Content : array (Producer_Type) of Integer;
      Assembly_Number      : array (Assembly_Type) of Integer := (1, 1, 1);
      In_Storage           : Integer := 0;
      type Stat_Counter is delta 0.01 digits 20;
      Product_Stats : array(Stat) of Stat_Counter := (0.0, 0.0);
      Assembly_Stats: array(Stat) of Stat_Counter := (0.0, 0.0);
      Stat_File: Ada.Text_IO.File_Type;
      
      Priority: array(Producer_Type) of Integer := (3, 3, 3, 3, 3);
      Highest_Priority: Integer;
      Lowest_Priority_Producer: Producer_Type;

      procedure RecalculatePriority is
        Lowest_Priority: Integer := 999999;
      begin
        for P in Producer_Type loop
            Priority(P) := Max_Assembly_Content(P) - Storage(P);
            if Highest_Priority < Priority(P) then
                Highest_Priority := Priority(P);
            end if;
            if Priority(P) < Lowest_Priority then
                Lowest_Priority := Priority(P);
                Lowest_Priority_Producer := P;
            end if;
        end loop;
      end RecalculatePriority;

      procedure Setup_Variables is
      begin
         for W in Producer_Type loop
            Max_Assembly_Content (W) := 0;
            for Z in Assembly_Type loop
               if Assembly_Content (Z, W) > Max_Assembly_Content (W) then
                  Max_Assembly_Content (W) := Assembly_Content (Z, W);
               end if;
            end loop;
         end loop;
      end Setup_Variables;

      procedure Today_Is_Cleaning_Day is
        Cleaning_Day_Takes: constant Integer := 3;
      begin
        for P in Producer_Type loop
            if Storage(P) >= Cleaning_Day_Takes then
                Put_Line ( ESC & "[92m" & "Cleaning storage for product " & Integer'Image(P) & ESC & "[0m");
                Storage(P) := Storage(P) - Cleaning_Day_Takes;
                In_Storage := In_Storage - Cleaning_Day_Takes;
            end if;
        end loop;
      end;
      function Can_Handle(P: Producer_Type) return Boolean is
        -- Ignore priorites before a certain treshold
        Storage_Safe_Treshold: constant Integer := Storage_Capacity - 7;
      begin
        if In_Storage < Storage_Safe_Treshold then
            return True;
        elsif Priority(P) > 0 then
            return True;
        elsif Highest_Priority > 0 or P = Lowest_Priority_Producer then
            return False;
        end if;
        return True;
      end;

      function Can_Accept (Product : Producer_Type) return Boolean is
      begin
        if not Can_Handle(Product) then
        --  if false then
            return False;
        elsif In_Storage >= Storage_Capacity then
            return False;
        else
            return True;
        end if;
      end Can_Accept;


      function Can_Deliver (Assembly : Assembly_Type) return Boolean is
      begin
         for W in Producer_Type loop
            if Storage (W) < Assembly_Content (Assembly, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Storage_Contents is
      begin
         for W in Producer_Type loop
            Put_Line
              ("|   Storage contents: " & Integer'Image (Storage (W)) & " " &
               Product_Name (W));
         end loop;
         Put_Line
           ("|   Number of products in storage: " &
            Integer'Image (In_Storage));

      end Storage_Contents;

      function Find_Max_In_Storage return Producer_Type is
      Max_Value: Integer := Storage(1);
      Max_Producer: Producer_Type := 1;
      begin
         for W in Producer_Type loop
            if Storage(W) > Max_Value then
               Max_Value := Storage(W);
               Max_Producer := W;
            end if;
         end loop;
         return Max_Producer;
      end Find_Max_In_Storage;
      
      procedure RemoveItem(Producer: Producer_Type) is
      begin
        if Storage(Producer) = 0 then
            return;
        end if;
        Storage(Producer) := Storage(Producer) - 1;
        In_Storage := In_Storage - 1;
      end;

      procedure CleanupRedundantStorage(Producer: Producer_Type) is
      begin
        if In_Storage < Storage_Capacity then
            return;
        end if;
        RemoveItem(Find_Max_In_Storage);
        RemoveItem(Lowest_Priority_Producer);
        RecalculatePriority;
      end;

   begin
      Create (Stat_File, Ada.Text_IO.Out_File, "stats.txt");
      Close(Stat_File);
      Put_Line (ESC & "[91m" & "B: Buffer started" & ESC & "[0m");
      Setup_Variables;
      loop
      select
         accept Take (Product : in Producer_Type; Number : in Integer) do
            CleanupRedundantStorage(Product);
            if Can_Accept (Product) then
               Put_Line
                 (ESC & "[91m" & "B: Accepted product " &
                  Product_Name (Product) & " number " &
                  Integer'Image (Number) & ESC & "[0m");
               Storage (Product) := Storage (Product) + 1;
               In_Storage        := In_Storage + 1;
               Product_Stats(Hit) := Product_Stats(Hit) + 1.0;
               RecalculatePriority;
            else
               Put_Line
                 (ESC & "[91m" & "B: Rejected product " &
                  Product_Name (Product) & " number " &
                  Integer'Image (Number) & ESC & "[0m");
                Product_Stats(Miss) := Product_Stats(Miss) + 1.0;
            end if;
         end Take;
         Storage_Contents;
        or
         accept Deliver (Assembly : in Assembly_Type; Number : out Integer) do
            if Can_Deliver (Assembly) then
               Put_Line
                 (ESC & "[91m" & "B: Delivered assembly " &
                  Assembly_Name (Assembly) & " number " &
                  Integer'Image (Assembly_Number (Assembly)) & ESC & "[0m");
               for W in Producer_Type loop
                  Storage (W) := Storage (W) - Assembly_Content (Assembly, W);
                  In_Storage  := In_Storage - Assembly_Content (Assembly, W);
               end loop;
               Number                     := Assembly_Number (Assembly);
               Assembly_Number (Assembly) := Assembly_Number (Assembly) + 1;
                Assembly_Stats(Hit) := Assembly_Stats(Hit) + 1.0;
            else
               Put_Line
                 (ESC & "[91m" & "B: Lacking products for assembly " &
                  Assembly_Name (Assembly) & ESC & "[0m");
               Number := 0;
                Assembly_Stats(Miss) := Assembly_Stats(Miss) + 1.0;
            end if;
         end Deliver;
         Storage_Contents;
        or
            accept Cleaning_Day do
                Today_Is_Cleaning_Day;
            end Cleaning_Day;
            Storage_Contents;
        or
            accept Print_Stats do
                Open(Stat_File, Ada.Text_IO.Append_File, "stats.txt");
                Put_Line (Stat_File, "Product stats:");
                Put_Line (Stat_File, "Hits: " & Stat_Counter'Image(Product_Stats(Hit)));
                Put_Line (Stat_File, "Misses: " & Stat_Counter'Image(Product_Stats(Miss)));
                Put_Line (Stat_File, "Ratio: " & Stat_Counter'Image(Product_Stats(Hit) / (Product_Stats(Hit) + Product_Stats(Miss))));
                Put_Line (Stat_File, "Assembly stats:");
                Put_Line (Stat_File, "Hits: " & Stat_Counter'Image(Assembly_Stats(Hit)));
                Put_Line (Stat_File, "Misses: " & Stat_Counter'Image(Assembly_Stats(Miss)));
                Put_Line (Stat_File, "Ratio: " & Stat_Counter'Image(Assembly_Stats(Hit) / (Assembly_Stats(Hit) + Assembly_Stats(Miss))));
                Close(Stat_File);
            end Print_Stats;
        end select;
      end loop;
   end Buffer;

   ---"MAIN" FOR SIMULATION---
begin
   for I in 1 .. Number_Of_Producers loop
      P (I).Start (I, 10);
   end loop;
   for J in 1 .. Number_Of_Consumers loop
      K (J).Start (J, 12);
   end loop;
    Cleaning.Start(10);
    loop
        delay 10.0;
        B.Print_Stats;
    end loop;
end Hello_World;

