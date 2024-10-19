-- A skeleton of an ADA program for an assignment in programming languages

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;

procedure Simulator is

   ----GLOBAL VARIABLES---

   Cook_Count  : constant Integer := 5;
   Meal_Count : constant Integer := 3;
   Customer_Count  : constant Integer := 2;

   -- Reduce the factor for faster simulation, increase for slower
   Delay_Factor: constant Duration := 1.0;

   subtype Cooks is Integer range 1 .. Cook_Count;
   subtype Meals is Integer range 1 .. Meal_Count;
   subtype Customers is Integer range 1 .. Customer_Count;

   -- each Cook is assigned a Dish that it produces
   Dish_Names  : constant array (Cooks) of Unbounded_String :=
     (To_Unbounded_String("Burger"), To_Unbounded_String("Frytki"), To_Unbounded_String("Cola"), To_Unbounded_String("Gwiazdki serowe"), To_Unbounded_String("6x Nuggetsy"));

   -- Dishes are part of the Meals
   Meal_Names : constant array (Meals) of Unbounded_String :=
     (To_Unbounded_String("McZestaw Big Mac"), To_Unbounded_String("Zestaw Bambi"), To_Unbounded_String("Powiekszony McZestaw Nuggets + Hamburger"));


   ----TASK DECLARATIONS----

   -- Prepares a dish
   task type Cook is
      entry Start (Dish : in Cooks; Cooking_Time : in Integer);
   end Cook;

   -- Consumes a meal
   task type Customer is
      entry Start
        (Consumer_Number : in Customers; Consumption_Time : in Integer);
   end Customer;

   -- Cleans the storage every N days
   task Cleaning is
        entry Start(Cleaning_Day_When: in Integer);
    end Cleaning;

   -- Kitchen receives dishes from cooks and delivers meals to Customers
   task type Kitchen is
      -- Accept a dish to the storage (provided there is a room for it)
      entry Take (Dish : in Cooks; Number : in Integer);
      -- Deliver a meal (provided there are enough products for it)
      entry Deliver (Meal : in Meals; Number : out Integer);
      -- Remove some dishes from storage
      entry Cleaning_Day;
      -- Print hit/miss stats of Take and Deliver to a file
      entry Print_Stats;
      -- Check if we can safely accept more products
      entry Check_Kitchen_Threshold(Dish: in Cooks; Can_Accept: out Boolean);
   end Kitchen;

   P : array (1 .. Cook_Count) of Cook;
   K : array (1 .. Customer_Count) of Customer;
   B : Kitchen;

   ----TASK DEFINITIONS----

   --Cook--

   task body Cook is
      subtype Cooking_Time_Range is Integer range 1 .. 3;
      package Random_Cooking is new Ada.Numerics.Discrete_Random
        (Cooking_Time_Range);
      G                    : Random_Cooking.Generator;
      Cook_Type_Number : Integer;
      Dish_Number       : Integer;
      Cooking           : Integer;
      Random_Time          : Duration;
   begin
      accept Start (Dish : in Cooks; Cooking_Time : in Integer)
      do
         Random_Cooking.Reset (G);
         Dish_Number       := 1;
         Cook_Type_Number := Dish;
         Cooking           := Cooking_Time;
      end Start;
      Put_Line
        (ESC & "[93m" & "Cook: Started preparing dish " &
         To_String(Dish_Names (Cook_Type_Number)) & ESC & "[0m");
      loop
         declare
            Can_Safely_Accept: Boolean;
         begin
            B.Check_Kitchen_Threshold(Cook_Type_Number, Can_Safely_Accept);
            Random_Time := Duration(Random_Cooking.Random(G));
            if not Can_Safely_Accept then
               --  delay production
               Random_Time := Random_Time * 2;
               Put_Line(ESC & "[93m" & "C: Demand for " & To_String(Dish_Names(Cook_Type_Number)) & " is low delaying cooking" & ESC & "[0m");
            end if;
         end;
         --  end of local scope
         delay Random_Time * Delay_Factor;
         Put_Line
           (ESC & "[93m" & "P: Cooked dish " &
            To_String(Dish_Names (Cook_Type_Number)) & " number " &
            Integer'Image (Dish_Number) & ESC & "[0m");
         -- Accept for storage
         loop
            select
               B.Take(Cook_Type_Number, Dish_Number);
               Dish_Number := Dish_Number + 1;
               exit;
            else
               Put_Line("Other dish is being loaded onto kitchen. PLZ WAIT");
               delay 0.5 * Delay_Factor;
            end select;
         end loop;
      end loop;
   end Cook;

   --Consumer--

   task body Customer is
      subtype Consumption_Time_Range is Integer range 4 .. 8;
      package Random_Consumption is new Ada.Numerics.Discrete_Random
        (Consumption_Time_Range);

      --each Consumer takes any (random) Meal from the Kitchen
      package Random_Meal is new Ada.Numerics.Discrete_Random
        (Meals);

      G               : Random_Consumption.Generator;
      GA              : Random_Meal.Generator;
      Consumer_Nb     : Customers;
      Meal_Number : Integer;
      Consumption     : Integer;
      Meal_Type   : Integer;
      Consumer_Name   :
        constant array (1 .. Customer_Count) of String (1 .. 9) :=
        ("Consumer1", "Consumer2");
   begin
      accept Start
        (Consumer_Number : in Customers; Consumption_Time : in Integer)
      do
         Random_Consumption.Reset (G);
         Random_Meal.Reset (GA);
         Consumer_Nb := Consumer_Number;
         Consumption := Consumption_Time;
      end Start;
      Put_Line
        (ESC & "[96m" & "Customer: Customer " & Consumer_Name (Consumer_Nb) & " starts ordering" &
         ESC & "[0m");
      loop
         delay Duration
           (Random_Consumption.Random (G) * Delay_Factor); --  simulate consumption
         Meal_Type := Random_Meal.Random (GA);
         -- take an assembly for consumption
         B.Deliver (Meal_Type, Meal_Number);
         -- if the assembly is not available, print a message
         if Meal_Number = 0 then
             Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) & " lacks dish " & To_String(Meal_Names(Meal_Type)) & ESC & "[0m");
         else
             Put_Line(ESC & "[96m" & "C: " & Consumer_Name(Consumer_Nb) &
                      " takes dish " & To_String(Meal_Names(Meal_Type)) & " number " &
                      Integer'Image(Meal_Number) & ESC & "[0m");
         end if;
      end loop;
   end Customer;

   -- Cleaning
   task body Cleaning is
       Cleaning_Day: Integer;
       Day_Number: Integer := 1;
       Day_Duration: constant Duration := 2.0;
   begin
      -- set on which day the cleaning will be
       accept Start(Cleaning_Day_When: in Integer) do
           Cleaning_Day := Cleaning_Day_When;
           Put_Line(ESC & "[92m" & "Cleaning countdown started" & ESC & "[0m");
       end Start;
       loop
            -- wait for day to end
            delay Day_Duration * Delay_Factor;
            Put_Line (ESC & "[92m" & "Day" & Integer'Image(Day_Number) & ESC & "[0m");
            if Day_Number mod Cleaning_Day = 0 then
               -- Time to clean
               Put_Line (ESC & "[92m" & "Cleaning day" & ESC & "[0m");
               B.Cleaning_Day;
            end if;
            Day_Number := Day_Number + 1;
       end loop;
   end Cleaning;


   --Kitchen--

   task body Kitchen is
      Kitchen_Counter_Capacity : constant Integer := 30;
      type Stat is (Hit, Miss);
      type Kitchen_Counter_type is array (Cooks) of Integer;
      Kitchen_Counter              : Kitchen_Counter_type := (0, 0, 0, 0, 0);
      Meal_Content     : array (Meals, Cooks) of Integer :=
            ((3, 2, 2, 0, 0), (0, 1, 0, 3, 1), (1, 3, 3, 0, 4));
      Max_Meal_Content : array (Cooks) of Integer;
      Meal_Number      : array (Meals) of Integer := (1, 1, 1);
      In_Kitchen_Counter           : Integer := 0;

      -- Stop accepting products when they reach the threshold of the expected demand
      Thresholds : array (Cooks) of Integer;

      -- Stats store counters for hits (successful Take/Deliver) and misses (rejected Take/Deliver)
      type Stat_Counter is delta 0.01 digits 20;
      -- Stats for Take
      Dish_Stats : array(Stat) of Stat_Counter := (0.0, 0.0);
      -- Stats for Deliver
      Meal_Stats: array(Stat) of Stat_Counter := (0.0, 0.0);
      Stat_File: Ada.Text_IO.File_Type;
      
      -- Priority is calculated as the difference between the maximum assembly content and the current storage
      -- We care less about products that can be already assembled
      Priority: array(Cooks) of Integer := (others => 3);
      Lowest_Priority_Cook: Cooks;

     -- Recalculate priority for all producers, find the one with the lowest priority
      procedure RecalculatePriority is
        Lowest_Priority: Integer := 999999;
      begin
        for P in Cooks loop
            Priority(P) := Max_Meal_Content(P) - Kitchen_Counter(P);
            if Priority(P) < Lowest_Priority then
                Lowest_Priority := Priority(P);
                Lowest_Priority_Cook := P;
            end if;
        end loop;
      end RecalculatePriority;

      procedure Setup_Variables is
         Total_Expected_Demand: Integer := 0;
         Multiplier: Float;
         Theoretically_Optimal_Kitchen_Size: Integer;
      begin
         for W in Cooks loop
            Max_Meal_Content (W) := 0;
            for Z in Meals loop
               if Meal_Content (Z, W) > Max_Meal_Content (W) then
                  Max_Meal_Content (W) := Meal_Content (Z, W);
               end if;
            end loop;
         end loop;
      
         declare
            Expected_Demand: array(Cooks) of Integer;
         begin
            for W in Cooks loop
               Expected_Demand(W) := 0;
               for Z in Meals loop
                  Expected_Demand(W) := Expected_Demand(W) + Meal_Content(Z, W);
               end loop;
               Total_Expected_Demand := Total_Expected_Demand + Expected_Demand(W);
            end loop;
      
            --  Determine the theoretically perfect kitchen  size
            Theoretically_Optimal_Kitchen_Size := ((Integer(Float(Total_Expected_Demand) * 2.0) + 9) / 10) * 10;
            if Kitchen_Counter_Capacity < Theoretically_Optimal_Kitchen_Size then
               Put_Line(ESC & "[91m" & "|   " & "Warning: Kitchen size" & Integer'Image(Kitchen_Counter_Capacity) & " is not optimal, should be" & Integer'Image(Theoretically_Optimal_Kitchen_Size) & ESC & "[0m");
            end if;

            -- Set the thresholds based on the multiplier
            Multiplier := (Float(Kitchen_Counter_Capacity) / Float(Total_Expected_Demand)) * 1.5;
            for W in Cooks loop
               Thresholds(W) := Integer(Float(Expected_Demand(W)) * Multiplier);
               Put_Line(ESC & "[91m" & "|   " & "Threshold for Cook" & Integer'Image(W) & ": " & Integer'Image(Thresholds(W)) & ESC & "[0m");
            end loop;

         end;
      end Setup_Variables;

      -- Removes N of each dish from the storage
      procedure Today_Is_Cleaning_Day is
        Cleaning_Day_Takes: constant Integer := 3;
      begin
        for P in Cooks loop
            if Kitchen_Counter(P) >= Cleaning_Day_Takes then
                Put_Line ( ESC & "[92m" & "Cleaning storage for product " & Integer'Image(P) & ESC & "[0m");
                Kitchen_Counter(P) := Kitchen_Counter(P) - Cleaning_Day_Takes;
                In_Kitchen_Counter := In_Kitchen_Counter - Cleaning_Day_Takes;
            end if;
        end loop;
      end;

      function Find_Max_In_Kitchen_Counter return Cooks is
      begin
        return Lowest_Priority_Cook;
      end Find_Max_In_Kitchen_Counter;
      
      procedure RemoveItem(Cook: Cooks) is
      begin
        if Kitchen_Counter(Cook) = 0 then
            return;
        end if;
        Kitchen_Counter(Cook) := Kitchen_Counter(Cook) - 1;
        In_Kitchen_Counter := In_Kitchen_Counter - 1;
      end;

      -- Check if we should cleanup redundant the storage
      function ShouldCleanup return Boolean is
        begin
            return In_Kitchen_Counter >= Kitchen_Counter_Capacity;
        end;

      -- Remove the product with the lowest priority
      function CleanupRedundantKitchen_Counter(Cook: Cooks) return Cooks is
      begin
        RemoveItem(Find_Max_In_Kitchen_Counter);
        RecalculatePriority;
        return Find_Max_In_Kitchen_Counter;
      end;

      function Can_Accept (Dish : Cooks) return Boolean is
      begin
       return In_Kitchen_Counter < Kitchen_Counter_Capacity;
      end Can_Accept;

      function Can_Safely_Accept(Dish: Cooks) return Boolean is
      begin
         if Kitchen_Counter(Dish) < Thresholds(Dish) then
            return True;
         else
            return False;
         end if;
      end Can_Safely_Accept;

      function Can_Deliver (Meal : Meals) return Boolean is
      begin
         for W in Cooks loop
            if Kitchen_Counter (W) < Meal_Content (Meal, W) then
               return False;
            end if;
         end loop;
         return True;
      end Can_Deliver;

      procedure Kitchen_Counter_Contents is
      begin
         for W in Cooks loop
            Put_Line
              ("|   Kitchen_Counter contents: " & Integer'Image (Kitchen_Counter (W)) & " " &
               To_String(Dish_Names (W)));
         end loop;
         Put_Line
           ("|   Number of products in storage: " &
            Integer'Image (In_Kitchen_Counter));
      end Kitchen_Counter_Contents;

   begin
      Create (Stat_File, Ada.Text_IO.Out_File, "stats.txt");
      Close(Stat_File);
      Put_Line (ESC & "[91m" & "B: Kitchen started" & ESC & "[0m");
      Setup_Variables;
      loop
      select
         accept Take (Dish : in Cooks; Number : in Integer) do
            -- check if there is a room for the dish, if there is no room, remove redundant product, don't go inside if we just removed the current dish to avoid filling it up again
            if not (ShouldCleanup and then CleanupRedundantKitchen_Counter(Dish) = Dish) and Can_Accept (Dish) then
               Put_Line
                 (ESC & "[91m" & "B: Accepted dish " &
                  To_String(Dish_Names (Dish)) & " number " &
                  Integer'Image (Number) & ESC & "[0m");
               Kitchen_Counter (Dish) := Kitchen_Counter (Dish) + 1;
               In_Kitchen_Counter        := In_Kitchen_Counter + 1;
               Dish_Stats(Hit) := Dish_Stats(Hit) + 1.0;
               RecalculatePriority;
            else
               Put_Line
                 (ESC & "[91m" & "B: Rejected dish " &
                  To_String(Dish_Names (Dish)) & " number " &
                  Integer'Image (Number) & ESC & "[0m");
                Dish_Stats(Miss) := Dish_Stats(Miss) + 1.0;
            end if;
         end Take;
         Kitchen_Counter_Contents;
        or
         accept Deliver (Meal : in Meals; Number : out Integer) do

            if Can_Deliver (Meal) then
               Put_Line
                 (ESC & "[91m" & "B: Delivered meal " &
                  To_String(Meal_Names (Meal)) & " number " &
                  Integer'Image (Meal_Number (Meal)) & ESC & "[0m");
               for W in Cooks loop
                  Kitchen_Counter (W) := Kitchen_Counter (W) - Meal_Content (Meal, W);
                  In_Kitchen_Counter  := In_Kitchen_Counter - Meal_Content (Meal, W);
               end loop;
               Number                     := Meal_Number (Meal);
               Meal_Number (Meal) := Meal_Number (Meal) + 1;
               Meal_Stats(Hit) := Meal_Stats(Hit) + 1.0;
            else
               Put_Line
                 (ESC & "[91m" & "B: Lacking dishes for meal " &
                  To_String(Meal_Names (Meal)) & ESC & "[0m");
               Number := 0;
                Meal_Stats(Miss) := Meal_Stats(Miss) + 1.0;
            end if;
         end Deliver;
         Kitchen_Counter_Contents;
        or
            accept Cleaning_Day do
                Today_Is_Cleaning_Day;
            end Cleaning_Day;
            Kitchen_Counter_Contents;
        or
            accept Print_Stats do
                Open(Stat_File, Ada.Text_IO.Append_File, "stats.txt");
                Put_Line (Stat_File, "Dish stats:");
                Put_Line (Stat_File, "Hits: " & Stat_Counter'Image(Dish_Stats(Hit)));
                Put_Line (Stat_File, "Misses: " & Stat_Counter'Image(Dish_Stats(Miss)));
                Put_Line (Stat_File, "Ratio: " & Stat_Counter'Image(Dish_Stats(Hit) / Stat_Counter'Max(Dish_Stats(Hit) + Dish_Stats(Miss), 1.00)));
                Put_Line (Stat_File, "Meal stats:");
                Put_Line (Stat_File, "Hits: " & Stat_Counter'Image(Meal_Stats(Hit)));
                Put_Line (Stat_File, "Misses: " & Stat_Counter'Image(Meal_Stats(Miss)));
                Put_Line (Stat_File, "Ratio: " & Stat_Counter'Image(Meal_Stats(Hit) / Stat_Counter'Max(Meal_Stats(Hit) + Meal_Stats(Miss), 1.00)));
                Close(Stat_File);
            end Print_Stats;
         or
            accept Check_Kitchen_Threshold(Dish: in Cooks; Can_Accept: out Boolean) do
               Can_Accept := Can_Safely_Accept(Dish);
            end Check_Kitchen_Threshold;
        end select;
      end loop;
   end Kitchen;

   ---"MAIN" FOR SIMULATION---
begin
   for I in 1 .. Cook_Count loop
      P (I).Start (I, 10);
   end loop;
   for J in 1 .. Customer_Count loop
      K (J).Start (J, 12);
   end loop;
    Cleaning.Start(10);
    loop
        delay 10.0;
        B.Print_Stats;
    end loop;
end Simulator;

